;;; org-confluence-publish.el --- Publish Org files to Confluence Cloud -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Tahir Butt

;; Author: Tahir Butt
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (org "9.0") (request "0.3.0"))
;; Keywords: confluence, org, outlines, wp
;; URL: https://github.com/majorgreys/org-confluence-publish
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides functionality to publish Org mode files to Confluence
;; Cloud pages.  Features:
;;
;; - One-way sync: Org file → Confluence
;; - New pages created as drafts
;; - Resync updates pages as published
;; - Automatic image upload and embedding
;; - Property-based tracking (CONFLUENCE_PAGE_ID, CONFLUENCE_VERSION, CONFLUENCE_URL)
;;
;; Usage:
;;
;; 1. Configure your Confluence credentials:
;;    (setq org-confluence-publish-base-url "https://yoursite.atlassian.net"
;;          org-confluence-publish-email "your-email@example.com"
;;          org-confluence-publish-api-token "your-api-token"
;;          org-confluence-publish-space-key "SPACE"
;;          org-confluence-publish-parent-id "123456")
;;
;; 2. In an Org buffer, run:
;;    M-x org-confluence-publish-buffer
;;
;; 3. To open the published page:
;;    M-x org-confluence-publish-open-page

;;; Code:

(require 'org)
(require 'request)
(require 'json)
(require 'url-util)

;;; Customization

(defgroup org-confluence-publish nil
  "Publish Org files to Confluence Cloud."
  :group 'org
  :prefix "org-confluence-publish-")

(defcustom org-confluence-publish-base-url nil
  "Base URL for Confluence Cloud instance (e.g., https://yoursite.atlassian.net)."
  :type 'string
  :group 'org-confluence-publish)

(defcustom org-confluence-publish-email nil
  "Email address for Confluence Cloud authentication."
  :type 'string
  :group 'org-confluence-publish)

(defcustom org-confluence-publish-api-token nil
  "API token for Confluence Cloud authentication.
Generate at: https://id.atlassian.com/manage-profile/security/api-tokens"
  :type 'string
  :group 'org-confluence-publish)

(defcustom org-confluence-publish-space-key nil
  "Confluence space key where pages will be published."
  :type 'string
  :group 'org-confluence-publish)

(defcustom org-confluence-publish-parent-id nil
  "Optional parent page ID for new pages.
Can be overridden per-file using #+CONFLUENCE_PARENT_ID: property."
  :type '(choice (const :tag "No parent" nil)
                 (string :tag "Parent page ID"))
  :group 'org-confluence-publish)

;;; Internal Variables

(defvar org-confluence-publish--space-id-cache nil
  "Cached space ID to avoid repeated API calls.")

;;; Property Management

(defun org-confluence-publish--get-property (name)
  "Get the value of property NAME from the current buffer."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (format "^#\\+%s: \\(.+\\)$" name) nil t)
      (match-string-no-properties 1))))

(defun org-confluence-publish--set-property (name value)
  "Set property NAME to VALUE in the current buffer.
If property exists, update it. Otherwise, add it after the title."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward (format "^#\\+%s: .+$" name) nil t)
        (replace-match (format "#+%s: %s" name value))
      ;; Insert after #+TITLE: if it exists, otherwise at the beginning
      (goto-char (point-min))
      (if (re-search-forward "^#\\+TITLE: .+$" nil t)
          (progn
            (end-of-line)
            (insert "\n" (format "#+%s: %s" name value)))
        (insert (format "#+%s: %s\n" name value))))))

(defun org-confluence-publish--remove-confluence-properties ()
  "Remove all CONFLUENCE_* properties from current buffer."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^#\\+CONFLUENCE_\\(PAGE_ID\\|VERSION\\|URL\\|PARENT_ID\\):.*\n" nil t)
      (replace-match "")))
  (message "Removed CONFLUENCE_* properties"))

;;; API Layer

(defun org-confluence-publish--auth-header ()
  "Generate Basic Auth header value."
  (let ((credentials (concat org-confluence-publish-email ":" org-confluence-publish-api-token)))
    (concat "Basic " (base64-encode-string credentials t))))

(defun org-confluence-publish--request (method url data callback &optional attempt files)
  "Make HTTP request with retry logic.
METHOD is the HTTP method (GET, POST, PUT).
URL is the endpoint URL.
DATA is the request body (will be JSON-encoded unless FILES is provided).
CALLBACK is called with (success data) on completion.
ATTEMPT is the current retry attempt (internal).
FILES is an alist for multipart/form-data uploads."
  (let ((attempt (or attempt 1))
        (headers `(("Authorization" . ,(org-confluence-publish--auth-header))
                   ("Accept" . "application/json"))))
    (request url
      :type method
      :headers (if files
                   headers
                 (append headers '(("Content-Type" . "application/json"))))
      :data (if files
                nil
              (when data (json-encode data)))
      :files files
      :parser 'json-read
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (funcall callback t data)))
      :error (cl-function
              (lambda (&key response &allow-other-keys)
                (let ((status (request-response-status-code response)))
                  (cond
                   ;; Rate limit or server error: retry with backoff
                   ((and (or (= status 429) (>= status 500))
                         (< attempt 3))
                    (let ((delay (* attempt 2)))
                      (message "Request failed (status %d), retrying in %d seconds (attempt %d/3)..."
                               status delay attempt)
                      (run-at-time delay nil
                                   #'org-confluence-publish--request
                                   method url data callback (1+ attempt) files)))
                   ;; Final failure
                   (t
                    (let ((error-data (request-response-data response)))
                      (funcall callback nil
                               (format "Request failed with status %d: %s"
                                       status
                                       (if error-data
                                           (json-encode error-data)
                                         "No error details")))))))))
      :sync t)))

(defun org-confluence-publish--get-space-id (callback)
  "Get space ID for configured space key, with caching.
Calls CALLBACK with (success space-id-or-error)."
  (if org-confluence-publish--space-id-cache
      (funcall callback t org-confluence-publish--space-id-cache)
    (let ((url (format "%s/wiki/api/v2/spaces?keys=%s"
                       org-confluence-publish-base-url
                       (url-hexify-string org-confluence-publish-space-key))))
      (org-confluence-publish--request
       "GET" url nil
       (lambda (success data)
         (if success
             (let* ((results (cdr (assoc 'results data)))
                    (space-id (when (> (length results) 0)
                                (cdr (assoc 'id (aref results 0))))))
               (if space-id
                   (progn
                     (setq org-confluence-publish--space-id-cache space-id)
                     (funcall callback t space-id))
                 (funcall callback nil "Space not found")))
           (funcall callback nil data)))))))

(defun org-confluence-publish--create-page (title body callback &optional parent-id)
  "Create a new Confluence page as draft.
TITLE is the page title.
BODY is the ADF JSON string.
CALLBACK is called with (success page-data-or-error).
PARENT-ID is optional parent page ID for creating hierarchical pages."
  (org-confluence-publish--get-space-id
   (lambda (success space-id)
     (if (not success)
         (funcall callback nil space-id)
       (let* ((url (format "%s/wiki/api/v2/pages" org-confluence-publish-base-url))
              (payload `((spaceId . ,space-id)
                        (status . "draft")
                        (title . ,title)
                        (body . ((representation . "atlas_doc_format")
                                (value . ,body))))))
         (when (and parent-id (not (string-empty-p parent-id)))
           (setq payload (append payload
                                `((parentId . ,parent-id)))))
         (org-confluence-publish--request
          "POST" url payload callback))))))

(defun org-confluence-publish--get-page-status (page-id callback)
  "Get current status and version of a Confluence page.
PAGE-ID is the page ID.
Calls CALLBACK with (success data-or-error) where data includes status and version."
  (let ((url (format "%s/wiki/api/v2/pages/%s" org-confluence-publish-base-url page-id)))
    (org-confluence-publish--request
     "GET" url nil callback)))

(defun org-confluence-publish--update-page (page-id version title body callback)
  "Update an existing Confluence page as published.
PAGE-ID is the page ID.
VERSION is the current version number.
TITLE is the page title.
BODY is the ADF JSON string.
Calls CALLBACK with (success page-data-or-error)."
  (let* ((url (format "%s/wiki/api/v2/pages/%s" org-confluence-publish-base-url page-id))
         (version-num (string-to-number version))
         ;; For draft pages (version 1), keep version 1 when publishing
         ;; For published pages, increment version
         (next-version (if (= version-num 1) 1 (1+ version-num)))
         (payload `((id . ,page-id)
                   (status . "current")
                   (title . ,title)
                   (body . ((representation . "atlas_doc_format")
                           (value . ,body)))
                   (version . ((number . ,next-version))))))
    (org-confluence-publish--request
     "PUT" url payload callback)))

(defun org-confluence-publish--upload-attachment (page-id file-path callback)
  "Upload an attachment to a Confluence page.
PAGE-ID is the page ID.
FILE-PATH is the local file path.
Calls CALLBACK with (success attachment-data-or-error)."
  (let* ((url (format "%s/wiki/api/v2/pages/%s/attachments"
                      org-confluence-publish-base-url
                      page-id))
         (filename (file-name-nondirectory file-path))
         (files `(("file" . (,filename :file ,file-path)))))
    (org-confluence-publish--request
     "POST" url nil callback nil files)))

(defun org-confluence-publish--delete-page (page-id callback)
  "Delete a Confluence page by moving it to trash.
PAGE-ID is the page ID to delete.
Calls CALLBACK with (success nil-or-error)."
  (let ((url (format "%s/wiki/api/v2/pages/%s"
                     org-confluence-publish-base-url
                     page-id)))
    (org-confluence-publish--request "DELETE" url nil callback)))

;;; ADF Export

(require 'ox-adf)

(defun org-confluence-publish--export-to-adf ()
  "Export current Org buffer to ADF (Atlassian Document Format).
Uses native ox-adf export backend (pure elisp, no external dependencies)."
  (ox-adf-export-as-string))

;;; Image Handling

(defun org-confluence-publish--find-images ()
  "Find all image file links in the current buffer.
Returns a list of absolute file paths."
  (let ((images '()))
    (org-element-map (org-element-parse-buffer) 'link
      (lambda (link)
        (when (string= (org-element-property :type link) "file")
          (let* ((path (org-element-property :path link))
                 (abs-path (expand-file-name path (file-name-directory (buffer-file-name)))))
            (when (and (file-exists-p abs-path)
                       (string-match-p "\\.\\(png\\|jpg\\|jpeg\\|gif\\|svg\\)$" abs-path))
              (push abs-path images))))))
    (delete-dups images)))

(defun org-confluence-publish--upload-images (page-id images callback)
  "Upload all IMAGES to PAGE-ID.
IMAGES is a list of file paths.
Calls CALLBACK with (success uploaded-images) where uploaded-images is an alist
of (file-path . filename)."
  (if (null images)
      (funcall callback t nil)
    (let ((uploaded '())
          (remaining (length images))
          (failed nil))
      (dolist (image images)
        (org-confluence-publish--upload-attachment
         page-id image
         (lambda (success _data)
           (setq remaining (1- remaining))
           (if success
               (push (cons image (file-name-nondirectory image)) uploaded)
             (setq failed t))
           (when (= remaining 0)
             (if failed
                 (funcall callback nil "Failed to upload one or more images")
               (funcall callback t uploaded)))))))))

;;; Validation

(defun org-confluence-publish--validate-config ()
  "Validate that all required configuration is set.
Signals an error if any required variable is nil."
  (let ((missing '()))
    (unless org-confluence-publish-base-url
      (push "org-confluence-publish-base-url" missing))
    (unless org-confluence-publish-email
      (push "org-confluence-publish-email" missing))
    (unless org-confluence-publish-api-token
      (push "org-confluence-publish-api-token" missing))
    (unless org-confluence-publish-space-key
      (push "org-confluence-publish-space-key" missing))
    (when missing
      (user-error "Missing required configuration: %s" (string-join missing ", ")))))

;;; Main Commands

(defun org-confluence-publish--extract-page-info (data)
  "Extract page info from API response DATA.
Returns plist with :version :url :id."
  (let* ((version (number-to-string (cdr (assoc 'number (cdr (assoc 'version data))))))
         (links (cdr (assoc '_links data)))
         (webui (cdr (assoc 'webui links)))
         (url (concat org-confluence-publish-base-url webui))
         (id (cdr (assoc 'id data))))
    (list :version version :url url :id id)))

(defun org-confluence-publish--finalize (page-id images page-url action)
  "Finalize page after ACTION with image uploads.
PAGE-ID is the Confluence page ID.
IMAGES is list of image paths to upload.
PAGE-URL is the final page URL.
ACTION is \"created\" or \"updated\" for messages."
  (if images
      (org-confluence-publish--upload-images
       page-id images
       (lambda (img-success _uploaded)
         ;; Temporarily disable git-auto-commit-mode to prevent conflicts
         (let ((gac-was-enabled (and (boundp 'git-auto-commit-mode) git-auto-commit-mode)))
           (when gac-was-enabled
             (git-auto-commit-mode -1))
           (save-buffer)
           (when gac-was-enabled
             (git-auto-commit-mode 1)))
         (if img-success
             (message "Page %s successfully with %d images: %s" action (length images) page-url)
           (message "Page %s but some images failed to upload: %s" action page-url))))
    ;; Temporarily disable git-auto-commit-mode to prevent conflicts
    (let ((gac-was-enabled (and (boundp 'git-auto-commit-mode) git-auto-commit-mode)))
      (when gac-was-enabled
        (git-auto-commit-mode -1))
      (save-buffer)
      (when gac-was-enabled
        (git-auto-commit-mode 1)))
    (message "Page %s successfully: %s" action page-url)))

;;;###autoload
(defun org-confluence-publish-buffer ()
  "Publish current Org buffer to Confluence.
If CONFLUENCE_PAGE_ID property exists, updates the page as published.
Otherwise, creates a new page as draft."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an Org buffer"))
  (org-confluence-publish--validate-config)

  (let* ((page-id (org-confluence-publish--get-property "CONFLUENCE_PAGE_ID"))
         (version (org-confluence-publish--get-property "CONFLUENCE_VERSION"))
         (title (or (org-confluence-publish--get-property "TITLE")
                   (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))))
         (adf (org-confluence-publish--export-to-adf))
         (images (org-confluence-publish--find-images))
         (parent-id (or (org-confluence-publish--get-property "CONFLUENCE_PARENT_ID")
                        org-confluence-publish-parent-id)))

    (if page-id
        ;; Update existing page - first check if page is trashed
        (progn
          (message "Checking page status...")
          (org-confluence-publish--get-page-status
           page-id
           (lambda (success status-data)
             (if (not success)
                 ;; Check if it's a 404 (page deleted)
                 (if (string-match "status 404" status-data)
                     (error "Page %s does not exist in Confluence (deleted or never existed). Remove CONFLUENCE_PAGE_ID, CONFLUENCE_VERSION, and CONFLUENCE_URL properties to create a new page" page-id)
                   (error "Failed to check page status: %s" status-data))
               (let* ((status (cdr (assoc 'status status-data)))
                      (current-version (number-to-string
                                       (cdr (assoc 'number (cdr (assoc 'version status-data))))))
                      (stored-version version)
                      (live-url (concat org-confluence-publish-base-url
                                       (cdr (assoc 'webui (cdr (assoc '_links status-data))))))
                      (stored-url (org-confluence-publish--get-property "CONFLUENCE_URL")))
                 (cond
                  ((string= status "trashed")
                   (error "Page %s is in trash. Please restore it in Confluence before updating, or remove CONFLUENCE_PAGE_ID property to create a new page" page-id))
                  (t
                   ;; Log version drift if detected
                   (when (and stored-version (not (string= stored-version current-version)))
                     (message "Version sync: local v%s → Confluence v%s" stored-version current-version))
                   ;; Log URL change if detected (e.g., draft was published manually)
                   (when (and stored-url (not (string= stored-url live-url)))
                     (message "URL changed (page was likely published): updating from draft URL to published URL"))
                   ;; Log status for visibility
                   (when (string= status "draft")
                     (message "Publishing draft page (v%s → v%s, draft → published)"
                              current-version current-version))
                   ;; Proceed with update using live version from Confluence
                   (message "Updating page %s (v%s → v%s)..." page-id current-version (1+ (string-to-number current-version)))
                   (org-confluence-publish--update-page
                    page-id current-version title adf
                    (lambda (success data)
                      (if (not success)
                          (error "Failed to update page: %s" data)
                        (let* ((info (org-confluence-publish--extract-page-info data))
                               (new-version (plist-get info :version))
                               (page-url (plist-get info :url)))
                          (org-confluence-publish--set-property "CONFLUENCE_VERSION" new-version)
                          (org-confluence-publish--set-property "CONFLUENCE_URL" page-url)
                          (org-confluence-publish--finalize page-id images page-url "updated")))))))))))

      ;; Create new page
      (message "Creating new page as draft: %s" title)
      (org-confluence-publish--create-page
       title adf
       (lambda (success data)
         (if (not success)
             (error "Failed to create page: %s" data)
           (let* ((info (org-confluence-publish--extract-page-info data))
                  (new-page-id (plist-get info :id))
                  (new-version (plist-get info :version))
                  (page-url (plist-get info :url)))
             (org-confluence-publish--set-property "CONFLUENCE_PAGE_ID" new-page-id)
             (org-confluence-publish--set-property "CONFLUENCE_VERSION" new-version)
             (org-confluence-publish--set-property "CONFLUENCE_URL" page-url)
             (org-confluence-publish--finalize new-page-id images page-url "created"))))
       parent-id))))

;;;###autoload
(defun org-confluence-publish-open-page ()
  "Open the published Confluence page in a browser."
  (interactive)
  (let ((url (org-confluence-publish--get-property "CONFLUENCE_URL")))
    (if url
        (browse-url url)
      (user-error "No CONFLUENCE_URL property found. Publish the page first."))))

;;;###autoload
(defun org-confluence-publish-debug-page ()
  "Fetch current page state from Confluence for debugging."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an Org buffer"))
  (org-confluence-publish--validate-config)
  (let ((page-id (org-confluence-publish--get-property "CONFLUENCE_PAGE_ID")))
    (unless page-id
      (user-error "No CONFLUENCE_PAGE_ID found"))
    (let ((url (format "%s/wiki/api/v2/pages/%s"
                       org-confluence-publish-base-url page-id)))
      (org-confluence-publish--request
       "GET" url nil
       (lambda (success data)
         (if success
             (let* ((version (cdr (assoc 'number (cdr (assoc 'version data)))))
                    (status (cdr (assoc 'status data)))
                    (title (cdr (assoc 'title data))))
               (message "Page status: %s, version: %s, title: %s"
                        status version title)
               (message "Full response: %S" data))
           (message "Failed to fetch page: %s" data))))))))

(defun org-confluence-publish-unpublish ()
  "Unpublish current buffer's page from Confluence.
This deletes the page from Confluence (moves to trash) and removes
all CONFLUENCE_* properties from the org file.

This operation requires confirmation."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an Org buffer"))
  (org-confluence-publish--validate-config)
  (let ((page-id (org-confluence-publish--get-property "CONFLUENCE_PAGE_ID"))
        (title (or (org-confluence-publish--get-property "TITLE") "Untitled")))
    (if (not page-id)
        (error "No page to unpublish: CONFLUENCE_PAGE_ID not found")
      (when (yes-or-no-p (format "Delete page '%s' (ID: %s) from Confluence and remove properties? "
                                 title page-id))
        (org-confluence-publish--delete-page
         page-id
         (lambda (success result)
           (if success
               (progn
                 (org-confluence-publish--remove-confluence-properties)
                 (save-buffer)
                 (message "Page '%s' deleted from Confluence and properties removed" title))
             (error "Failed to delete page: %s" result))))))))

;;; ADF Validation

(defun org-confluence-publish-validate-adf (adf-json)
  "Validate ADF-JSON string against official Atlassian schema.
Uses the adf-validator npm package (https://github.com/torifat/adf-validator).
Returns t if valid, signals error with validation details if invalid.

This function requires Node.js and npx to be installed.
The adf-validator package will be automatically downloaded if not present."
  (let ((temp-file (make-temp-file "adf-validation-" nil ".json")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert adf-json))
          (let* ((cmd (format "npx --yes --quiet adf-validator %s 2>&1"
                             (shell-quote-argument temp-file)))
                 (result (shell-command-to-string cmd)))
            (if (string-match "Successfully validated" result)
                t
              (error "ADF validation failed:\n%s" result))))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(provide 'org-confluence-publish)

;;; org-confluence-publish.el ends here
