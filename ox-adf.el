;;; ox-adf.el --- ADF (Atlassian Document Format) Backend for Org Export -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Tahir Butt

;; Author: Tahir Butt
;; Keywords: org, confluence, adf
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (org "9.0"))
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

;; This library implements an Atlassian Document Format (ADF) backend
;; for Org exporter.  ADF is a JSON-based document format used by
;; Confluence Cloud for page content.
;;
;; Design:
;; - Each transcoder returns a JSON string with a trailing newline
;; - The template function parses all fragments and consolidates them
;; - Text nodes with identical marks are merged for efficiency
;;
;; Usage:
;;   (require 'ox-adf)
;;   (ox-adf-export-as-string)  ; Returns ADF JSON string
;;
;; See: https://developer.atlassian.com/cloud/jira/platform/apis/document/structure/

;;; Code:

(require 'ox)
(require 'json)
(require 'cl-lib)

;;; Backend Definition

(org-export-define-backend 'adf
  '((bold . ox-adf-bold)
    (code . ox-adf-code)
    (headline . ox-adf-headline)
    (horizontal-rule . ox-adf-horizontal-rule)
    (italic . ox-adf-italic)
    (item . ox-adf-item)
    (line-break . ox-adf-line-break)
    (link . ox-adf-link)
    (paragraph . ox-adf-paragraph)
    (plain-list . ox-adf-plain-list)
    (plain-text . ox-adf-plain-text)
    (quote-block . ox-adf-quote-block)
    (section . ox-adf-section)
    (src-block . ox-adf-src-block)
    (strike-through . ox-adf-strike-through)
    (subscript . ox-adf-subscript)
    (superscript . ox-adf-superscript)
    (table . ox-adf-table)
    (table-cell . ox-adf-table-cell)
    (table-row . ox-adf-table-row)
    (template . ox-adf-template)
    (underline . ox-adf-underline)
    (verbatim . ox-adf-verbatim))
  :menu-entry
  '(?a "Export to ADF"
       ((?A "As ADF buffer" ox-adf-export-as-adf)
        (?a "To temporary buffer" ox-adf-export-to-buffer))))

;;; Helper Functions

(defun ox-adf--strip-checkboxes (text)
  "Strip checkbox unicode characters from TEXT.
Org TODO items get converted to checkbox characters which should
not appear in the final output."
  (let ((result text))
    ;; Strip checkbox characters (☐ ☑ ☒) with optional following whitespace
    (setq result (replace-regexp-in-string "^☐\\s-*" "" result))
    (setq result (replace-regexp-in-string "^☑\\s-*" "" result))
    (setq result (replace-regexp-in-string "^☒\\s-*" "" result))
    result))

(defun ox-adf--marks-equal (marks1 marks2)
  "Return t if MARKS1 and MARKS2 are equal mark arrays.
Compares both type and attrs of each mark."
  (and (= (length marks1) (length marks2))
       (cl-every (lambda (pair)
                   (let ((m1 (car pair))
                         (m2 (cdr pair)))
                     (and (equal (alist-get 'type m1)
                                 (alist-get 'type m2))
                          (equal (alist-get 'attrs m1)
                                 (alist-get 'attrs m2)))))
                 (cl-mapcar #'cons marks1 marks2))))

(defun ox-adf--consolidate-text-nodes (nodes)
  "Merge consecutive text nodes with identical marks in NODES.

Algorithm: Accumulate text while marks match, flush when marks differ
or non-text node encountered.  This reduces ADF size and improves
rendering in Confluence.

Returns a new list (not in-place modification)."
  (let (result current-text current-marks)
    (dolist (node nodes)
      (let ((node-type (alist-get 'type node)))
        (if (not (equal node-type "text"))
            ;; Non-text node: flush accumulated text, add node
            (progn
              (when current-text
                (push `((type . "text")
                        (text . ,current-text)
                        ,@(when current-marks `((marks . ,current-marks))))
                      result)
                (setq current-text nil
                      current-marks nil))
              (push node result))
          ;; Text node: try to merge
          (let ((text (alist-get 'text node))
                (marks (alist-get 'marks node)))
            (if (and current-text (ox-adf--marks-equal marks current-marks))
                ;; Same marks: accumulate text
                (setq current-text (concat current-text text))
              ;; Different marks: flush and start new
              (when current-text
                (push `((type . "text")
                        (text . ,current-text)
                        ,@(when current-marks `((marks . ,current-marks))))
                      result))
              (setq current-text text
                    current-marks marks))))))
    ;; Flush remaining
    (when current-text
      (push `((type . "text")
              (text . ,current-text)
              ,@(when current-marks `((marks . ,current-marks))))
            result))
    (nreverse result)))

(defun ox-adf--sort-keys (obj)
  "Recursively sort alist OBJ keys in ADF preferred order.
ADF prefers keys in order: version, type, attrs, content, text, marks."
  (if (not (listp obj))
      obj
    (let* ((key-order '((version . 1) (type . 2) (attrs . 3)
                        (content . 4) (text . 5) (marks . 6)))
           (sorted (sort (copy-sequence obj)
                         (lambda (a b)
                           (let ((pos-a (or (alist-get (car a) key-order) 99))
                                 (pos-b (or (alist-get (car b) key-order) 99)))
                             (< pos-a pos-b))))))
      (mapcar (lambda (pair)
                (cons (car pair)
                      (if (listp (cdr pair))
                          (if (and (consp (cdr pair))
                                   (not (consp (cadr pair))))
                              ;; Don't recurse on (key . value) pairs
                              (cdr pair)
                            ;; Recurse on lists of alists
                            (if (and (listp (cdr pair))
                                     (listp (cadr pair))
                                     (consp (caadr pair)))
                                (mapcar #'ox-adf--sort-keys (cdr pair))
                              (cdr pair)))
                        (cdr pair))))
              sorted))))

(defun ox-adf--encode-json (obj)
  "Encode elisp OBJ to JSON with ADF-specific formatting.
Ensures proper key ordering and handles empty values."
  (let ((json-object-type 'alist)
        (json-array-type 'list)
        (json-false :json-false)
        (json-encoding-pretty-print t))
    (json-encode (ox-adf--sort-keys obj))))

(defun ox-adf--parse-json-fragment (str)
  "Parse JSON string STR into elisp alist.

Error handling: Returns nil (not error) for empty strings or invalid JSON.
This allows callers to use `delq nil' to filter out parse failures."
  (when (and str (not (string-empty-p str)))
    (condition-case nil
        (let ((json-object-type 'alist)
              (json-array-type 'list))
          (json-read-from-string str))
      (error nil))))

(defun ox-adf--parse-json-contents (contents)
  "Parse newline-separated JSON CONTENTS into list of nodes.
Returns nil if CONTENTS is empty or all fragments are invalid."
  (when (and contents (not (string-empty-p (string-trim contents))))
    (let ((fragments (split-string contents "\n" t)))
      (delq nil (mapcar (lambda (frag)
                          (ox-adf--parse-json-fragment (string-trim frag)))
                        fragments)))))

(defun ox-adf--json-line (obj)
  "Encode OBJ to JSON and append newline.
Standard output format for transcoders."
  (concat (json-encode obj) "\n"))

(defun ox-adf--make-text-node (text &optional marks)
  "Create ADF text node with TEXT and optional MARKS.
MARKS should be a list of mark alists.
Strips trailing newlines from text to prevent extra spacing in rendered output."
  (let ((clean-text (replace-regexp-in-string "\n+\\'" "" text)))
    `((type . "text")
      (text . ,clean-text)
      ,@(when marks `((marks . ,(vconcat marks)))))))

(defun ox-adf--add-mark-to-node (node mark)
  "Add MARK to NODE's marks array.
Returns modified node."
  (if (equal (alist-get 'type node) "text")
      (let ((existing-marks (or (alist-get 'marks node) '())))
        `((type . "text")
          (text . ,(alist-get 'text node))
          (marks . ,(append existing-marks (list mark)))))
    node))

(defun ox-adf--add-mark-to-json (contents mark)
  "Parse CONTENTS and add MARK to all text nodes.

CONTENTS is newline-separated JSON strings (transcoder output format).
MARK is an alist like ((type . \"strong\")) or ((type . \"link\") (attrs ...)).

Returns newline-separated JSON objects with trailing newline.
The trailing newline is critical: org-export appends post-blank spaces
after transcoder output, and the newline ensures those spaces don't
corrupt the JSON."
  (when (and contents (not (string-empty-p (string-trim contents))))
    ;; Split by newlines, parse each fragment, add marks, re-encode
    (let* ((fragments (split-string contents "\n" t))
           (result-parts
            (mapcar
             (lambda (frag)
               (let* ((trimmed (string-trim frag))
                      (parsed (ox-adf--parse-json-fragment trimmed)))
                 (when parsed
                   (if (and (listp parsed) (not (consp (car parsed))))
                       ;; It's an array - map over elements
                       (mapconcat
                        (lambda (n) (json-encode (ox-adf--add-mark-to-node n mark)))
                        parsed "\n")
                     ;; Single object
                     (json-encode (ox-adf--add-mark-to-node parsed mark))))))
             fragments)))
      ;; Add trailing newline so post-blank spaces don't break JSON
      (concat (mapconcat #'identity (delq nil result-parts) "\n") "\n"))))

;;; Transcoder Functions

(defun ox-adf-template (contents _info)
  "Return complete ADF document.
CONTENTS is the transcoded elements. INFO is communication channel."
  (let* ((nodes (ox-adf--parse-json-contents contents))
         (consolidated (if nodes (ox-adf--consolidate-text-nodes nodes) '()))
         (doc `((version . 1)
                (type . "doc")
                (content . ,(vconcat consolidated)))))
    (ox-adf--encode-json doc)))

(defun ox-adf-section (_section contents _info)
  "Transcode SECTION element.
CONTENTS is the section contents. INFO is communication channel."
  ;; Just pass through contents - template will parse and consolidate
  (or contents ""))

(defun ox-adf-paragraph (paragraph contents _info)
  "Transcode PARAGRAPH element to ADF.
CONTENTS is the paragraph contents. INFO is communication channel."
  ;; Filter whitespace-only children for standalone image detection
  (let* ((children (org-element-contents paragraph))
         (significant-children (cl-remove-if
                               (lambda (c) (and (stringp c) (string-blank-p c)))
                               children)))
    (if (and (= 1 (length significant-children))
             (eq 'link (org-element-type (car significant-children)))
             (ox-adf--image-link-p (car significant-children)))
        ;; Standalone image -> mediaSingle
        (let* ((link (car significant-children))
               (path (org-element-property :path link))
               (filename (file-name-nondirectory path)))
          (ox-adf--json-line `((type . "mediaSingle")
                               (attrs . ((layout . "center")))
                               (content . [((type . "media")
                                           (attrs . ((type . "file")
                                                     (id . ,filename)
                                                     (collection . ""))))]))))
      ;; Normal paragraph
      (let ((content-nodes (ox-adf--parse-json-contents contents)))
        (when content-nodes
          (ox-adf--json-line `((type . "paragraph")
                               (content . ,(vconcat content-nodes)))))))))

(defun ox-adf--image-link-p (link)
  "Return t if LINK is an image link."
  (when (eq 'link (org-element-type link))
    (let ((type (org-element-property :type link))
          (path (org-element-property :path link)))
      (and (equal type "file")
           (string-match-p "\\.\\(png\\|jpe?g\\|gif\\|svg\\)\\'" path)))))

(defun ox-adf-plain-text (text info)
  "Transcode TEXT to ADF text node.
INFO is communication channel with :adf-marks."
  (let* ((marks (plist-get info :adf-marks))
         (clean-text (ox-adf--strip-checkboxes text)))
    (ox-adf--json-line (ox-adf--make-text-node clean-text (when marks (reverse marks))))))

(defun ox-adf-bold (_bold contents _info)
  "Transcode BOLD object to ADF.
CONTENTS has the formatted contents."
  (ox-adf--add-mark-to-json contents '((type . "strong"))))

(defun ox-adf-italic (_italic contents _info)
  "Transcode ITALIC object to ADF.
CONTENTS has the formatted contents."
  (ox-adf--add-mark-to-json contents '((type . "em"))))

(defun ox-adf-code (code _contents _info)
  "Transcode CODE object to ADF.
CODE element has :value property with the code text."
  (let ((value (org-element-property :value code)))
    (ox-adf--json-line (ox-adf--make-text-node value '(((type . "code")))))))

(defun ox-adf-verbatim (verbatim _contents _info)
  "Transcode VERBATIM object to ADF.
VERBATIM element has :value property with the verbatim text."
  (let ((value (org-element-property :value verbatim)))
    (ox-adf--json-line (ox-adf--make-text-node value '(((type . "code")))))))

(defun ox-adf-strike-through (_strike-through contents _info)
  "Transcode STRIKE-THROUGH object to ADF.
CONTENTS has the formatted contents."
  (ox-adf--add-mark-to-json contents '((type . "strike"))))

(defun ox-adf-underline (_underline contents _info)
  "Transcode UNDERLINE object to ADF.
CONTENTS has the formatted contents."
  (ox-adf--add-mark-to-json contents '((type . "underline"))))

(defun ox-adf-subscript (_subscript contents _info)
  "Transcode SUBSCRIPT object to ADF.
CONTENTS has the formatted contents."
  (ox-adf--add-mark-to-json contents '((type . "subsup")
                                       (attrs . ((type . "sub"))))))

(defun ox-adf-superscript (_superscript contents _info)
  "Transcode SUPERSCRIPT object to ADF.
CONTENTS has the formatted contents."
  (ox-adf--add-mark-to-json contents '((type . "subsup")
                                       (attrs . ((type . "sup"))))))

(defun ox-adf-line-break (_line-break _contents _info)
  "Transcode LINE-BREAK object to ADF."
  (ox-adf--json-line '((type . "hardBreak"))))

(defun ox-adf-headline (headline contents _info)
  "Transcode HEADLINE element to ADF.
CONTENTS is the headline contents (section). INFO is communication channel."
  (let* ((level (org-element-property :level headline))
         (title (org-element-property :raw-value headline))
         (text-node (ox-adf--make-text-node title))
         (heading-json (ox-adf--json-line `((type . "heading")
                                            (attrs . ((level . ,level)))
                                            (content . [,text-node])))))
    (if contents
        (concat heading-json contents)
      heading-json)))

(defun ox-adf-src-block (src-block _contents _info)
  "Transcode SRC-BLOCK element to ADF.
CONTENTS is nil. INFO is communication channel."
  (let* ((lang (org-element-property :language src-block))
         (value (org-element-property :value src-block))
         (normalized-lang (cond
                          ((member lang '("emacs-lisp" "elisp")) "lisp")
                          ((equal lang "shell") "bash")
                          (t (or lang "text"))))
         (text-node (ox-adf--make-text-node value)))
    (ox-adf--json-line `((type . "codeBlock")
                         (attrs . ((language . ,normalized-lang)))
                         (content . [,text-node])))))

(defun ox-adf-quote-block (_quote-block contents _info)
  "Transcode QUOTE-BLOCK element to ADF.
CONTENTS is the quote contents. INFO is communication channel."
  (let ((content-nodes (ox-adf--parse-json-contents contents)))
    (when content-nodes
      (ox-adf--json-line `((type . "blockquote")
                           (content . ,(vconcat content-nodes)))))))

(defun ox-adf-horizontal-rule (_horizontal-rule _contents _info)
  "Transcode HORIZONTAL-RULE element to ADF."
  (ox-adf--json-line '((type . "rule"))))

(defun ox-adf-link (link desc info)
  "Transcode LINK object to ADF.
DESC is the link description. INFO is communication channel."
  (let ((type (org-element-property :type link))
        (path (org-element-property :path link)))
    (cond
     ;; Image links
     ((ox-adf--image-link-p link)
      (let ((parent (org-element-property :parent link)))
        (if (and (eq 'paragraph (org-element-type parent))
                 (= 1 (length (org-element-contents parent))))
            ;; Standalone - handled by paragraph transcoder
            ""
          ;; Inline image
          (let ((filename (file-name-nondirectory path)))
            (ox-adf--json-line `((type . "mediaInline")
                                 (attrs . ((type . "file")
                                           (id . ,filename)
                                           (collection . "")))))))))
     ;; URL links
     ((member type '("http" "https"))
      (let ((url (concat type ":" path)))
        (if (plist-get info :adf-in-link)
            desc  ; Nested link: flatten
          (ox-adf--add-mark-to-json desc `((type . "link")
                                           (attrs . ((href . ,url))))))))
     ;; Other link types
     (t desc))))

(defun ox-adf-plain-list (plain-list contents _info)
  "Transcode PLAIN-LIST element to ADF.
CONTENTS is the list contents. INFO is communication channel."
  (let ((list-type (org-element-property :type plain-list))
        (items (ox-adf--parse-json-contents contents)))
    (when items
      (ox-adf--json-line
       (pcase list-type
         ('ordered `((type . "orderedList")
                     (attrs . ((order . 1)))
                     (content . ,(vconcat items))))
         ('unordered `((type . "bulletList")
                       (content . ,(vconcat items))))
         ;; Definition lists -> bullet lists (ADF lacks native support)
         ('descriptive `((type . "bulletList")
                         (content . ,(vconcat items)))))))))

(defun ox-adf-item (item contents _info)
  "Transcode ITEM element to ADF.
CONTENTS is the item contents. INFO is communication channel."
  (let* ((tag (org-element-property :tag item))
         (term-text (when tag (org-element-interpret-data tag)))
         (content-nodes (ox-adf--parse-json-contents contents)))
    (when (or content-nodes term-text)
      (let ((final-content
             (if term-text
                 ;; Definition list: term paragraph + definition content
                 (vconcat
                  (list `((type . "paragraph")
                          (content . [,(ox-adf--make-text-node term-text '(((type . "strong"))))])))
                  content-nodes)
               (vconcat content-nodes))))
        (ox-adf--json-line `((type . "listItem")
                             (content . ,final-content)))))))

(defun ox-adf-table (table _contents info)
  "Transcode TABLE element to ADF.
CONTENTS is nil. INFO is communication channel."
  (let* ((rows (org-element-map table 'table-row
                 (lambda (row)
                   (unless (eq 'rule (org-element-property :type row))
                     row))
                 info))
         (rule-pos (cl-position-if
                    (lambda (row)
                      (eq 'rule (org-element-property :type row)))
                    (org-element-map table 'table-row #'identity info)))
         (header-rows (if rule-pos rule-pos 0))
         (row-nodes (cl-loop for row in rows
                            for idx from 0
                            collect (ox-adf--transcode-table-row row (< idx header-rows) info))))
    (ox-adf--json-line `((type . "table")
                         (attrs . ((isNumberColumnEnabled . :json-false)
                                  (layout . "default")))
                         (content . ,(vconcat row-nodes))))))

(defun ox-adf--transcode-table-row (row is-header info)
  "Transcode table ROW to ADF.
IS-HEADER indicates if this is a header row. INFO is communication channel."
  (let ((cells (org-element-map row 'table-cell #'identity info)))
    `((type . "tableRow")
      (content . ,(vconcat
                  (mapcar (lambda (cell)
                           (ox-adf--transcode-table-cell cell is-header info))
                         cells))))))

(defun ox-adf--wrap-inline-nodes-in-paragraph (nodes)
  "Wrap inline-level NODES in paragraph nodes if needed.
ADF table cells must contain block-level nodes (paragraph, etc), not bare inline nodes.
NODES can be a vector or list."
  (let ((nodes-vec (if (vectorp nodes) nodes (vconcat nodes))))
    (if (zerop (length nodes-vec))
        ;; Empty: return paragraph with space (ADF requires minLength 1)
        '[((type . "paragraph")
           (content . [((type . "text") (text . " "))]))]
      ;; Check if nodes are block-level or inline-level
      (let ((block-types '("paragraph" "heading" "bulletList" "orderedList"
                          "codeBlock" "panel" "blockquote" "table"))
            (first-node-type (alist-get 'type (aref nodes-vec 0))))
        (if (member first-node-type block-types)
            ;; Already block-level
            nodes-vec
          ;; Inline-level: wrap in paragraph
          `[((type . "paragraph")
             (content . ,nodes-vec))])))))

(defun ox-adf--transcode-table-cell (cell is-header info)
  "Transcode table CELL to ADF.
IS-HEADER indicates if cell is in header. INFO is communication channel."
  (let* ((cell-type (if is-header "tableHeader" "tableCell"))
         (contents (org-export-data (org-element-contents cell) info))
         (content-str (string-trim contents)))
    `((type . ,cell-type)
      (content . ,(vconcat
                  (if (string-empty-p content-str)
                      ;; Empty cell: paragraph with space (ADF requires minLength 1)
                      '[((type . "paragraph")
                         (content . [((type . "text") (text . " "))]))]
                    ;; Parse and wrap nodes from cell contents
                    (let ((nodes (ox-adf--parse-json-fragment
                                  (if (string-prefix-p "[" content-str)
                                      content-str
                                    (concat "[" content-str "]")))))
                      (ox-adf--wrap-inline-nodes-in-paragraph nodes))))))))

(defun ox-adf-table-row (_table-row _contents _info)
  "Transcode TABLE-ROW element to ADF.
Required stub for org-export backend registration.
Actual transcoding handled by `ox-adf-table' via direct traversal."
  "")

(defun ox-adf-table-cell (_table-cell _contents _info)
  "Transcode TABLE-CELL element to ADF.
Required stub for org-export backend registration.
Actual transcoding handled by `ox-adf--transcode-table-cell' via direct traversal."
  "")

;;; Public API

;;;###autoload
(defun ox-adf-export-as-string ()
  "Export current Org buffer to ADF JSON string.
Returns the ADF JSON as a string."
  (let ((org-export-use-babel nil))
    (org-export-as 'adf nil nil nil)))

;;;###autoload
(defun ox-adf-export-as-adf (&optional async subtreep visible-only)
  "Export current buffer to an ADF buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Export is done in a buffer named \"*Org ADF Export*\"."
  (interactive)
  (org-export-to-buffer 'adf "*Org ADF Export*"
    async subtreep visible-only nil nil (lambda () (text-mode))))

;;;###autoload
(defun ox-adf-export-to-buffer (&optional async subtreep visible-only body-only)
  "Export current buffer to an ADF buffer.
Same as `ox-adf-export-as-adf' but with simpler interface.
BODY-ONLY is ignored for compatibility with org-export-dispatch."
  (interactive)
  (ox-adf-export-as-adf async subtreep visible-only))

(provide 'ox-adf)
;;; ox-adf.el ends here
