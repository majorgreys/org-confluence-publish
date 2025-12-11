;;; org-confluence-publish-test.el --- Tests for org-confluence-publish -*- lexical-binding: t; -*-

;; Initialize packages to make dependencies available
(require 'package)
(package-initialize)

(require 'buttercup)
(require 'org-confluence-publish)
(require 'json)

(describe "org-confluence-publish--export-to-adf"

  (describe "headings"
    (it "converts org headings to ADF heading nodes"
      (with-temp-buffer
        (insert "* Level 1 Heading\n\n** Level 2 Heading\n\n*** Level 3 Heading\n")
        (org-mode)
        (let* ((adf-json (org-confluence-publish--export-to-adf))
               (adf (json-read-from-string adf-json))
               (content (cdr (assoc 'content adf))))
          (expect (length content) :to-equal 3)
          (expect (cdr (assoc 'type (aref content 0))) :to-equal "heading")
          (expect (cdr (assoc 'level (cdr (assoc 'attrs (aref content 0))))) :to-equal 1)
          (expect (cdr (assoc 'level (cdr (assoc 'attrs (aref content 1))))) :to-equal 2)
          (expect (cdr (assoc 'level (cdr (assoc 'attrs (aref content 2))))) :to-equal 3)))))

  (describe "text marks"
    (it "converts bold to strong mark"
      (with-temp-buffer
        (insert "*Bold* text\n")
        (org-mode)
        (let* ((adf-json (org-confluence-publish--export-to-adf))
               (adf (json-read-from-string adf-json))
               (content (cdr (assoc 'content adf)))
               (para (aref content 0))
               (para-content (cdr (assoc 'content para)))
               (bold-text (aref para-content 0)))
          (expect (cdr (assoc 'text bold-text)) :to-equal "Bold")
          (let ((marks (cdr (assoc 'marks bold-text))))
            (expect (length marks) :to-equal 1)
            (expect (cdr (assoc 'type (aref marks 0))) :to-equal "strong")))))

    (it "converts italic to em mark"
      (with-temp-buffer
        (insert "/Italic/ text\n")
        (org-mode)
        (let* ((adf-json (org-confluence-publish--export-to-adf))
               (adf (json-read-from-string adf-json))
               (content (cdr (assoc 'content adf)))
               (para (aref content 0))
               (para-content (cdr (assoc 'content para)))
               (italic-text (aref para-content 0)))
          (expect (cdr (assoc 'text italic-text)) :to-equal "Italic")
          (let ((marks (cdr (assoc 'marks italic-text))))
            (expect (length marks) :to-equal 1)
            (expect (cdr (assoc 'type (aref marks 0))) :to-equal "em")))))

    (it "converts code to code mark"
      (with-temp-buffer
        (insert "=code= text\n")
        (org-mode)
        (let* ((adf-json (org-confluence-publish--export-to-adf))
               (adf (json-read-from-string adf-json))
               (content (cdr (assoc 'content adf)))
               (para (aref content 0))
               (para-content (cdr (assoc 'content para)))
               (code-text (aref para-content 0)))
          (expect (cdr (assoc 'text code-text)) :to-equal "code")
          (let ((marks (cdr (assoc 'marks code-text))))
            (expect (length marks) :to-equal 1)
            (expect (cdr (assoc 'type (aref marks 0))) :to-equal "code")))))

    (it "converts strikethrough to strike mark"
      (with-temp-buffer
        (insert "+strikethrough+ text\n")
        (org-mode)
        (let* ((adf-json (org-confluence-publish--export-to-adf))
               (adf (json-read-from-string adf-json))
               (content (cdr (assoc 'content adf)))
               (para (aref content 0))
               (para-content (cdr (assoc 'content para)))
               (strike-text (aref para-content 0)))
          (expect (cdr (assoc 'text strike-text)) :to-equal "strikethrough")
          (let ((marks (cdr (assoc 'marks strike-text))))
            (expect (length marks) :to-equal 1)
            (expect (cdr (assoc 'type (aref marks 0))) :to-equal "strike")))))

    (it "converts underline to underline mark"
      (with-temp-buffer
        (insert "_underline_ text\n")
        (org-mode)
        (let* ((adf-json (org-confluence-publish--export-to-adf))
               (adf (json-read-from-string adf-json))
               (content (cdr (assoc 'content adf)))
               (para (aref content 0))
               (para-content (cdr (assoc 'content para)))
               (underline-text (aref para-content 0)))
          (expect (cdr (assoc 'text underline-text)) :to-equal "underline")
          (let ((marks (cdr (assoc 'marks underline-text))))
            (expect (length marks) :to-equal 1)
            (expect (cdr (assoc 'type (aref marks 0))) :to-equal "underline"))))))

  (describe "subscript and superscript"
    (it "converts subscript to subsup mark with type=sub"
      (with-temp-buffer
        (insert "H_{2}O\n")
        (org-mode)
        (let* ((adf-json (org-confluence-publish--export-to-adf))
               (adf (json-read-from-string adf-json))
               (content (cdr (assoc 'content adf)))
               (para (aref content 0))
               (para-content (cdr (assoc 'content para)))
               (sub-text (aref para-content 1)))
          (expect (cdr (assoc 'text sub-text)) :to-equal "2")
          (let* ((marks (cdr (assoc 'marks sub-text)))
                 (mark (aref marks 0)))
            (expect (cdr (assoc 'type mark)) :to-equal "subsup")
            (expect (cdr (assoc 'type (cdr (assoc 'attrs mark)))) :to-equal "sub")))))

    (it "converts superscript to subsup mark with type=sup"
      (with-temp-buffer
        (insert "E=mc^{2}\n")
        (org-mode)
        (let* ((adf-json (org-confluence-publish--export-to-adf))
               (adf (json-read-from-string adf-json))
               (content (cdr (assoc 'content adf)))
               (para (aref content 0))
               (para-content (cdr (assoc 'content para)))
               (sup-text (aref para-content 1)))
          (expect (cdr (assoc 'text sup-text)) :to-equal "2")
          (let* ((marks (cdr (assoc 'marks sup-text)))
                 (mark (aref marks 0)))
            (expect (cdr (assoc 'type mark)) :to-equal "subsup")
            (expect (cdr (assoc 'type (cdr (assoc 'attrs mark)))) :to-equal "sup"))))))

  (describe "lists"
    (it "converts bullet lists to bulletList nodes"
      (with-temp-buffer
        (insert "- Item 1\n- Item 2\n- Item 3\n")
        (org-mode)
        (let* ((adf-json (org-confluence-publish--export-to-adf))
               (adf (json-read-from-string adf-json))
               (content (cdr (assoc 'content adf)))
               (list-node (aref content 0)))
          (expect (cdr (assoc 'type list-node)) :to-equal "bulletList")
          (expect (length (cdr (assoc 'content list-node))) :to-equal 3))))

    (it "converts ordered lists to orderedList nodes"
      (with-temp-buffer
        (insert "1. First\n2. Second\n3. Third\n")
        (org-mode)
        (let* ((adf-json (org-confluence-publish--export-to-adf))
               (adf (json-read-from-string adf-json))
               (content (cdr (assoc 'content adf)))
               (list-node (aref content 0)))
          (expect (cdr (assoc 'type list-node)) :to-equal "orderedList")
          (expect (length (cdr (assoc 'content list-node))) :to-equal 3)
          (expect (cdr (assoc 'order (cdr (assoc 'attrs list-node)))) :to-equal 1))))

    (it "converts definition lists to bulletList nodes"
      (with-temp-buffer
        (insert "- Term :: Definition\n")
        (org-mode)
        (let* ((adf-json (org-confluence-publish--export-to-adf))
               (adf (json-read-from-string adf-json))
               (content (cdr (assoc 'content adf)))
               (list-node (aref content 0)))
          (expect (cdr (assoc 'type list-node)) :to-equal "bulletList")
          ;; Should have 1 item with term (bold) + definition inside
          (expect (length (cdr (assoc 'content list-node))) :to-equal 1)))))

  (describe "code blocks"
    (it "converts src blocks to codeBlock nodes"
      (with-temp-buffer
        (insert "#+BEGIN_SRC python\nprint('hello')\n#+END_SRC\n")
        (org-mode)
        (let* ((adf-json (org-confluence-publish--export-to-adf))
               (adf (json-read-from-string adf-json))
               (content (cdr (assoc 'content adf)))
               (code-node (aref content 0)))
          (expect (cdr (assoc 'type code-node)) :to-equal "codeBlock")
          (expect (cdr (assoc 'language (cdr (assoc 'attrs code-node)))) :to-equal "python")
          (expect (cdr (assoc 'text (aref (cdr (assoc 'content code-node)) 0)))
                  :to-match "print('hello')")))))

  (describe "tables"
    (it "converts org tables to table nodes"
      (with-temp-buffer
        (insert "| H1 | H2 |\n|----+----|\n| C1 | C2 |\n")
        (org-mode)
        (let* ((adf-json (org-confluence-publish--export-to-adf))
               (adf (json-read-from-string adf-json))
               (content (cdr (assoc 'content adf)))
               (table-node (aref content 0)))
          (expect (cdr (assoc 'type table-node)) :to-equal "table")
          (let* ((rows (cdr (assoc 'content table-node)))
                 (header-row (aref rows 0))
                 (header-cells (cdr (assoc 'content header-row))))
            (expect (cdr (assoc 'type (aref header-cells 0))) :to-equal "tableHeader")
            (expect (length rows) :to-equal 2)))))

    (it "does not include empty attrs in table cells"
      (with-temp-buffer
        (insert "| H1 | H2 |\n|----+----|\n| C1 | C2 |\n")
        (org-mode)
        (let* ((adf-json (org-confluence-publish--export-to-adf))
               (adf (json-read-from-string adf-json))
               (content (cdr (assoc 'content adf)))
               (table-node (aref content 0))
               (rows (cdr (assoc 'content table-node)))
               (header-row (aref rows 0))
               (header-cell (aref (cdr (assoc 'content header-row)) 0)))
          ;; attrs should not exist if empty
          (expect (assoc 'attrs header-cell) :to-equal nil)))))

  (describe "blockquotes"
    (it "converts quote blocks to blockquote nodes"
      (with-temp-buffer
        (insert "#+BEGIN_QUOTE\nQuoted text\n#+END_QUOTE\n")
        (org-mode)
        (let* ((adf-json (org-confluence-publish--export-to-adf))
               (adf (json-read-from-string adf-json))
               (content (cdr (assoc 'content adf)))
               (quote-node (aref content 0)))
          (expect (cdr (assoc 'type quote-node)) :to-equal "blockquote")))))

  (describe "images"
    (it "converts standalone images to mediaSingle nodes"
      (with-temp-buffer
        (insert "[[file:test-image.png]]\n")
        (org-mode)
        (let* ((adf-json (org-confluence-publish--export-to-adf))
               (adf (json-read-from-string adf-json))
               (content (cdr (assoc 'content adf)))
               (media-node (aref content 0)))
          (expect (cdr (assoc 'type media-node)) :to-equal "mediaSingle")
          (let* ((media-content (cdr (assoc 'content media-node)))
                 (media (aref media-content 0)))
            (expect (cdr (assoc 'type media)) :to-equal "media")
            (expect (cdr (assoc 'id (cdr (assoc 'attrs media)))) :to-equal "test-image.png")))))

    (it "converts inline images to mediaInline nodes"
      (with-temp-buffer
        (insert "Text with [[file:icon.png]] image.\n")
        (org-mode)
        (let* ((adf-json (org-confluence-publish--export-to-adf))
               (adf (json-read-from-string adf-json))
               (content (cdr (assoc 'content adf)))
               (para (aref content 0))
               (para-content (cdr (assoc 'content para)))
               (media-node (aref para-content 1)))
          (expect (cdr (assoc 'type media-node)) :to-equal "mediaInline")
          (expect (cdr (assoc 'id (cdr (assoc 'attrs media-node)))) :to-equal "icon.png")))))

  (describe "links"
    (it "converts links to link marks"
      (with-temp-buffer
        (insert "[[https://example.com][Link text]]\n")
        (org-mode)
        (let* ((adf-json (org-confluence-publish--export-to-adf))
               (adf (json-read-from-string adf-json))
               (content (cdr (assoc 'content adf)))
               (para (aref content 0))
               (para-content (cdr (assoc 'content para)))
               (link-text (aref para-content 0)))
          (expect (cdr (assoc 'text link-text)) :to-equal "Link text")
          (let* ((marks (cdr (assoc 'marks link-text)))
                 (mark (aref marks 0)))
            (expect (cdr (assoc 'type mark)) :to-equal "link")
            (expect (cdr (assoc 'href (cdr (assoc 'attrs mark)))) :to-equal "https://example.com")))))))

(describe "org-confluence-publish--get-property"
  (it "retrieves property values from buffer"
    (with-temp-buffer
      (insert "#+TITLE: Test\n#+CONFLUENCE_PAGE_ID: 12345\n")
      (org-mode)
      (expect (org-confluence-publish--get-property "CONFLUENCE_PAGE_ID") :to-equal "12345")))

  (it "returns nil for non-existent properties"
    (with-temp-buffer
      (insert "#+TITLE: Test\n")
      (org-mode)
      (expect (org-confluence-publish--get-property "CONFLUENCE_PAGE_ID") :to-equal nil))))

(describe "org-confluence-publish--set-property"
  (it "adds new properties after title"
    (with-temp-buffer
      (insert "#+TITLE: Test\n\nSome content\n")
      (org-mode)
      (org-confluence-publish--set-property "CONFLUENCE_PAGE_ID" "12345")
      (goto-char (point-min))
      (expect (re-search-forward "^#\\+CONFLUENCE_PAGE_ID: 12345$" nil t) :to-be-truthy)))

  (it "updates existing properties"
    (with-temp-buffer
      (insert "#+TITLE: Test\n#+CONFLUENCE_PAGE_ID: 12345\n")
      (org-mode)
      (org-confluence-publish--set-property "CONFLUENCE_PAGE_ID" "67890")
      (goto-char (point-min))
      (expect (re-search-forward "^#\\+CONFLUENCE_PAGE_ID: 67890$" nil t) :to-be-truthy)
      (goto-char (point-min))
      (expect (re-search-forward "12345" nil t) :to-equal nil))))

(describe "nested link handling"
  (it "exports links with link mark correctly"
    (with-temp-buffer
      (insert "[[https://github.com/torvalds/linux/blob/master/kernel/sched/core.c#L1000-L1050][core.c:1000-1050]]\n")
      (org-mode)
      (let* ((adf-json (org-confluence-publish--export-to-adf))
             (adf (json-read-from-string adf-json))
             (content (cdr (assoc 'content adf)))
             (para (aref content 0))
             (para-content (cdr (assoc 'content para)))
             ;; First element is the link text
             (text-node (aref para-content 0)))
        ;; Check link text and mark
        (expect (cdr (assoc 'text text-node)) :to-equal "core.c:1000-1050")
        (let* ((marks (cdr (assoc 'marks text-node)))
               (mark (aref marks 0)))
          (expect (cdr (assoc 'type mark)) :to-equal "link")
          (expect (cdr (assoc 'href (cdr (assoc 'attrs mark))))
                  :to-equal "https://github.com/torvalds/linux/blob/master/kernel/sched/core.c#L1000-L1050")))))

  (it "handles links with multiple periods correctly"
    (with-temp-buffer
      (insert "[[https://example.com][file.name.go:123]]\n")
      (org-mode)
      (let* ((adf-json (org-confluence-publish--export-to-adf))
             (adf (json-read-from-string adf-json))
             (content (cdr (assoc 'content adf)))
             (para (aref content 0))
             (para-content (cdr (assoc 'content para)))
             ;; First element is the link text
             (text-node (aref para-content 0)))
        (expect (cdr (assoc 'text text-node)) :to-equal "file.name.go:123")))))

(describe "org-confluence-publish--validate-config"
  (it "signals error when base-url is missing"
    (let ((org-confluence-publish-base-url nil)
          (org-confluence-publish-email "test@example.com")
          (org-confluence-publish-api-token "token")
          (org-confluence-publish-space-key "SPACE"))
      (expect (org-confluence-publish--validate-config)
              :to-throw 'user-error)))

  (it "signals error when email is missing"
    (let ((org-confluence-publish-base-url "https://example.atlassian.net")
          (org-confluence-publish-email nil)
          (org-confluence-publish-api-token "token")
          (org-confluence-publish-space-key "SPACE"))
      (expect (org-confluence-publish--validate-config)
              :to-throw 'user-error)))

  (it "signals error when api-token is missing"
    (let ((org-confluence-publish-base-url "https://example.atlassian.net")
          (org-confluence-publish-email "test@example.com")
          (org-confluence-publish-api-token nil)
          (org-confluence-publish-space-key "SPACE"))
      (expect (org-confluence-publish--validate-config)
              :to-throw 'user-error)))

  (it "signals error when space-key is missing"
    (let ((org-confluence-publish-base-url "https://example.atlassian.net")
          (org-confluence-publish-email "test@example.com")
          (org-confluence-publish-api-token "token")
          (org-confluence-publish-space-key nil))
      (expect (org-confluence-publish--validate-config)
              :to-throw 'user-error)))

  (it "does not signal error when all required config is present"
    (let ((org-confluence-publish-base-url "https://example.atlassian.net")
          (org-confluence-publish-email "test@example.com")
          (org-confluence-publish-api-token "token")
          (org-confluence-publish-space-key "SPACE"))
      (expect (org-confluence-publish--validate-config) :not :to-throw))))

(describe "org-confluence-publish--find-images"
  (it "finds file links with image extensions"
    (with-temp-buffer
      (insert "[[file:image.png]]\n")
      (insert "[[file:photo.jpg]]\n")
      (insert "[[file:diagram.svg]]\n")
      (org-mode)
      (let ((buffer-file-name "/tmp/test.org"))
        ;; Mock file-exists-p to return true for test files
        (cl-letf (((symbol-function 'file-exists-p)
                   (lambda (path) t)))
          (let ((images (org-confluence-publish--find-images)))
            (expect (length images) :to-equal 3)
            (expect (member "/tmp/image.png" images) :to-be-truthy)
            (expect (member "/tmp/photo.jpg" images) :to-be-truthy)
            (expect (member "/tmp/diagram.svg" images) :to-be-truthy))))))

  (it "ignores non-image file links"
    (with-temp-buffer
      (insert "[[file:document.pdf]]\n")
      (insert "[[file:script.sh]]\n")
      (org-mode)
      (let ((buffer-file-name "/tmp/test.org"))
        (cl-letf (((symbol-function 'file-exists-p)
                   (lambda (path) t)))
          (let ((images (org-confluence-publish--find-images)))
            (expect (length images) :to-equal 0))))))

  (it "ignores http links"
    (with-temp-buffer
      (insert "[[https://example.com/image.png]]\n")
      (org-mode)
      (let ((buffer-file-name "/tmp/test.org"))
        (cl-letf (((symbol-function 'file-exists-p)
                   (lambda (path) t)))
          (let ((images (org-confluence-publish--find-images)))
            (expect (length images) :to-equal 0))))))

  (it "deduplicates duplicate image references"
    (with-temp-buffer
      (insert "[[file:image.png]]\n")
      (insert "Some text\n")
      (insert "[[file:image.png]]\n")
      (org-mode)
      (let ((buffer-file-name "/tmp/test.org"))
        (cl-letf (((symbol-function 'file-exists-p)
                   (lambda (path) t)))
          (let ((images (org-confluence-publish--find-images)))
            (expect (length images) :to-equal 1))))))

  (it "excludes files that do not exist"
    (with-temp-buffer
      (insert "[[file:exists.png]]\n")
      (insert "[[file:missing.png]]\n")
      (org-mode)
      (let ((buffer-file-name "/tmp/test.org"))
        (cl-letf (((symbol-function 'file-exists-p)
                   (lambda (path)
                     (string-match-p "exists\\.png$" path))))
          (let ((images (org-confluence-publish--find-images)))
            (expect (length images) :to-equal 1)
            (expect (member "/tmp/exists.png" images) :to-be-truthy)))))))

(describe "org-confluence-publish--auth-header"
  (it "generates Basic Auth header with base64 encoding"
    (let ((org-confluence-publish-email "user@example.com")
          (org-confluence-publish-api-token "test-token"))
      (let ((header (org-confluence-publish--auth-header)))
        (expect header :to-match "^Basic ")
        ;; Verify it's base64 (should decode back to email:token)
        (let* ((encoded (substring header 6))
               (decoded (base64-decode-string encoded)))
          (expect decoded :to-equal "user@example.com:test-token"))))))

(describe "draft to published version logic"
  (it "keeps version 1 when publishing draft for the first time"
    ;; Mock the API call to verify the version sent
    (let* ((org-confluence-publish-base-url "https://example.atlassian.net")
           (captured-payload nil)
           (mock-callback (lambda (success data) nil)))
      (cl-letf (((symbol-function 'org-confluence-publish--request)
                 (lambda (method url payload callback &optional attempt files)
                   (setq captured-payload payload)
                   (funcall callback t '((id . "123")
                                        (version . ((number . 1)))
                                        (_links . ((webui . "/page"))))))))
        (org-confluence-publish--update-page "123" "1" "Test" "{}" mock-callback)
        ;; Verify version in payload is 1, not 2
        (expect (cdr (assoc 'number (cdr (assoc 'version captured-payload)))) :to-equal 1))))

  (it "increments version when updating published page"
    (let* ((org-confluence-publish-base-url "https://example.atlassian.net")
           (captured-payload nil)
           (mock-callback (lambda (success data) nil)))
      (cl-letf (((symbol-function 'org-confluence-publish--request)
                 (lambda (method url payload callback &optional attempt files)
                   (setq captured-payload payload)
                   (funcall callback t '((id . "123")
                                        (version . ((number . 3)))
                                        (_links . ((webui . "/page"))))))))
        (org-confluence-publish--update-page "123" "2" "Test" "{}" mock-callback)
        ;; Verify version incremented from 2 to 3
        (expect (cdr (assoc 'number (cdr (assoc 'version captured-payload)))) :to-equal 3))))

  (it "sets status to current when updating"
    (let* ((org-confluence-publish-base-url "https://example.atlassian.net")
           (captured-payload nil)
           (mock-callback (lambda (success data) nil)))
      (cl-letf (((symbol-function 'org-confluence-publish--request)
                 (lambda (method url payload callback &optional attempt files)
                   (setq captured-payload payload)
                   (funcall callback t '((id . "123")
                                        (version . ((number . 1)))
                                        (_links . ((webui . "/page"))))))))
        (org-confluence-publish--update-page "123" "1" "Test" "{}" mock-callback)
        ;; Verify status is "current" (published), not "draft"
        (expect (cdr (assoc 'status captured-payload)) :to-equal "current")))))

(describe "per-file parent page ID"
  (it "reads CONFLUENCE_PARENT_ID property from buffer"
    (with-temp-buffer
      (insert "#+TITLE: Test\n#+CONFLUENCE_PARENT_ID: 789012\n")
      (org-mode)
      (expect (org-confluence-publish--get-property "CONFLUENCE_PARENT_ID") :to-equal "789012")))

  (it "includes parentId in payload when parent-id provided"
    (let* ((org-confluence-publish-base-url "https://example.atlassian.net")
           (org-confluence-publish-space-key "TEST")
           (captured-payload nil)
           (mock-callback (lambda (success data) nil)))
      (cl-letf (((symbol-function 'org-confluence-publish--get-space-id)
                 (lambda (callback)
                   (funcall callback t "space-123")))
                ((symbol-function 'org-confluence-publish--request)
                 (lambda (method url payload callback &optional attempt files)
                   (setq captured-payload payload)
                   (funcall callback t '((id . "new-page")
                                        (version . ((number . 1)))
                                        (_links . ((webui . "/page"))))))))
        (org-confluence-publish--create-page "Test" "{}" mock-callback "789012")
        ;; Verify parentId is in payload
        (expect (cdr (assoc 'parentId captured-payload)) :to-equal "789012"))))

  (it "excludes parentId from payload when parent-id is nil"
    (let* ((org-confluence-publish-base-url "https://example.atlassian.net")
           (org-confluence-publish-space-key "TEST")
           (captured-payload nil)
           (mock-callback (lambda (success data) nil)))
      (cl-letf (((symbol-function 'org-confluence-publish--get-space-id)
                 (lambda (callback)
                   (funcall callback t "space-123")))
                ((symbol-function 'org-confluence-publish--request)
                 (lambda (method url payload callback &optional attempt files)
                   (setq captured-payload payload)
                   (funcall callback t '((id . "new-page")
                                        (version . ((number . 1)))
                                        (_links . ((webui . "/page"))))))))
        (org-confluence-publish--create-page "Test" "{}" mock-callback nil)
        ;; Verify parentId is NOT in payload
        (expect (assoc 'parentId captured-payload) :to-equal nil))))

  (it "excludes parentId from payload when parent-id is empty string"
    (let* ((org-confluence-publish-base-url "https://example.atlassian.net")
           (org-confluence-publish-space-key "TEST")
           (captured-payload nil)
           (mock-callback (lambda (success data) nil)))
      (cl-letf (((symbol-function 'org-confluence-publish--get-space-id)
                 (lambda (callback)
                   (funcall callback t "space-123")))
                ((symbol-function 'org-confluence-publish--request)
                 (lambda (method url payload callback &optional attempt files)
                   (setq captured-payload payload)
                   (funcall callback t '((id . "new-page")
                                        (version . ((number . 1)))
                                        (_links . ((webui . "/page"))))))))
        (org-confluence-publish--create-page "Test" "{}" mock-callback "")
        ;; Verify parentId is NOT in payload for empty string
        (expect (assoc 'parentId captured-payload) :to-equal nil)))))

(describe "org-confluence-publish--remove-confluence-properties"
  (it "removes all CONFLUENCE_* properties"
    (with-temp-buffer
      (org-mode)
      (insert "#+TITLE: Test\n")
      (insert "#+CONFLUENCE_PAGE_ID: 12345\n")
      (insert "#+CONFLUENCE_VERSION: 2\n")
      (insert "#+CONFLUENCE_URL: https://example.com\n")
      (insert "#+CONFLUENCE_PARENT_ID: 67890\n")
      (insert "\nContent\n")
      (org-confluence-publish--remove-confluence-properties)
      (expect (buffer-string) :to-equal "#+TITLE: Test\n\nContent\n"))))

(describe "org-confluence-publish--delete-page"
  (it "calls DELETE endpoint with correct URL"
    (let ((org-confluence-publish-base-url "https://example.atlassian.net")
          (captured-method nil)
          (captured-url nil))
      (cl-letf (((symbol-function 'org-confluence-publish--request)
                 (lambda (method url payload callback)
                   (setq captured-method method)
                   (setq captured-url url)
                   (funcall callback t nil))))
        (org-confluence-publish--delete-page "12345" (lambda (s r)))
        (expect captured-method :to-equal "DELETE")
        (expect captured-url :to-equal "https://example.atlassian.net/wiki/api/v2/pages/12345")))))

;;; org-confluence-publish-test.el ends here
