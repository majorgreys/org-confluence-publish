# org-confluence-publish

Publish Org mode files to Confluence Cloud as native Atlassian Document Format (ADF) pages.

## Features

- **One-way sync**: Org file → Confluence (no two-way sync)
- **Native ADF export**: Pure elisp, no external dependencies
- **Draft workflow**: New pages created as drafts, updates publish as current
- **Automatic image upload**: Embedded images uploaded as attachments
- **Property tracking**: Page ID, version, and URL stored in org file
- **Robust state sync**: Always checks live page state before updates, handles version drift and URL changes gracefully

## Requirements

- Emacs 26.1+
- Org mode 9.0+
- `request` package

## Installation

```elisp
;; Clone repository
;; Add to load-path and require
(add-to-list 'load-path "/path/to/org-confluence-publish")
(require 'org-confluence-publish)
```

## Configuration

```elisp
(setq org-confluence-publish-base-url "https://yoursite.atlassian.net"
      org-confluence-publish-email "your-email@example.com"
      org-confluence-publish-api-token "your-api-token"
      org-confluence-publish-space-key "SPACE"
      org-confluence-publish-parent-id "123456")  ; optional
```

### Per-File Parent Page

Override the global parent ID for individual files:

```org
#+TITLE: My Document
#+CONFLUENCE_PARENT_ID: 789012

Content here...
```

Priority order:
1. File property `#+CONFLUENCE_PARENT_ID:`
2. Global `org-confluence-publish-parent-id`
3. No parent (top-level page)

Note: Parent ID only applies to new page creation. Parent relationship cannot be changed after creation.

## Usage

1. Open an Org file
2. Run `M-x org-confluence-publish-buffer`
3. First publish creates a draft page
4. Subsequent publishes update as published

To open the published page in browser: `M-x org-confluence-publish-open-page`

## State Sync and Error Handling

The package automatically handles common edge cases:

- **Version drift**: If the page was edited in Confluence directly, the package detects version mismatches and uses the live version from Confluence
- **Draft → Published transition**: If a draft page was manually published in Confluence, the package detects the URL change and updates the org file accordingly
- **Trashed pages**: If a page is in trash, the package provides clear guidance: restore it in Confluence or remove `CONFLUENCE_PAGE_ID` property to create a new page
- **Status visibility**: Logs the current page status (draft/published) for awareness

Before each update, the package queries Confluence for the current page state to ensure operations succeed.

## Supported Org Elements

- Headings (levels 1-6)
- Paragraphs with inline formatting (bold, italic, code, strikethrough, underline)
- Subscript and superscript
- Bullet, ordered, and definition lists
- Code blocks with language highlighting
- Tables
- Blockquotes
- Links (external URLs)
- Images (standalone and inline)

## ADF Validation

This package exports Org documents to Atlassian Document Format (ADF), the native format used by Confluence. To ensure generated ADF conforms to the official schema:

### Validation Tools

- **Official schema**: http://go.atlassian.com/adf-json-schema
- **Interactive playground**: https://developer.atlassian.com/cloud/jira/platform/apis/document/playground/
- **CLI validator**: [adf-validator](https://github.com/torifat/adf-validator) (npm package)

### Manual Validation

To validate ADF output for any Org file:

```bash
# Export to ADF
emacs --batch -l ox-adf.el --eval "(progn (find-file \"file.org\") (princ (ox-adf-export-as-string)))" > output.json

# Validate against official schema
npx --yes adf-validator output.json
```

### Programmatic Validation

Use the built-in validation function:

```elisp
(require 'org-confluence-publish)
(require 'ox-adf)

;; Validate exported ADF
(let ((adf-json (ox-adf-export-as-string)))
  (org-confluence-publish-validate-adf adf-json))
;; Returns t if valid, signals error with details if invalid
```

**Note**: Validation requires Node.js and npx. The validator is automatically downloaded when first used.

## Optional Keybindings

```elisp
(define-key org-mode-map (kbd "C-c C-x p") #'org-confluence-publish-buffer)
(define-key org-mode-map (kbd "C-c C-x o") #'org-confluence-publish-open-page)
```

## License

MIT
