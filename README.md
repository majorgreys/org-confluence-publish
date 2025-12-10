# org-confluence-publish

Publish Org mode files to Confluence Cloud as native Atlassian Document Format (ADF) pages.

## Features

- **One-way sync**: Org file → Confluence (no two-way sync)
- **Native ADF export**: Pure elisp, no external dependencies
- **Draft workflow**: New pages created as drafts, updates publish as current
- **Automatic image upload**: Embedded images uploaded as attachments
- **Property tracking**: Page ID, version, and URL stored in org file

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

## Usage

1. Open an Org file
2. Run `M-x org-confluence-publish-buffer`
3. First publish creates a draft page
4. Subsequent publishes update as published

To open the published page in browser: `M-x org-confluence-publish-open-page`

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

## Optional Keybindings

```elisp
(define-key org-mode-map (kbd "C-c C-x p") #'org-confluence-publish-buffer)
(define-key org-mode-map (kbd "C-c C-x o") #'org-confluence-publish-open-page)
```

## License

MIT
