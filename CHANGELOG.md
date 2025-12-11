# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Fixed
- **ox-adf.el**: Table cells now wrap inline content in paragraph nodes as required by ADF schema
  - Previously generated invalid ADF with bare text nodes in table cells
  - Confluence accepted invalid ADF for small documents but rejected it for larger ones (causing 500 errors)
  - All table cell content now properly wrapped in block-level nodes per ADF specification
  - Empty table cells now use single space instead of empty string (ADF requires minLength: 1)
- **ox-adf.el**: Strip trailing newlines from text nodes
  - Prevents extra spacing/line breaks in rendered lists and paragraphs
  - Text nodes no longer include trailing `\n` characters from org-export
- ADF validation helper function: `org-confluence-publish-validate-adf`
  - Validates ADF JSON against official Atlassian schema using adf-validator
  - Useful for development, debugging, and ensuring schema compliance
- **ox-adf.el**: Disable babel processing during export
  - Prevents unnecessary code block evaluation during ADF export
  - Eliminates "org-babel-exp process..." messages when publishing
  - Code blocks are exported as-is without evaluation
- **org-confluence-publish.el**: Prevent git-auto-commit-mode conflicts
  - Temporarily disables git-auto-commit-mode during buffer save
  - Prevents auto-revert from removing CONFLUENCE properties
  - Fixes integration with Doom Emacs and org-roam auto-commit workflows

### Added
- **License**: GPL-3.0-or-later
  - Added LICENSE file with full GPL-3.0 text
  - Updated package headers with SPDX identifier
  - Added badges to README (CI status, license, Emacs version)
  - Standard open source license for Emacs packages
- Eldev configuration for development and testing
  - Multi-version testing support (Emacs 26.3, 27.2, 28.2, 29.4, snapshot)
  - GitHub Actions CI workflow using setup-eldev
  - Automated dependency management and test execution
- Acknowledgements section in README
  - Credits sync-docs.el for inspiration
  - Acknowledges Claude AI assistance in development
- Initial MVP release
- One-way sync: Org files → Confluence Cloud
- Publish new pages as drafts (Confluence API status: "draft")
- Update existing pages as published (Confluence API status: "current")
- Automatic image upload and embedding
- Property-based tracking (`#+CONFLUENCE_PAGE_ID:`, `#+CONFLUENCE_VERSION:`, `#+CONFLUENCE_URL:`)
- Command: `org-confluence-publish-buffer` - Main publish/update command
- Command: `org-confluence-publish-open-page` - Open published page in browser
- Command: `org-confluence-publish-debug-page` - Debug helper to inspect page state
- Command: `org-confluence-publish-unpublish` - Delete page from Confluence and remove properties
  - Requires confirmation before deleting
  - Moves page to trash (not permanent deletion)
  - Removes all `#+CONFLUENCE_*` properties from org file
  - Auto-saves buffer after unpublish
- Improved draft status messages with version information
  - Draft publishing now shows version transition and status change
  - More informative feedback when publishing draft pages
- Configuration variables: `org-confluence-publish-base-url`, `org-confluence-publish-email`, `org-confluence-publish-api-token`, `org-confluence-publish-space-key`, `org-confluence-publish-parent-id`
- API retry logic with exponential backoff for 429 (rate limit) and 5xx errors
- Image discovery using org-element parser
- Robust state synchronization: Always queries Confluence for current page state before updates
- Trashed page detection: Clear error messages when attempting to update trashed pages
- Version drift detection: Automatically syncs with Confluence version when local version is stale
- URL change detection: Detects and updates org file when draft URLs change to published URLs
- Status logging: Reports page status (draft/current/trashed) for visibility
- Per-file parent page ID via `#+CONFLUENCE_PARENT_ID:` property
  - Overrides global `org-confluence-publish-parent-id` when set
  - Only applies to new page creation (parent is immutable)

### ox-adf.el - Native ADF Export Backend
- 22 transcoders for all org element types
- Supports: headings (1-6), paragraphs, inline marks (bold, italic, code, strikethrough, underline), subscript/superscript, lists (bullet, ordered, definition), code blocks, tables, blockquotes, links, images
- Text node consolidation for optimized ADF output
- Pure elisp implementation (no external dependencies)

### Technical Details
- Two-file implementation (~380 + ~520 lines)
- Dependencies: emacs 26.1+, org 9.0+, request 0.3.0+
- Confluence REST API v2 exclusively (pages and attachments)
- Basic Auth with email and API token
- Space ID caching to reduce API calls
- 38 unit tests (buttercup)

[Unreleased]: https://github.com/majorgreys/org-confluence-publish/commits/main
