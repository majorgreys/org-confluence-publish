# Development Workflow

This document outlines the development workflow and conventions for contributing to org-confluence-publish.

## Commit Message Convention

This project follows [Conventional Commits](https://www.conventionalcommits.org/) specification.

### Format

```
<type>[optional scope]: <description>

[optional body]

[optional footer(s)]
```

### Types

- **feat**: A new feature
- **fix**: A bug fix
- **docs**: Documentation only changes
- **style**: Changes that do not affect the meaning of the code (white-space, formatting, etc)
- **refactor**: A code change that neither fixes a bug nor adds a feature
- **test**: Adding missing tests or correcting existing tests
- **chore**: Changes to the build process or auxiliary tools and libraries

### Examples

```
feat: add support for inline images
fix: handle empty table cells correctly
docs: update installation instructions
test: add coverage for nested lists
chore: update dependencies
```

### Rules

1. Use lowercase for type and description
2. Do not end the subject line with a period
3. Use the imperative mood in the subject line ("add" not "adds" or "added")
4. Limit the subject line to 50 characters
5. Separate subject from body with a blank line
6. Wrap the body at 72 characters
7. Use the body to explain what and why vs. how

## Development Setup

### Prerequisites

- Emacs 26.1+
- Eldev (for testing and linting)

### Install Eldev

```bash
curl -fsSL https://raw.github.com/emacs-eldev/eldev/master/webinstall/eldev | sh
```

### Running Tests

```bash
# Run all tests
eldev test

# Run tests with specific Emacs version
eldev -e 26.3 test
eldev -e 29.4 test

# Run lint checks
eldev lint
```

## Testing

All tests must pass before submitting a pull request. The project uses [Buttercup](https://github.com/jorgenschaefer/emacs-buttercup) as the testing framework.

Tests are located in `test/org-confluence-publish-test.el`.

## Pull Request Process

1. Create a new branch from `main`
2. Make your changes following the commit message convention
3. Ensure all tests pass (`eldev test`)
4. Update documentation as needed
5. Submit a pull request

## Code Style

- Follow standard Emacs Lisp conventions
- Use `lexical-binding: t`
- Keep functions focused and well-documented
- Add docstrings to all public functions
- Use descriptive variable names

## ADF Validation

When modifying ADF export code, validate the output:

```bash
# Export to ADF
emacs --batch -l ox-adf.el --eval "(progn (find-file \"file.org\") (princ (ox-adf-export-as-string)))" > output.json

# Validate against official schema
npx --yes adf-validator output.json
```

## Release Process

1. Update version in package headers
2. Update CHANGELOG.md
3. Tag the release: `git tag v0.2.0`
4. Push with tags: `git push origin main --tags`
