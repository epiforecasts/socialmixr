# Claude Code Instructions for socialmixr

## Development Workflow

### Before committing

1.  Run
    [`devtools::test()`](https://devtools.r-lib.org/reference/test.html)
    to ensure all tests pass
2.  Run
    [`devtools::document()`](https://devtools.r-lib.org/reference/document.html)
    to regenerate documentation from roxygen comments
3.  Run `lintr::lint_package()` to check for style issues (fix any in
    files you modified)
4.  Add a news entry to NEWS.md
5.  Add tests for bug fixes (regression tests) or new features where
    appropriate

### Branching

- Always create a feature branch for changes (never commit directly to
  main)
- Use descriptive branch names (e.g., `fix-age-group-filtering`,
  `add-new-feature`)

### Commit conventions

- Write one-sentence commit messages (detail goes in PR description)
- When AI assists with code, indicate this with a bot co-author (e.g.,
  `Co-authored-by: username-bot <email>`)
- Issue numbers belong in PR descriptions, not commit messages
- Keep commits focused - one logical change per commit

### Pull requests

- Reference the issue being addressed (e.g., “Fixes \#123”)
- Provide detailed explanation in the PR description, not the commit
  message
- Do not include “Generated with Claude Code” or test plans in PR
  descriptions
