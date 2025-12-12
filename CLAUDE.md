# Claude Code Instructions for socialmixr

## Development Workflow

### Before committing
1. Run `devtools::test()` to ensure all tests pass
2. Run `devtools::document()` to regenerate documentation from roxygen comments
3. Run `lintr::lint_package()` to check for style issues (fix any in files you modified)
4. Add a news entry to NEWS.md (can be edited for unreleased changes)
5. Add tests for bug fixes (regression tests) or new features where appropriate

### Backwards compatibility
- Only functions exported in a previous CRAN release require deprecation warnings
- Changes between releases (i.e., in the development version) don't require deprecation
- NEWS.md entries for unreleased changes can be freely edited or consolidated

### Branching
- Always create a feature branch for changes (never commit directly to main)
- Use descriptive branch names (e.g., `fix-age-group-filtering`, `add-new-feature`)

### Commit conventions
- Write one-sentence commit messages (detail goes in PR description)
- When AI assists with code, indicate this with a bot co-author (e.g., `Co-authored-by: username-bot <email>`)
- Issue numbers belong in PR descriptions, not commit messages
- Keep commits focused - one logical change per commit

### Pull requests
- Reference the issue being addressed (e.g., "Fixes #123")
- Provide detailed explanation in the PR description, not the commit message
- Do not include "Generated with Claude Code" or test plans in PR descriptions
