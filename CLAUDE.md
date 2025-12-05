# Claude Code Instructions for socialmixr

## Development Workflow

### Before committing
1. Run `devtools::document()` to regenerate documentation from roxygen comments
2. Run `lintr::lint_package()` to check for style issues (fix any in files you modified)
3. Add a news entry to NEWS

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
