# Claude Code Instructions for socialmixr

## Development Workflow

### Before committing

1.  Run
    [`devtools::document()`](https://devtools.r-lib.org/reference/document.html)
    to regenerate documentation from roxygen comments
2.  Run `lintr::lint_package()` to check for style issues (fix any in
    files you modified)

### Commit conventions

- Write short, atomic commit messages describing what changed

- The repository owner will be the committer; use `sbfnk-bot` as
  co-author:

      Co-authored-by: sbfnk-bot <242615673+sbfnk-bot@users.noreply.github.com>

- Issue numbers belong in PR descriptions, not commit messages

- Keep commits focused - one logical change per commit

### Pull requests

- Reference the issue being addressed (e.g., “Fixes \#123”)
- Provide detailed explanation in the PR description, not the commit
  message
