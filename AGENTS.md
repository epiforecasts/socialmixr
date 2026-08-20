# Working on socialmixr with a coding assistant

This file is the entry point for any assistant — Codex, Cursor, Claude Code,
or anything else that reads `AGENTS.md`. It deliberately holds no conventions of
its own, so there is nothing here to drift out of step with the files it points
at.

- **Conventions for changing this package** — testing, documenting, NEWS
  entries, backwards compatibility: read `CLAUDE.md`. Despite the name it is
  tool-neutral, and it is the source of truth for how work here is done.

- **Reviewing a change** — what to look for, and what is not worth reporting:
  read `.github/REVIEW.md`.

The repo deliberately ships no editor- or assistant-specific command files. If
you want `/review` as a one-word command, wire it up in your own configuration
to point at `.github/REVIEW.md`; the criteria stay here so everyone reviews
against the same bar.
