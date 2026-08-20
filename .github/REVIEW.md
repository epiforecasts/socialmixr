# Reviewing a change to socialmixr

What to look for in a change to socialmixr specifically. The reviewing method —
how to scope a review, what counts as a finding, how to report and suggest the
fix, the trust rules — is the org half of this spec (`epiforecasts/.github` →
`REVIEW.md`), and a review follows both.

## What to look for

- Correctness bugs and unhandled edge cases
- **data.table pitfalls**: `:=` modifying a caller's table by reference without
  `copy()`, assumptions about key or row order that the code does not itself
  set, `.SD`/`.SDcols` misuse
- **R footguns that bite on empty or single-row input**: `1:n` where `seq_len(n)`
  is meant, `drop = TRUE` collapsing a data frame to a vector, silent recycling,
  `sapply` returning an unexpected type
- **Validation gaps** where the surrounding code uses checkmate to enforce the
  same contract
- **Test coverage** for the changed behaviour — this package's convention is a
  regression test for every bug fix
- **Exported API and roxygen accuracy**: documented arguments matching the
  signature, `@export` matching NAMESPACE, `.Rd` files regenerated
- Anything `CLAUDE.md` in this repo asks for that the change skips — a NEWS entry
  for user-visible changes, deprecation handled the way the file describes

Read `CLAUDE.md` before starting; it is the source for this package's
conventions, and this list does not restate all of it.
