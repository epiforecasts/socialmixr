# Test environments

* local Ubuntu install (R 4.5.2)
* GitHub Actions (ubuntu-latest, windows-latest, macos-latest)
* win-builder (devel)

# R CMD check results

There were no ERRORs or WARNINGs. There was one NOTE about a possibly
invalid timestamp ("unable to verify current time"), which is a transient
network issue on the check host and unrelated to the package.

# Reverse dependencies

There is one reverse dependency (`multigroup.vaccine`); no problems are
expected from this release.

# Notes

This release adds a composable pipeline for contact matrix computation
(`assign_age_groups()`, `weigh()`, `compute_matrix()`, `symmetrise()`,
`split_matrix()`, `per_capita()`) and a `contact_matrix` S3 class. See
NEWS.md for details. Two breaking changes:

* Minimum R version bumped from 3.5.0 to 4.1.0 (the pipeline examples use
  the native `|>` pipe).
* Terminal age group labels using bracket notation changed from `N+` to
  `[N,Inf)` to match the contactmatrix package. Code matching on strings
  like `"15+"` may need updating; dash notation (e.g. `"15+"`) is
  unchanged.
