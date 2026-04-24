# Test environments

* local Ubuntu install (R 4.5.2)
* GitHub Actions (ubuntu-latest, windows-latest, macos-latest)
* win-builder (devel)

# R CMD check results

There were no ERRORs, WARNINGs or NOTEs.

# Reverse dependencies

There is one reverse dependency (`multigroup.vaccine`).

# Notes

This release introduces a composable pipeline workflow for contact matrix
computation. See NEWS.md for details. Terminal age group labels using
bracket notation have been changed from `N+` to `[N,Inf)` for consistency
with the contactmatrix package; code matching on strings like `"15+"` may
need to be updated. Dash notation (e.g. `"15+"`) is unchanged.
