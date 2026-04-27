# Test environments

* local Ubuntu install (R 4.5.2)
* GitHub Actions (ubuntu-latest, windows-latest, macos-latest)
* win-builder (devel)

# R CMD check results

There were no ERRORs, WARNINGs or NOTEs.

# Reverse dependencies

All four reverse dependencies (contactsurveys, finalsize, multigroup.vaccine,
o2geosocial) checked cleanly with `revdepcheck::revdep_check()`.
