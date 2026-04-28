# Resubmission

This is a resubmission of `socialmixr` 0.6.0. The previous submission
caused failures in two reverse dependencies; this version keeps the
relevant dependency in place and instead deprecates the affected code
path so downstream packages can migrate before it is removed. All four
reverse dependencies now check cleanly (see below).

# Test environments

* local Ubuntu install (R 4.5.2)
* GitHub Actions (ubuntu-latest, windows-latest, macos-latest)
* win-builder (devel)

# R CMD check results

There were no ERRORs, WARNINGs or NOTEs.

# Reverse dependencies

All four reverse dependencies (contactsurveys, finalsize, multigroup.vaccine,
o2geosocial) checked cleanly with `revdepcheck::revdep_check()`.
