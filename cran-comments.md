# Resubmission

This is a resubmission. The previous submission was flagged at incoming
pre-test for a NOTE on Debian: the `compute_matrix()` example used more CPU
time than elapsed time (a multithreading artefact from data.table). The
example now runs data.table single-threaded, so CPU and elapsed time match.

# Submission

This is a minor release (0.7.0) adding support for contact matrices across
more than one grouping, new age-rebinning helpers, and related changes. It
also deprecates a few older code paths (the dotted argument names, `pop_age()`,
and the implicit population lookup in `contact_matrix()`). These continue to
work with a deprecation warning, so reverse dependencies keep passing and can
migrate before anything is removed.

# Test environments

* local Ubuntu install (R 4.5.2)
* GitHub Actions (ubuntu-latest, windows-latest, macos-latest)
* win-builder (devel)

# R CMD check results

There were no ERRORs, WARNINGs or NOTEs.

# Reverse dependencies

All four reverse dependencies (contactsurveys, finalsize, multigroup.vaccine,
o2geosocial) checked cleanly with `revdepcheck::revdep_check()`.
