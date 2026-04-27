# Resubmission

This is a resubmission of socialmixr 0.6.0. The previous submission failed
the auto-check on `finalsize` and `multigroup.vaccine` because their tests
and vignettes call `socialmixr::contact_matrix(countries = ...)` without
supplying `survey_pop`, which fell through to `wpp_age()` after wpp2017 had
been moved from Imports to Suggests.

In this submission `wpp2017` has been kept in Imports and a deprecation
warning has been added to the implicit lookup path; the move to Suggests
is deferred to a future release once downstream packages have had time to
adjust.

# Test environments

* local Ubuntu install (R 4.5.2)
* GitHub Actions (ubuntu-latest, windows-latest, macos-latest)
* win-builder (devel)

# R CMD check results

There were no ERRORs, WARNINGs or NOTEs.

# Reverse dependencies

All four reverse dependencies (contactsurveys, finalsize, multigroup.vaccine,
o2geosocial) checked cleanly with `revdepcheck::revdep_check()`.
