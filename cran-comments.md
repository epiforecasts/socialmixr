## Resubmission
There was a NOTE about an example taking too long - this is very likely because of the duration of a call to countrycode::countrycode, which can't be fixed here. The example is no longer run.

Install on r-oldrel fails - this is expected because the `addNA` argument to `xtabs` does not exist.

## Test environments
* local macOS install (10.13.2), R 3.4.3
* local linux install (Ubuntu 16.04), R 3.4.3
* Ubuntu (Trusty) on travis-ci

## R CMD check results
There were no ERRORs or WARNINGs. 

There was a note about a potential mis-spelling in the DESCRIPTION (an author name, which is spelled correctly, and "et al.").

## Downstream dependencies
0 packages with problems.
