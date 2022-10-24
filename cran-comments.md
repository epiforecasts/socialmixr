## Test environments
* local macOS install (12.6), R 4.2.1
* R-hub
* winbuilder

## R CMD check results

There were no ERRORs or WARNINGs. 

There was one NOTE:

```
* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'
```
As noted in [R-hub issue #503](https://github.com/r-hub/rhub/issues/503), this could be due to a bug/crash in MiKTeX and can likely be ignored.

## Downstream dependencies

0 packages with problems.

## Resubmission notes

A previous NOTE on long-running examples has been resolved.
