# Load a survey from local files

Loads a survey from a local file system. Tables are expected as csv
files, and a reference (if present) as JSON.

## Usage

``` r
load_survey(files, ...)
```

## Arguments

- files:

  a vector of file names as returned by
  [`download_survey()`](https://epiforecasts.io/socialmixr/reference/download_survey.md)

- ...:

  options for
  [`clean()`](https://epiforecasts.io/socialmixr/reference/clean.md),
  which is called at the end of this

## Value

a survey in the correct format

## Examples

``` r
if (FALSE) { # \dontrun{
list_surveys()
peru_files <- download_survey("https://doi.org/10.5281/zenodo.1095664")
peru_survey <- load_survey(peru_files)
} # }
```
