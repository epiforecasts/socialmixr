# List all surveys available for download

**\[deprecated\]**

`list_surveys()` has been deprecated in favour of
`contactsurveys::list_surveys()`.

## Usage

``` r
list_surveys(clear_cache = FALSE)
```

## Arguments

- clear_cache:

  logical, whether to clear the cache before downloading the survey; by
  default, the cache is not cleared and so multiple calls of this
  function to access the same survey will not result in repeated
  downloads.

## Value

character vector of surveys

## Examples

``` r
# we recommend using the contactsurveys package now for listing surveys.
if (FALSE) { # \dontrun{
contactsurveys::list_surveys()
} # }
```
