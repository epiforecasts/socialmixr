# Citation for a survey

**\[deprecated\]**

`get_citation()` has been deprecated in favour of
[`contactsurveys::get_citation()`](http://epiforecasts.io/contactsurveys/reference/get_citation.md).

Gets a full citation for a
[`survey()`](https://epiforecasts.io/socialmixr/reference/survey.md).

## Usage

``` r
get_citation(x)
```

## Arguments

- x:

  a character vector of surveys to cite

## Value

citation as bibentry

## Examples

``` r
# we recommend using the contactsurveys package for get_citation()
if (FALSE) { # \dontrun{
data(polymod)
citation <- contactsurveys::get_citation(polymod)
print(citation)
print(citation, style = "bibtex")
} # }
```
