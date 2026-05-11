# Citation for a survey

**\[defunct\]**

`get_citation()` is defunct. Use
[`contactsurveys::get_citation()`](http://epiforecasts.io/contactsurveys/reference/get_citation.md)
instead.

## Usage

``` r
get_citation(x)
```

## Arguments

- x:

  a character vector of surveys to cite

## Value

Always errors.

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
