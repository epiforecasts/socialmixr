# Draws an image plot of a contact matrix with a legend strip and the numeric values in the cells.

This function combines the R image.plot function with numeric contact
rates in the matrix cells.

## Usage

``` r
matrix_plot(
  mij,
  min.legend = 0,
  max.legend = NA,
  num.digits = 2,
  num.colors = 50,
  main,
  xlab,
  ylab,
  legend.width,
  legend.mar,
  legend.shrink,
  cex.lab,
  cex.axis,
  cex.text,
  color.palette = heat.colors
)
```

## Arguments

- mij:

  a contact matrix containing contact rates between participants of age
  i (rows) with contacts of age j (columns). This is the default matrix
  format of
  [`contact_matrix()`](https://epiforecasts.io/socialmixr/reference/contact_matrix.md).

- min.legend:

  the color scale minimum (default = 0). Set to NA to use the minimum
  value of `mij`.

- max.legend:

  the color scale maximum (default = NA). Set to NA to use the maximum
  value of `mij`.

- num.digits:

  the number of digits when rounding the contact rates (default = 2).
  Use NA to disable this.

- num.colors:

  the number of color breaks (default = 50)

- main:

  the figure title

- xlab:

  a title for the x axis (default: "Age group (years)")

- ylab:

  a title for the y axis (default: "Contact age group (years)")

- legend.width:

  width of the legend strip in characters. Default is 1.

- legend.mar:

  width in characters of legend margin. Default is 5.1.

- legend.shrink:

  amount to shrink the size of legend relative to the full height or
  width of the plot. Default is 0.9.

- cex.lab:

  size of the x and y labels (default: 1.2)

- cex.axis:

  size of the axis labels (default: 0.8)

- cex.text:

  size of the numeric values in the matrix (default: 1)

- color.palette:

  the color palette to use (default:
  [`heat.colors()`](https://rdrr.io/r/grDevices/palettes.html)). Other
  examples are
  [`topo.colors()`](https://rdrr.io/r/grDevices/palettes.html),
  [`terrain.colors()`](https://rdrr.io/r/grDevices/palettes.html) and
  [`hcl.colors()`](https://rdrr.io/r/grDevices/palettes.html).
  User-defined functions are also possible if they take the number of
  colors to be in the palette as function argument.

## Details

This is a function using basic R graphics to visualise a social contact
matrix.

## Author

Lander Willem

## Examples

``` r
if (FALSE) { # \dontrun{
data(polymod)
mij <- contact_matrix(polymod, countries = "United Kingdom", age_limits = c(0, 18, 65))$matrix
matrix_plot(mij)
} # }
```
