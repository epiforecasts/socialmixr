## Package logo: the POLYMOD UK contact matrix in five-year age groups, drawn as
## a viridis mosaic inside the standard R hexagon. The five-year resolution
## keeps both signatures of an empirical contact matrix visible: the diagonal
## of within-age-group contacts and the parent-child wings either side of it.
##
## Writes data-raw/logo.svg. The rendered logo used by the package is produced
## from it with, from the package root:
##
##   inkscape --export-type=png --export-filename=man/figures/logo.png \
##     --export-width=240 --export-background-opacity=0 data-raw/logo.svg
##
## Any other SVG renderer (rsvg-convert, ImageMagick) works as well. The
## wordmark is set in Fira Sans, falling back to any sans-serif face.

library(socialmixr)

age_limits <- seq(0, 70, by = 5)

## UN World Population Prospects estimates for the United Kingdom in 2005, when
## POLYMOD was conducted, in five-year age groups with 70+ aggregated
uk_pop <- data.frame(
  age = limits_to_age_groups(age_limits, notation = "brackets"),
  population = c(
    3453670, 3558887, 3826567, 3960166, 3906577, 3755132, 4169859, 4694734,
    4655093, 3989175, 3615150, 3902231, 3126452, 2710063, 6962995
  )
)

result <- polymod[country == "United Kingdom"] |>
  assign_age_groups(age_limits = age_limits) |>
  compute_matrix()
mat <- symmetrise(result, survey_pop = align_ages(uk_pop, result))$matrix
n <- nrow(mat)

## viridis, sampled at ten points
viridis <- c(
  "#440154", "#482878", "#3E4A89", "#31688E", "#26828E",
  "#1F9E89", "#35B779", "#6DCD59", "#B4DE2C", "#FDE725"
)
ramp <- colorRamp(viridis, space = "Lab")
## capped at 2 contacts so the parent-child wings register as clearly as the
## much larger school-age peak on the diagonal
scaled <- pmin(as.vector(mat), 2)
level <- (scaled - min(scaled)) / (max(scaled) - min(scaled))
cols <- matrix(rgb(ramp(level), maxColorValue = 255), n, n)

## standard R hex sticker proportions (43.9 x 50.8 mm), pointy top
width <- 1200
height <- width * 2 / sqrt(3)
cx <- width / 2
cy <- height / 2

hexagon <- function(scale = 1) {
  x <- cx + c(0, 1, 1, 0, -1, -1) * width / 2 * scale
  y <- cy + c(-2, -1, 1, 2, 1, -1) / 2 * height / 2 * scale
  paste0("M", paste(sprintf("%.2f,%.2f", x, y), collapse = "L"), "Z")
}

side <- 730
gap <- 5
cell <- side / n
x0 <- cx - side / 2
y0 <- cy - side / 2 - 80
tiles <- unlist(lapply(seq_len(n), function(i) {
  sprintf(
    '<rect x="%.2f" y="%.2f" width="%.2f" height="%.2f" rx="3" fill="%s"/>',
    x0 + (seq_len(n) - 1) * cell + gap / 2, y0 + (i - 1) * cell + gap / 2,
    cell - gap, cell - gap, cols[i, ]
  )
}))

svg <- c(
  sprintf(
    paste0(
      '<svg xmlns="http://www.w3.org/2000/svg" width="%.0f" height="%.0f" ',
      'viewBox="0 0 %.0f %.0f">'
    ),
    width, height, width, height
  ),
  sprintf('<path d="%s" fill="#f7f7f5"/>', hexagon()),
  tiles,
  sprintf(
    '<path d="%s" fill="none" stroke="#31688E" stroke-width="26"/>',
    hexagon(0.985)
  ),
  sprintf(
    paste0(
      '<text x="%.0f" y="%.0f" text-anchor="middle" font-size="132" ',
      'font-weight="600" fill="#1b2430" ',
      'font-family="Fira Sans, DejaVu Sans, sans-serif">socialmixr</text>'
    ),
    cx, height - 250
  ),
  "</svg>"
)

writeLines(svg, file.path("data-raw", "logo.svg"))
