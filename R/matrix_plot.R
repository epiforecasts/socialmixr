#' Draws an image plot of a contact matrix with a legend strip and the numeric values in the cells.
#'
#' This function combines the R image.plot function with numeric contact rates in the matrix cells.
#'
#' @param mij a contact matrix containing contact rates between participants of age i (rows) with contacts of age j (columns). This is the default matrix format of [contact_matrix()].
#' @param min.legend the color scale minimum (default = 0). Set to NA to use the minimum value of `mij`.
#' @param max.legend the color scale maximum (default = NA). Set to NA to use the maximum value of `mij`.
#' @param num.digits the number of digits when rounding the contact rates (default = 2). Use NA to disable this.
#' @param num.colors the number of color breaks (default = 50)
#' @param main the figure title
#' @param legend.width width of the legend strip in characters. Default is 1.
#' @param legend.mar width in characters of legend margin. Default is 5.1.
#' @param legend.shrink amount to shrink the size of legend relative to the full height or width of the plot. Default is 0.9.
#' @param cex.lab size of the x and y labels (default: 1.2)
#' @param cex.axis size of the axis labels (default: 0.8)
#' @param cex.text size of the numeric values in the matrix (default: 1)
#' @param xlab a title for the x axis (default: "Age group (years)")
#' @param ylab a title for the y axis (default: "Contact age group (years)")
#' @param color.palette the color palette to use (default: [heat.colors()]). Other examples are [topo.colors()], [terrain.colors()] and [hcl.colors()]. User-defined functions are also possible if they take the number of colors to be in the palette as function argument.
#' @importFrom grDevices heat.colors
#' @importFrom graphics axis text image par
#' @details This is a function using basic R graphics to visualise a social contact matrix.
#' @export
#' @examples
#' \dontrun{
#' data(polymod)
#' mij <- contact_matrix(polymod, countries = "United Kingdom", age.limits = c(0, 18, 65))$matrix
#' matrix_plot(mij)
#' }
#' @author Lander Willem
matrix_plot <- function(mij, min.legend = 0, max.legend = NA, num.digits = 2, num.colors = 50, main, xlab, ylab, legend.width, legend.mar, legend.shrink, cex.lab, cex.axis, cex.text, color.palette = heat.colors) {
  # check function arguments
  xlab <- ifelse(!missing(xlab), xlab, "Age group (year)")
  ylab <- ifelse(!missing(ylab), ylab, "Contact age group (year)")
  main <- ifelse(!missing(main), main, "Contact rates")
  cex.lab <- ifelse(!missing(cex.lab), cex.lab, 1.2)
  cex.axis <- ifelse(!missing(cex.axis), cex.axis, 0.8)
  cex.text <- ifelse(!missing(cex.text), cex.text, 1)
  legend.width <- ifelse(!missing(legend.width), legend.width, 1)
  legend.mar <- ifelse(!missing(legend.mar), legend.mar, 5.1)
  legend.shrink <- ifelse(!missing(legend.shrink), legend.shrink, 0.9)

  # set colors
  redc <- rev(color.palette(num.colors))

  # set legend scale
  zlim <- range(pretty(mij), na.rm = TRUE)
  if (!is.na(min.legend)) zlim[1] <- min.legend
  if (!is.na(max.legend)) zlim[2] <- max.legend

  # set breaks and midpoints
  breaks <- seq(zlim[1], zlim[2], length = num.colors + 1)
  midpoints <- matrix(
    breaks[-length(breaks)] + diff(breaks) / 2,
    nrow = 1, ncol = length(breaks) - 1
  )

  # get plot region for matrix and legend based on current graphical parameters
  # note: based on layout from fields::imagePlot
  char.size <- par()$cin[1] / par()$din[1] # get text character size
  offset <- char.size * par()$mar[4] # space between legend and main plot

  # set legends' plot region
  legend_plot_region <- par()$plt
  legend_plot_region[2] <- 1 - (legend.mar * char.size)
  legend_plot_region[1] <- legend_plot_region[2] - (legend.width * char.size)

  # account for legend.shrink
  pr <- (legend_plot_region[4] - legend_plot_region[3]) *
    ((1 - legend.shrink) / 2)
  legend_plot_region[4] <- legend_plot_region[4] - pr
  legend_plot_region[3] <- legend_plot_region[3] + pr

  # set main matrix' plot region
  main_plot_region <- par()$plt
  main_plot_region[2] <- min(main_plot_region[2], legend_plot_region[1] - offset)

  # defensive check for main and legends' plot region
  dp <- legend_plot_region[2] - legend_plot_region[1]
  legend_plot_region[1] <- min(main_plot_region[2] + offset, legend_plot_region[1])
  legend_plot_region[2] <- legend_plot_region[1] + dp

  # store old graphical parameters, and initiate the ones for the main plot
  old.par <- par(no.readonly = TRUE)
  par(plt = main_plot_region)

  # add image plot
  image(mij,
    xlab = xlab,
    ylab = ylab,
    main = main,
    cex.lab = cex.lab,
    breaks = breaks,
    col = redc,
    xaxt = "n",
    yaxt = "n"
  )

  # add axis labels
  plt_ticks <- seq(0, 1, length = nrow(mij))
  axis(2, at = plt_ticks, labels = c(colnames(mij)), cex.axis = cex.axis, tick = FALSE, las = 1)
  axis(1, at = plt_ticks, labels = c(colnames(mij)), cex.axis = cex.axis, tick = FALSE)

  # add numeric values if num.digits != NA and cex.text > 0
  if (!is.na(num.digits) && !is.na(cex.text) && cex.text > 0) {
    # format results (rounding/scientific)
    if (any(max(mij, na.rm = TRUE) > 1)) {
      mij <- round(mij, digits = num.digits)
    } else {
      mij <- format(mij, digits = num.digits)
    }

    # get grid centers and add values
    e_grid <- expand.grid(plt_ticks, plt_ticks)
    text(e_grid, labels = mij, cex = cex.text)
  }

  # set graphical parameters for the legend
  par(new = TRUE, pty = "m", plt = legend_plot_region, err = -1)

  # include legend bar with axis
  image(
    x = 1:2, y = breaks, z = midpoints,
    xaxt = "n", yaxt = "n", xlab = "",
    ylab = "", col = redc,
    breaks = breaks
  )
  axis(side = 4, mgp = c(3, 1, 0), las = 2)

  # restore original graphical parameters
  par(plt = old.par$plt, err = old.par$err)
}
