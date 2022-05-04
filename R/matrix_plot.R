##' Draws an image plot of a contact matrix with a legend strip and the numeric values in the cells.
##'
##' This function combines the R image.plot function with numeric contact rates in the matrix cells. 
##'
##' @param mij a contact matrix containing contact rates between participants of age i (rows) with contacts of age j (columns). This is the default matrix format of \code{\link{contact_matrix}}.
##' @param min.legend the color scale minimum (default = 0). Set to NA to use the minimium value of `mij`.
##' @param max.legend the color scale maximum (default = NA). Set to NA to use the maximum value of `mij`.
##' @param num.digits the number of digits when rounding the contact rates (default = 2). Use NA to disable this.
##' @param num.colors the number of color breaks (default = 50)
##' @param main the figure title
##' @param legend.width width of the legend strip in characters. Default is 1.
##' @param cex.lab size of the x and y labels (default: 1.2)
##' @param cex.axis size of the axis labels (default: 0.8) 
##' @param cex.text size of the numeric values in the matrix (default: 1)
##' @param xlab a title for the x axis (default: "Age group (years)")
##' @param ylab a title for the y axis (default: "Contact age group (years)")
##' @param color.palette the color palette to use (default: \code{\link{heat.colors}}). Other examples are \code{\link{topo.colors}}, \code{\link{terrain.colors}} and \code{\link{hcl.colors}}. User-defined functions are also possible if they take the number of colors to be in the palette as function argument.
##' @param ... further arguments to pass to \code{\link{image.plot}} 
##' @importFrom fields image.plot
##' @importFrom grDevices heat.colors
##' @importFrom graphics axis text
##' @details This is a function using the basic R graphics to make it easier for users of socialmixr to visualise social contact matrices.
##' @export
##' @examples
##' data(polymod)
##' mij = contact_matrix(polymod, countries = "United Kingdom", age.limits = c(0, 18,65))$matrix
##' matrix_plot(mij)
##' @author Lander Willem
matrix_plot <- function(mij, min.legend = 0, max.legend = NA, num.digits = 2, num.colors = 50, main, xlab, ylab, legend.width, cex.lab, cex.axis, cex.text, color.palette = heat.colors,...){

  # check funtion arguments
  xlab     <- ifelse(!missing(xlab),xlab,"Age group (year)")
  ylab     <- ifelse(!missing(ylab),ylab,"Contact age group (year)")
  main     <- ifelse(!missing(main),main,"Contact rates")
  cex.lab  <- ifelse(!missing(cex.lab),cex.lab,1.2) 
  cex.axis <- ifelse(!missing(cex.axis),cex.axis,0.8) 
  cex.text <- ifelse(!missing(cex.text),cex.text,1)
  legend.width <- ifelse(!missing(legend.width),legend.width,1)
   
  # set colors
  redc <- rev(color.palette(num.colors))
  
  # set legend scale
  legend_scale <- range(pretty(mij),na.rm=T)
  if(!is.na(min.legend)) legend_scale[1] <- min.legend
  if(!is.na(max.legend)) legend_scale[2] <- max.legend
  
  # adjust legend breaks
  breaks   <- seq(legend_scale[1],legend_scale[2],length=num.colors+1)
  
  # plot matrix
  image.plot(mij,
             col=redc,
             xlab=xlab,
             ylab=ylab, 
             main=main,
             zlim=legend_scale, 
             legend.width=legend.width,
             cex.lab=cex.lab,
             breaks = breaks,
             xaxt="n", 
             yaxt="n",
             ...)
  
  # add axis labels
  plt_ticks <- seq(0,1,length=nrow(mij))
  axis(2, at=plt_ticks, labels = c(colnames(mij)), cex.axis=cex.axis, tick = FALSE,las=1)
  axis(1, at=plt_ticks, labels = c(colnames(mij)), cex.axis=cex.axis, tick = FALSE)
  
  # add numeric values if num.digits != NA and cex.text > 0
  if(!is.na(num.digits) && !is.na(cex.text) && cex.text > 0){
    
    # format results (rounding/scientific)
    if(any(max(mij,na.rm=T)>1)){
      mij <- round(mij, digits = num.digits)
    } else{
      mij <- format(mij,digits = num.digits)
    }
    
    # get grid centers and add values
    e_grid <- expand.grid(plt_ticks,plt_ticks)
    text(e_grid, labels = mij, cex = cex.text)
  }
  
}




