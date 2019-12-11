##' Convert lower age limits to age groups.
##'
##' Mostly used for plot labelling
##' @param x age limits to transform
##' @param limits lower age limits; if not given, will use all limits in \code{x}
##' @return Age groups (limits separated by dashes)
##' @examples
##' limits_to_agegroups(c(0, 5, 10))
##' @export
limits_to_agegroups <- function(x, limits) {
    if (missing(limits)) limits <- unique(x)[order(unique(x))]
    limits <- limits[!is.na(limits)]
    agegroups <- c()
    if (length(limits) > 1) {
        agegroups <- vapply(seq(1, length(limits) - 1), function(y) {
            if ((limits[y+1] - 1) > limits[y]) {
                paste(limits[y], limits[y+1] - 1, sep = "-")
            } else {
                paste(limits[y])
            }
        }, "")
    }
    agegroups <- c(agegroups, paste(limits[length(limits)], "+", sep = ""))
    agegroups <- factor(agegroups, levels = agegroups)
    names(agegroups) <- limits
    return(unname(agegroups[as.character(x)]))
}

