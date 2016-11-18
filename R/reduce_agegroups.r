##' Reduce the number of age groups given a broader set of limtis
##'
##' Operates on lower limits
##' @param x vector of limits
##' @param limits new limits
##' @return vector with the new agegroups
##' @export
reduce_agegroups <- function(x, limits) {
    return(limits[findInterval(x, limits)])
}

