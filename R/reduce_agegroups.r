##' Reduce the number of age groups given a broader set of limits
##'
##' Operates on lower limits
##' @param x vector of limits
##' @param limits new limits
##' @return vector with the new age groups
##' @examples
##' reduce_agegroups(seq_len(20), c(0, 5, 10))
##' @export
reduce_agegroups <- function(x, limits) {
    return(limits[findInterval(x, limits)])
}

