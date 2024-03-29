% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/limits_to_agegroups.r
\name{limits_to_agegroups}
\alias{limits_to_agegroups}
\title{Convert lower age limits to age groups.}
\usage{
limits_to_agegroups(x, limits = sort(unique(x)))
}
\arguments{
\item{x}{age limits to transform}

\item{limits}{lower age limits; if not given, will use all limits in \code{x}}
}
\value{
Age groups (limits separated by dashes)
}
\description{
Mostly used for plot labelling
}
\examples{
limits_to_agegroups(c(0, 5, 10))
}
