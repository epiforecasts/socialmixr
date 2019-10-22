#' Social contact data from 8 European countries
#'
#' A dataset containing social mixing diary data from 8 European countries:
#' Belgium, Germany, Finland, Great Britain, Italy, Luxembourg,
#' The Netherlands and Poland.
#' The Data are fully described in Mossong J, Hens N, Jit M, Beutels P, Auranen
#' K, Mikolajczyk R, et al. (2008) Social Contacts and Mixing Patterns Relevant
#' to the Spread of Infectious Diseases. PLoS Med 5(3): e74.
#'
#' @format A list of two data frames:
#' \describe{
#'   \item{participants}{the study participant, with age, country, year and day
#'   of the week (starting with 1 = Monday)}
#'   \item{contacts}{reported contacts of the study participants. The variable
#'   phys_contact has two levels (1 denotes physical contact while 2 denotes
#'   non-physical contact), duration_multi has five levels (1 is less than 5
#'   minutes while 5 is more than 4 hours, increasing in the order found in
#'   Figure 1 in Mossong et al.), and frequency_multi has five levels (1 is
#'   daily, 2 is weekly, 3 is montly, 4 is less often, and 5 is first time)}
#'   All other variables are described on the Zenodo repository of the data,
#'   available at https://doi.org/10.5281/zenodo.1043437
#' }
#' @source \url{http://dx.doi.org/10.1371/journal.pmed.0050074}
"polymod"
