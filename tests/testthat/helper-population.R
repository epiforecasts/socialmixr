## Population in one-year bands: any integer age limits aggregate out of it
## exactly, so one table serves every test that needs a population.
##
## The profile is the UK age structure in 2005 (UN World Population Prospects,
## five-year bands spread evenly), so age weighting acts on a realistic shape.
test_population <- function() {
  five_year <- c(
    3453670, 3558887, 3826567, 3960166, 3906577, 3755132, 4169859, 4694734,
    4655093, 3989175, 3615150, 3902231, 3126452, 2710063, 2352113, 1964744,
    1480606, 757996, 324245, 74738, 8553
  )
  ## pad beyond the oldest band so tests asking for very high age groups
  ## still aggregate
  oldest <- length(five_year) * 5L
  data.frame(
    lower.age.limit = seq(0L, 120L),
    population = c(rep(five_year, each = 5) / 5, rep(0, 121L - oldest))
  )
}
