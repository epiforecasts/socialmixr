## Population in one-year bands, for tests that ask for a symmetric, split,
## per-capita or age-weighted matrix. Any integer age limits aggregate out of
## it exactly, so one table serves every test.
##
## The profile is the UK age structure in 2005 (UN World Population Prospects,
## five-year bands spread evenly within each band), so that age weighting acts
## on something with the shape of a real population.
test_population <- function() {
  five_year <- c(
    3453670, 3558887, 3826567, 3960166, 3906577, 3755132, 4169859, 4694734,
    4655093, 3989175, 3615150, 3902231, 3126452, 2710063, 2352113, 1964744,
    1480606, 757996, 324245, 74738, 8553
  )
  data.frame(
    lower.age.limit = seq(0L, length(five_year) * 5L - 1L),
    population = rep(five_year, each = 5) / 5
  )
}
