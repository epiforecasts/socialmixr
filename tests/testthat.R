# nolint start
library(testthat)
library(socialmixr)
# nolint end

data.table::setDTthreads(1) # nolint: namespace_linter.
test_check("socialmixr")
