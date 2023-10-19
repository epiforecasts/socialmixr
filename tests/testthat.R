# nolint start
library(testthat)
library(socialmixr)
# nolint end

data.table::setDTthreads(1)
test_check("socialmixr")
