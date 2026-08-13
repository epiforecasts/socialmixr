library(testthat)
library(socialmixr) # nolint: unused_import_linter.
library(data.table)

setDTthreads(1)
test_check("socialmixr")
