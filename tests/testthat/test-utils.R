library(testthat)

test_that("Test index_of for list", {
  f1 <- function(x) x
  f2 <- function(x) x+1
  f3 <- function(x) x+2
  expect_equal(index_of(list(f1, f2, f3), f2), 2)
  expect_equal(index_of(list(f1, f2, f3), f1), 1)
  expect_equal(index_of(list(f1, f2, f3), f3), 3)

  expect_equal(index_of(list(1, 2, 3), 3), 3)
  expect_equal(index_of(list("A", "B", "C"), "B"), 2)

  expect_equal(index_of(list(), "B"), -1)
})
