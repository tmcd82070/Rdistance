# test_simple.expansion.r
library(Rdistance)
context("simple.expansion")

set.seed(883839)
x <- rnorm(1000) * 100
x <- x[ 0 < x & x < 100 ]

# tests for cases where expansion input lies outside of constraints
test_that("expansion > 4 results in expected warning", {
  expect_warning(simple.expansion(x, 5), "Too many Simple polynomial expansion terms. Only 4 used.")
})

test_that("expansion < 1 results in expected error", {
  expect_error(simple.expansion(x, 0.9), "Number of expansions must be >= 1") 
})

# generate expected results for dataset with given input
exp_1.5 <- simple.expansion(x, 1.5)
exp_2 <- simple.expansion(x, 2)
exp_3 <- simple.expansion(x, 3)
exp_4 <- simple.expansion(x, 4)

test_that("expansion of 1.5 generates expected results", {
  expect_equal(simple.expansion(x, 1.5), exp_1.5)
})

test_that("expansion of 2 generates expected results", {
  expect_equal(simple.expansion(x, 2), exp_2)
})

test_that("expansion of 3 generates expected results", {
  expect_equal(simple.expansion(x, 3), exp_3)
})

test_that("expansion of 4 generates expected results", {
  expect_equal(simple.expansion(x, 4), exp_4)
})