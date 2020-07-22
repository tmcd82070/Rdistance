# test_hermite.expansion.r
library(Rdistance)
context("hermite.expansion")

set.seed(83828233)
x <- rnorm(1000) * 100
x <- x[0 < x & x < 100]

test_that("hermite.expansion(x, 5) returns expected warning", {
  expect_warning(hermite.expansion(x, 5), "Too many Hermite polynomial expansion terms. Only 4 used.")
})

test_that("hermite.expansion(x, 0) returns expected error", {
  expect_error(hermite.expansion(x, 0), "Number of expansions must be >= 1")
})

cos_exp_1 <- hermite.expansion(x, 1)
test_that("hermite.expansion(x, 1) returns expected object for seeded random input", {
  expect_equal(hermite.expansion(x, 1), cos_exp_1)
})

cos_exp_2 <- hermite.expansion(x, 2)
test_that("hermite.expansion(x, 2) returns expected object for seeded random input", {
  expect_equal(hermite.expansion(x, 2), cos_exp_2)
})

cos_exp_3 <- hermite.expansion(x, 3)
test_that("hermite.expansion(x, 3) returns expected object for seeded random input", {
  expect_equal(hermite.expansion(x, 3), cos_exp_3)
})

cos_exp_4 <- hermite.expansion(x, 4)
test_that("hermite.expansion(x, 4) returns expected object for seeded random input", {
  expect_equal(hermite.expansion(x, 4), cos_exp_4)
})