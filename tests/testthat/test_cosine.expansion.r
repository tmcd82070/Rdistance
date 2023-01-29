# test_cosine.expansion.r
library(Rdistance)
# context("cosine.expansion")

set.seed(33328)
x <- rnorm(1000) * 100
x <- x[ 0 < x & x < 100 ]

test_that("cosine.expansion(x, 6) returns expected warning", {
  expect_warning(cosine.expansion(x, 6), "Too many Cosine polynomial expansion terms. Only 5 used.")
})

test_that("cosine.expansion(x, 0) returns expected error", {
  expect_error(cosine.expansion(x, 0), "Number of expansions must be >= 1")
})

cos_exp_2 <- cosine.expansion(x, 2)
test_that("cosine.expansion(x, 2) returns expected object for seeded random input", {
  expect_equal(cosine.expansion(x, 2), cos_exp_2)
})

cos_exp_3 <- cosine.expansion(x, 3)
test_that("cosine.expansion(x, 3) returns expected object for seeded random input", {
  expect_equal(cosine.expansion(x, 3), cos_exp_3)
})

cos_exp_4 <- cosine.expansion(x, 4)
test_that("cosine.expansion(x, 4) returns expected object for seeded random input", {
  expect_equal(cosine.expansion(x, 4), cos_exp_4)
})

cos_exp_5 <- cosine.expansion(x, 5)
test_that("cosine.expansion(x, 5) returns expected object for seeded random input", {
  expect_equal(cosine.expansion(x, 5), cos_exp_5)
})









