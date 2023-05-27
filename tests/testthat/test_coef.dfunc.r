# test_coef.dfunc.r
library(Rdistance)
# context("coef.dfunc")

# Load example sparrow data (line transect survey type)
data(sparrowDetectionData)

# Fit half-normal detection function
non_density_case <- dfuncEstim(formula=dist~1
                             , detectionData = sparrowDetectionData
                             , w.hi = units::set_units(100, "m")
                             )

# Fit smoothed detection function
density_case <- dfuncSmu(dist~1
                       , sparrowDetectionData
                       , w.hi=units::set_units(150, "m")
                       )

test_that("input=non_density_case outputs=46.3586986528704", {
  expect_equal(coef(non_density_case)[["Sigma"]], 46.3586986528704)
})

test_that("input=density_case outputs=NULL", {
  expect_equal(coef(density_case), NULL)
})
