# test_F.maximize.g.r
library(Rdistance)

context("F.maximize.g")

data("sparrowDetectionData")
data("sparrowSiteData")
dfunc_line <- dfuncEstim(formula = dist ~ 1,
                         detectionData = sparrowDetectionData,
                         siteData = sparrowSiteData,
                         likelihood = "halfnorm", 
                         w.hi = 150)

test_that("F.maximize.g(dfunc_line) returns equivalent obj as it did previously", {
  invisible(capture.output(expect_equal_to_reference(F.maximize.g(dfunc_line), "F.maximize.g_dfunc_line.rds")))
})

# setup dfunc_line2 to fall into if case: x < w.lo
dfunc_line2 <- dfunc_line
dfunc_line2$w.lo <- 1

test_that("F.maximize.g(dfunc_line2_x_less_wlo) returns equivalent obj as it did previously", {
  invisible(capture.output(expect_equal_to_reference(F.maximize.g(dfunc_line2), "F.maximize.g_dfunc_line2_x_less_w.lo.rds")))
})

# setup dfunc_line2 to fall into if case: x.max$convergence != 0
dfunc_line2 <- dfunc_line
dfunc_line2$w.hi <- -1

test_that("F.maximize.g(dfunc_line2_neg_whi) returns error and warning", {
  invisible(capture.output(expect_warning(expect_error(F.maximize.g(dfunc_line2)))))
})
