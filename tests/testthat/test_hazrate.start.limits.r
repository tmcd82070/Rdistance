# hazrate.start.limits

sparrowDf <- Rdistance::RdistDf(sparrowSiteData
                                , sparrowDetectionData
                                , by = NULL
                                , pointSurvey = FALSE
                                , observer = "single"
                                , .detectionCol = "detections")

ml <- Rdistance::parseModel(sparrowDf
                            , formula = dist ~ 1
                            , likelihood = "hazrate"
                            , w.lo = 0
                            , w.hi = NULL
                            , series = "cosine"
                            , x.scl = 0
                            , g.x.scl = 1
                            , outputUnits = "m"
                            )

sl <- Rdistance::startLimits(ml)

test_that("hazrate.start.limits returns type=list", {
  expect_type(sl, "list")
})

test_that("hazrate.start.limits returns list of length 4", {
  expect_length(sl, 4)
})

slen <- c(
    start = 2
  , low = 2
  , high = 2
  , names = 2
)
test_that("hazrate.start.limits returns 4 lists of length 2", {
  expect_equal(lengths(sl), slen)
})

# With covariates ----

ml <- Rdistance::parseModel(sparrowDf
                            , formula = dist ~ observer + bare
                            , likelihood = "hazrate"
                            , w.lo = 0
                            , w.hi = NULL
                            , expansions = 0
                            , series = "cosine"
                            , x.scl = 0
                            , g.x.scl = 1
                            , outputUnits = "m"
)

sl <- Rdistance::startLimits(ml)

test_that("hazrate.start.limits + covars returns type=list", {
  expect_type(startLimits(ml), "list")
})

np <- 2 + 4 + 1 + 0 # NativeParms + obsCoef + bareCoef + expans
slen <- c(
  start = np
  , low = np
  , high = np
  , names = np
)
test_that("hazrate.start.limits + covars returns 4 lists of length 6", {
  expect_equal(lengths(sl), slen)
})

nms <- c("(Intercept)"
         , "observerobs2", "observerobs3", "observerobs4", "observerobs5"
         , "bare", "k")
test_that("check names of hazrate.start.limits + covars starts", {
  expect_equal(names(sl$start), nms)
})
test_that("check names of hazrate.start.limits + covars", {
  expect_equal(sl$names, nms)
})

# With covariates and expansions ----

ml <- Rdistance::parseModel(sparrowDf
                            , formula = dist ~ observer + bare
                            , likelihood = "hazrate"
                            , w.lo = 0
                            , w.hi = NULL
                            , expansions = 2
                            , series = "cosine"
                            , x.scl = 0
                            , g.x.scl = 1
                            , outputUnits = "m"
)

sl <- Rdistance::startLimits(ml)

test_that("hazrate.start.limits+covars+expans returns type=list", {
  expect_type(sl, "list")
})

np <- 2 + 4 + 1 + 2 # NativeParam + obsCoef + bareCoef + expans
slen <- c(
  start = np
  , low = np
  , high = np
  , names = np
)
test_that("hazrate.start.limits+covars+expans has 4 lists of right length", {
  expect_equal(lengths(sl), slen)
})

nms <- c("(Intercept)"
         , "observerobs2", "observerobs3", "observerobs4", "observerobs5"
         , "bare"
         , "k"
         , "a1", "a2")
test_that("check names of hazrate.start.limits + covars starts", {
  expect_equal(names(sl$start), nms)
})
test_that("check names of hazrate.start.limits + covars", {
  expect_equal(sl$names, nms)
})


# Check no missings ----

sl <- do.call(rbind, sl[1:3])
nmiss <- data.frame(sl) |> 
  dplyr::summarise(dplyr::across(dplyr::everything()
                                 , .fns = ~ sum(is.na(.x))))
nmissSum <- sum(nmiss)
if(nmissSum > 0){
  cat("Number of missings in HAZRATE start limits:\n")
  print(nmiss)
}
test_that("hazrate.start.limits does not throw NA's", {
  expect_equal(nmissSum, 0)
})

