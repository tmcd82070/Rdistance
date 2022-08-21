# Test for warning issued when g.x.scl > 1

data(sparrowDetectionData)

test_that("g.x.scl Warning issued", {
  expect_warning(dfuncEstim(dist ~ 1
                            , detectionData = sparrowDetectionData, 
                            g.x.scl = 1.5))
})
