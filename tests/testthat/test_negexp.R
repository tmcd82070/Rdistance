library(Rdistance)
context("Negexp distance function")

# Load example sparrow data (line transect survey type)
data(sparrowDetectionData)

# Write a function that does all the testing in these sections. 
# i.e., write a function that fits a distance funciton, then 
# tests coef, ESW, AIC, print, and plot against input values.
#
# This function should take in a set of parameters, run the estimation, 
# then test coef, ESW, AIC, print and plot.  A set of all possible 
# combinations of relevant parameters is:
testInputs <- expand.grid(
  likelihood = c( "uniform", "halfnorm", "hazrate", "negexp", "Gamma"),
<<<<<<< HEAD
  pointSurvey = c(FALSE, TRUE),
=======
  pointSurvey = c(TRUE, FALSE),
>>>>>>> enforceUnits
  w.lo = c(0, 50), 
  w.hi = c(NULL, 100), 
  expansions = 0:5,
  series = c('simple', 'hermite', 'cosine'),
  x.scl = c(0, 100),
  g.x.scl = c(1, .85, "max"), 
  observer = "both",
  # observer = c("1","2","both"),
  outputUnits = c(NULL, "m", "ft"), 
  stringsAsFactors = FALSE
)

# Need to test:  presence of covariates, Point surveys

test_dfuncEstim <- function( params, 
                             expectedResults, 
                             detectDf = sparrowDetectionData, 
                             formula = dist ~ 1){
  
  for( i in 1:nrow(params) ){
    dfuncFit <- dfuncEstim(formula = formula,
                           detectionData=detectDf,
                           likelihood = params$likelihood[i],  
                           w.lo = units::as_units(params$w.lo[i], "m"),
                           w.hi = units::as_units(params$w.hi[i], "m"), 
                           pointSurvey = params$pointSurvey[i], 
                           expansions = params$expansions[i], 
                           x.scl = units::as_units(params$x.scl[i], "m"), 
<<<<<<< HEAD
                           g.x.scl = as.numeric(params$g.x.scl[i]), 
                           observer = params$observer[i], 
                           outputUnits = params$outputUnits[i])
    
    testParams <- paste0("Like=", params$likelihood[i], 
                         ", w.lo=", params$w.lo[i],
                         ", w.hi=", params$w.hi[i], 
                         ", pointSurvey=", params$pointSurvey[i],
                         ", expansions=", params$expansions[i], 
                         ", x.scl=", params$x.scl[i], 
                         ", g.x.scl=", params$g.x.scl[i], 
                         ", observer=", params$observer[i],
                         ", outputUnits=", params$outputUnits[i]
                         )

    # Basically, go down the output line by line testing. If it prints, it's good.
    test_that(paste(testParams, ": TEST Success"), {
      expect_output(print(dfuncFit), regexp = "\\nConvergence: Success\\n")
    })
    
    test_that(paste(testParams, ": TEST Like"), {
      expect_output(print(dfuncFit), regexp = paste0("\\nFunction: ", toupper(params$likelihood[i]), "\\s+\\n"))
    })
    
    test_that(paste(testParams, ": TEST g.x.scl"), {
      expect_output(print(dfuncFit), regexp = paste("=", params$g.x.scl[i]))
    })
    
    
    test_that(paste(testParams, ": TEST AICc"), {
      expect_output(print(dfuncFit), regexp = "\\nAICc: [0-9\\.]+\\n")
    })
    
    
        
=======
                           g.x.scl = params$g.x.scl[i], 
                           observer = params$observer[i], 
                           outputUnits = params$outputUnits[i])
    
    test_that("Halfnorm g(x.sclll) < 1 prints", {
      expect_output(print(dfuncFit), regexp = "= 0.8")
    })
    
    
    test_that("Halfnorm g.x.scl < 1 Sigma = 46.3587", {
      expect_equal(round(coef(dfuncFit)[["Sigma"]], 4), round(46.3587, 4))
    })
    
    
>>>>>>> enforceUnits
    test_that("Halfnorm g.x.scl < 1 ESW =  52.09004 [m]", {
      expect_equal(round(ESW(dfuncFit), 5), units::as_units( 52.09004, "m"))
    })
    
    aicRef <- c(2970.605)
    attr(aicRef, "criterion") <- "AICc"
    
    test_that("Halfnorm g.x.scl < 1 AIC = 2970.605", {
      expect_equal(round(AIC(dfuncFit), 3), aicRef)
    })
    
    test_that("Halfnorm g.x.scl < 1 prints", {
      expect_gt(length(capture_output_lines(print(dfuncFit))), 15) # > 15 lines of output
    })
    
    # Devise some method to test plots
    test_that("Halfnorm g.x.scl < 1 plots", {
      expect_silent(tmp <- plot(dfuncFit))
      expect_gt(tmp$yscl, 0)
    })
    
    test_that("Halfnorm prints", {
      expect_output(print(dfuncFit), regexp = "AICc:")
    })
  }
  
}  
  
  
<<<<<<< HEAD

=======
}
>>>>>>> enforceUnits

