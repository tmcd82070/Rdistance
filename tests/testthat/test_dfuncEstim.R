library(Rdistance)
# context("dfuncEstim")

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
  pointSurvey = c(FALSE, TRUE),
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
                           w.lo = units::set_units(params$w.lo[i], "m"),
                           w.hi = units::set_units(params$w.hi[i], "m"), 
                           pointSurvey = params$pointSurvey[i], 
                           expansions = params$expansions[i], 
                           x.scl = units::set_units(params$x.scl[i], "m"), 
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

    test_that(paste(testParams, ": TEST Strip"), {
      lo <- units::set_units( units::set_units(params$w.lo[i], "m"), params$outputUnits[i], mode = "standard" )
      hi <- units::set_units( units::set_units(params$w.hi[i], "m"), params$outputUnits[i], mode = "standard" )
      tstString <- paste0("Strip: ", format(lo), " to ", format(hi))
      tstString <- gsub("[\\[\\]]", ".", tstString, perl = T)
      expect_output(print(dfuncFit), regexp = tstString)
    })

    test_that(paste(testParams, ": TEST ESW computation"), {
      expect_length(ESW(dfuncFit), 1)
    })
    
    test_that(paste(testParams, ": TEST ESW print"), {
      esw <- format(ESW(dfuncFit))
      esw <- gsub("[\\[\\]]", ".", esw, perl = T)
      if(params$pointSurvey[i]){
        tstString <- paste("Effective detection radius \\(EDR\\):", esw)
      } else {
        tstString <- paste("Effective strip width \\(ESW\\):", esw)
      }
      expect_output(print(dfuncFit), regexp = tstString)
    })
    
    test_that(paste(testParams, ": TEST Scaling"), {
      x0 <- format(units::set_units( units::set_units(params$x.scl[i], "m"),  params$outputUnits[i], mode = "standard" ))
      x0 <- gsub("[\\[\\]]", ".", x0, perl = T)
      tstString <- paste0("Scaling: g\\(", x0, "\\) = ", params$g.x.scl[i])
      expect_output(print(dfuncFit), regexp = tstString)
    })
    
    test_that(paste(testParams, ": TEST AICc computation"), {
      expect_length(AIC(dfuncFit), 1)
    })    
        
    test_that(paste(testParams, ": TEST AICc"), {
      expect_output(print(dfuncFit), regexp = "\\nAICc: [0-9\\.]+\\n")
    })
  }
    

}  
  
  


