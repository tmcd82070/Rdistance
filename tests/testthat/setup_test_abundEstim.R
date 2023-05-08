#' @title test_abund - A function to test abundance estimation 
#' from distance functions
#' 
#' @description A function that accepts a distance function 
#' object and a set of abundance 
#' parameters, estimates abundance, and 
#' applies specific tests. 
#' 
#' @param params A data frame containing parameters to 
#' \code{abundEstim}.  
#' See documentation for \code{dfuncEstim} for the list of parameters 
#' that can be used.  
#' 
#' @param detectDf A data frame containing distance data. 
#' Need this for groupsize and (eventually) detection level 
#' covariates. 
#' 
#' @param abundDf A data frame containing abundance data (like 
#' transect length) to use 
#' for estimating abundance. Parameters are 'area', 'ci', 'R', 
#' 'plot.bs', 'showProgress' and 'bySite'.
#' 
#' @param distFunc An estimated distance function object from 
#' \code{dfuncEstim}.
#' 
#' @return NULL.  This function is run for it's testing side effects. 
#' 
#' @examples 
#' 
#' p <- data.frame(likelihood = c("halfnorm", "hazrate"), 
#'                 w.lo = c(0, 100))
#' test_dfuncEstim( p )               


cat(crayon::bgYellow("Installed test_abundEstim function\n"))

test_abundEstim <- function( abundParams, 
                             distFunc,
                             dfuncDf = sparrowDetectionData, 
                             abundDf = sparrowSiteData
                             ){
  
  for( j in 1:nrow(abundParams) ){
    testParams <- abundParams[j,,drop = FALSE]
    
    if(is.na(testParams$ci)){
      ciParam <- NULL
    } else {
      ciParam <- testParams$ci
    }
    
    testContext <- paste0("  A:", j, "/", nrow(abundParams),
                         ", Area=", testParams$area,
                         ", ci=", ciParam, 
                         ", plot.bs=", testParams$plot.bs,
                         ", showProgress=", testParams$showProgress, 
                         ", bySite=", testParams$bySite,
                         ", R=", testParams$R 
    )
    
    
    # context(testContext)
    
    # cat(crayon::green(paste("i=",i,testParams)))
    # cat("\n")
    
    abundEst <- abundEstim(dfunc = distFunc, 
                             detectionData = dfuncDf, 
                             siteData = abundDf,
                             area = testParams$area,
                             ci = ciParam, 
                             plot.bs = testParams$plot.bs,
                             showProgress = testParams$showProgress, 
                             bySite = testParams$bySite,
                             R =  testParams$R, 
                             control = RdistanceControls()
    )
    
    # Check added components of output
    if(distFunc$pointSurvey){
      newComponents <- c( "density"
                        , "n.hat"
                        , "n"
                        , "area"
                        , "surveyedUnits"
                        , "avg.group.size"
                        , "effDistance" )
    } else {
      newComponents <- c( "density"
                          , "n.hat"
                          , "n"
                          , "area"
                          , "surveyedUnits"
                          , "avg.group.size"
                          , "effDistance" )
    }
    
    if(!is.null(ciParam)){
      newComponents <- c(newComponents, 
                         c("n.hat.ci"
                           , "density.ci"
                           , "effDistance.ci"
                           , "B"
                           , "nItersConverged"
                           , "alpha" ))
    }
    
    test_that("abundComponents", {
      expect_setequal(setdiff(names(abundEst), names(distFunc)), newComponents)
    })

    # n.hat not null, not NA, and does not have units
    test_that("n.hatNotNull", {
      expect_true(!is.null(abundEst$n.hat))
    })
    
    test_that("n.hatNotNA", {
      expect_true(!is.na(abundEst$n.hat))
    })
    
    test_that("n.hatNoUnits", {
      expect_s3_class(abundEst$n.hat, NA)
    })
    
    # Density should have units
    test_that("densityIsUnits", {
      expect_s3_class(abundEst$density, "units")
    })

    test_that("areaIsUnits", {
      expect_s3_class(abundEst$area, "units")
    })

    if( !distFunc$pointSurvey ){
      test_that("tran.lenIsUnits", {
        expect_s3_class(abundEst$surveyedUnits, "units")
      })
    
      test_that("tran.lenUnits", {
        expect_equal(units(abundEst$surveyedUnits), abundEst$outputUnits)
      })
    }

    test_that("areaUnits", {
      x <- units::set_units(1, abundEst$outputUnits, mode = "standard")
      expect_equal(units(abundEst$area), units(x*x))
    })
    
    numberRegEx <- "[\\d\\.\\-\\+e]+"
    
    test_that("densityPrint", {
      expect_output(print(abundEst, maxBSFailPropForWarning = 1.0), 
                    regexp = paste0("Density in sampled area: ", numberRegEx),
                    perl = TRUE)
    })

    test_that("n.hatPrint", {
      expect_output(print(abundEst, maxBSFailPropForWarning = 1.0), 
                    regexp = paste0("Abundance in ", numberRegEx, " \\[.+\\] study area: ", numberRegEx), 
                    perl = TRUE)
    })

    # Confidence interval tests
    if(!is.null(ciParam)){
      if( abundEst$nItersConverged > 0 ){
        ciLev <- paste0(100*ciParam, "% ")
        
        test_that("n.hatCILength2", {
          expect_length(abundEst$n.hat.ci, 2)
        })
        
        test_that("n.hatCINotMissing", {
          expect_true(all(!is.na(abundEst$n.hat.ci)))
        })
  
        test_that("densCILength2", {
          expect_length(abundEst$density.ci, 2)
        })
        
        test_that("densCINotMissing", {
          expect_true(all(!is.na(abundEst$density.ci)))
        })
        
        test_that("densCIPrint", {
          expect_output(print(abundEst, maxBSFailPropForWarning = 1.0), 
                        regexp = paste0(ciLev, "CI: ", numberRegEx, " to ", numberRegEx), 
                        perl = TRUE)
        })
      }
    }
  }

}  
  
  