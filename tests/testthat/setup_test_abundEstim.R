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


cat(crayon::green("Installed test_abundEstim function\n"))

test_abundEstim <- function( abundParams, 
                             distFunc
                             ){
  
  for( j in 1:nrow(abundParams) ){
    testParams <- abundParams[j,,drop = FALSE]
    
    if(is.na(testParams$ci)){
      ciParam <- NULL
    } else {
      ciParam <- testParams$ci
    }
    
    testContext <- paste0("Abund:", j, "/", nrow(abundParams),
                         ", Area=", testParams$area,
                         ", ci=", ciParam, 
                         ", plot.bs=", testParams$plot.bs,
                         ", showProgress=", testParams$showProgress, 
                         ", R=", testParams$R 
    )
    
    
    # context(testContext)
    
    # cat(crayon::green(paste("i=",i,testParams)))
    # cat("\n")
    
    abundEst <- abundEstim(  object = distFunc, 
                             area = testParams$area,
                             ci = ciParam, 
                             plot.bs = testParams$plot.bs,
                             showProgress = testParams$showProgress, 
                             R =  testParams$R 
    )
    
    # Check added components of output
    newComponents <- c( 
                        "estimates"
                      , "B" 
                      , "ci"
                      )


    test_that("abundComponents", {
      expect_setequal(setdiff(names(abundEst), names(distFunc)), newComponents)
    })

    # n.hat not null, not NA, and does not have units
    test_that("n.hatNotNull", {
      expect_true(!is.null(abundEst$estimates$abundance))
    })
    
    test_that("n.hatNotNA", {
      expect_true(!is.na(abundEst$estimates$abundance))
    })
    
    test_that("n.hatNoUnits", {
      expect_s3_class(abundEst$estimates$abundance, NA)
    })
    
    # Density should have units
    test_that("densityIsUnits", {
      expect_s3_class(abundEst$estimates$density, "units")
    })

    test_that("areaIsUnits", {
      expect_s3_class(abundEst$estimates$area, "units")
    })

    if( !is.points(distFunc) ){
      test_that("tran.lenIsUnits", {
        expect_s3_class(abundEst$estimates$surveyedUnits, "units")
      })
    
      test_that("tran.lenUnits", {
        expect_equal(units(abundEst$estimates$surveyedUnits), abundEst$outputUnits)
      })
    }

    test_that("areaUnits", {
      x <- units::set_units(1, abundEst$outputUnits, mode = "standard")
      expect_equal(units(abundEst$estimates$area), units(x*x))
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
      nItersConverged <- sum(!is.na(abundEst$B$abundance))
      if( nItersConverged > 0 ){
        ciLev <- paste0(100*ciParam, "% ")
        abundCI <- c( abundEst$estimates$abundance_lo
                    , abundEst$estimates$abundance_hi)
        densiCI <- c( abundEst$estimates$density_lo
                      , abundEst$estimates$density_hi)
        
        test_that("n.hatCINotMissing", {
          expect_true(all(!is.na(abundCI)))
        })
  
        test_that("densCILength2", {
          expect_length(abundCI, 2)
        })
        
        test_that("densCINotMissing", {
          expect_true(all(!is.na(densiCI)))
        })
        
        test_that("densCIPrint", {
          expect_output(summary(abundEst, maxBSFailPropForWarning = 1.0), 
                        regexp = paste0(ciLev, "CI: ", numberRegEx, " to ", numberRegEx), 
                        perl = TRUE)
        })
      }
    }
  }

}  
  
  