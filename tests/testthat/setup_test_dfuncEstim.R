#' @title test_dfuncEstim - A function to test distance functions
#' 
#' @description A function that accepts a set of distance 
#' function parameters, estimates distance functions from them, and 
#' applies specific tests. 
#' 
#' @param dfuncParams A data frame containing parameters to 
#' \code{dfuncEstim}.  Tests are applied to distance function 
#' objects estimated from the parameter of each row of this data frame. 
#' See documentation for \code{dfuncEstim} for the list of parameters 
#' that can be used.  Names of the variables in this data frame just match
#' parameters in the function \code{dfuncEstim}.
#' 
#' @param abundParams A data frame containing parameters to 
#' \code{abundEstim}.  
#' See documentation for \code{abundEstim} for the list of parameters 
#' that can be used.  
#' 
#' @param abundDf A data frame containing abundance data (like 
#' transect length) to use 
#' for estimating abundance. Parameters are 'area', 'ci', 'R', 
#' 'plot.bs', 'showProgress' and 'bySite'. If NULL, do not 
#' estimate abundance, only estimate (and test) distance function.
#' 
#' @param detectDf A data frame containing distance data to use 
#' for estimating distance functions. 
#' 
#' @param formula A formula object giving the distance function 
#' to be estimated from each row of parameters.
#' 
#' @return NULL.  This function is run for it's testing side effects. 
#' 
#' @examples 
#' 
#' p <- data.frame(likelihood = c("halfnorm", "hazrate"), 
#'                 w.lo = c(0, 100))
#' test_dfuncEstim( p )               


# Notes: Need to test:  presence of covariates, Point surveys

cat(crayon::bgYellow("Installed test_dfuncEstim function\n"))

test_dfuncEstim <- function( detectParams,
                             abundParams = NULL,
                             detectDf = sparrowDetectionData, 
                             abundDf = sparrowSiteData,
                             formula = dist ~ 1){
  
  for( i in 1:nrow(detectParams) ){
    testParams <- paste0("D:", i, "/", nrow(detectParams),
                         ", Like=", detectParams$likelihood[i], 
                         ", w.lo=", detectParams$w.lo[i],
                         ", w.hi=", detectParams$w.hi[i], 
                         ", pointSurvey=", detectParams$pointSurvey[i],
                         ", expansions=", detectParams$expansions[i], 
                         ", x.scl=", detectParams$x.scl[i], 
                         ", g.x.scl=", detectParams$g.x.scl[i], 
                         ", observer=", detectParams$observer[i],
                         ", outputUnits=", detectParams$outputUnits[i]
    )
    
    
    # context(testParams)
    
    # cat(crayon::green(paste("i=",i,testParams)))
    # cat("\n")
    # ---- Get units of responses for w.lo, w.hi, and x.scl ----
    formulaVars <- all.vars(formula)
    responseLoc <- attr(terms(formula), "response")
    responseVar <- formulaVars[responseLoc]
    outUnits <- detectParams$outputUnits[i]
    
    # ---- x.scl param ----
    if( detectParams$x.scl[i] == "max" ){
      param.x.scl = "max"
    } else {
      param.x.scl = units::set_units(as.numeric(detectParams$x.scl[i]), 
                                     units(detectDf[,responseVar]), 
                                     mode = "standard")
      param.x.scl.output <- units::set_units(param.x.scl
                                         , detectParams$outputUnits[i]
                                         , mode = "standard")
    }
    
    # ---- w.lo param ----
    param.w.lo = units::set_units(as.numeric(detectParams$w.lo[i]),
                                  units(detectDf[,responseVar]), 
                                  mode = "standard")
    param.w.lo.output <- units::set_units(param.w.lo
                                        , detectParams$outputUnits[i]
                                        , mode = "standard")
    
    # ---- w.hi param ----
    if( is.na(detectParams$w.hi[i]) ){
      param.w.hi <- NULL
      param.w.hi.output <- max(detectDf[,responseVar], na.rm=TRUE)
    } else {
      param.w.hi = units::set_units(detectParams$w.hi[i], 
                                    units(detectDf[,responseVar]), 
                                    mode = "standard")
      param.w.hi.output <- param.w.hi
    }
    param.w.hi.output <- units::set_units(param.w.hi.output
                                          , detectParams$outputUnits[i]
                                          , mode = "standard")
    
    # ---- param.x.scl < param.w.lo Warning ----
    if( !is.character(param.x.scl) ){
      if( param.x.scl < param.w.lo ){
        # could run this case in separate "warnings" test file
        test_that(paste(testParams,"x.scl less than w.lo warning", sep=";")
          , {expect_warning(
            dfuncEstim(formula = formula
                       , detectionData=detectDf
                       , siteData = abundDf
                       , likelihood = detectParams$likelihood[i]
                       , w.lo = param.w.lo
                       , w.hi = param.w.hi
                       , pointSurvey = detectParams$pointSurvey[i]
                       , expansions = detectParams$expansions[i]
                       , series = detectParams$series[i]
                       , x.scl = param.x.scl
                       , g.x.scl = as.numeric(detectParams$g.x.scl[i])
                       , observer = detectParams$observer[i]
                       , outputUnits = detectParams$outputUnits[i]
            )
            , regexp = "x.scl is less than specified lower limit"
          )
        })
        param.x.scl <- param.w.lo
        param.x.scl.output <- param.w.lo.output
      }
    }
    
    # ---- Fit the distance function ----
    # Beware of warnings; Errors should fail.
    dfuncFit <- suppressWarnings(
      dfuncEstim(formula = formula
                 , detectionData=detectDf
                 , siteData = abundDf
                 , likelihood = detectParams$likelihood[i]
                 , w.lo = param.w.lo
                 , w.hi = param.w.hi
                 , pointSurvey = detectParams$pointSurvey[i]
                 , expansions = detectParams$expansions[i]
                 , series = detectParams$series[i]
                 , x.scl = param.x.scl
                 , g.x.scl = as.numeric(detectParams$g.x.scl[i])
                 , observer = detectParams$observer[i]
                 , outputUnits = detectParams$outputUnits[i]
        )
      )
    
    # print(testParams)
    # print(tmp)
    # print("----")
    # 
    # if( inherits(tmp, "warning") | inherits(tmp, "error") ){
    #   # I cannot seem to trap the warning thrown by dfuncEstim AND return the 
    #   # fitted object outside test_that. So, when an error or warning is thrown, 
    #   # refit under "expect warning" which returns the fitted object. 
    #   # This is inefficient because same fit happens twice in this case.
    #   dfuncFit <- expect_warning( 
    #                 dfuncEstim(formula = formula
    #                          , detectionData=detectDf
    #                          , siteData = abundDf
    #                          , likelihood = detectParams$likelihood[i]
    #                          , w.lo = param.w.lo
    #                          , w.hi = param.w.hi
    #                          , pointSurvey = detectParams$pointSurvey[i]
    #                          , expansions = detectParams$expansions[i]
    #                          , series = detectParams$series[i]
    #                          , x.scl = param.x.scl
    #                          , g.x.scl = as.numeric(detectParams$g.x.scl[i])
    #                          , observer = detectParams$observer[i]
    #                          , outputUnits = detectParams$outputUnits[i]
    #                          )
    #                 )
    #   cat("Refitted dfuncFit:\n")
    #   print(dfuncFit)
    # } else {
    #   # no warning first time
    #   dfuncFit <- tmp
    # }

    
    # ---- Tests ----
    # Now that we have the fitted object, go down the output 
    # line by line testing. If it prints, it's good.
    
    if( is.na(detectParams$w.hi[i]) ){
      param.w.hi <- dfuncFit$w.hi
    }
    
    if( length(dfuncFit$convergence) == 0 ){
      print(dfuncFit$fit)
      print(names(dfuncFit))
    }
    
    if( dfuncFit$convergence == 0 ) {
      vcDiag <- diag(dfuncFit$varcovar)
      if( any(is.na(vcDiag)) | any(vcDiag < 0.0)) {
        convergemess <- "(singular variance-covariance matrix)"
      } else {
        convergemess <- "Success"
      }
    } else {
      convergemess <- dfuncFit$fit$message
      convergemess <- gsub("\\(", "\\\\(", convergemess)
      convergemess <- gsub("\\)", "\\\\)", convergemess)
    }
    
    test_that(paste(testParams,"Convergence message prints", sep=";"), {
      expect_output(print(dfuncFit), regexp = convergemess)
    })
  
    test_that(paste(testParams,"Like prints", sep=";"), {
      if( detectParams$expansions[i] == 0 ){
        tstString <- paste0("\\nFunction: ", toupper(detectParams$likelihood[i]), "\\s+\\n")
      } else {
        tstString <- paste0("\\nFunction: ", toupper(detectParams$likelihood[i])
                          , " with "
                          , detectParams$expansions[i]
                          , " expansion.+"
                          , toupper(detectParams$series[i])
                          )
      }
      expect_output(print(dfuncFit), regexp = tstString)
    })

    test_that(paste(testParams,"Strip", sep=";"), {
      tstString <- paste0("Strip: ", format(param.w.lo.output), " to ", format(param.w.hi.output))
      tstString <- gsub("[\\[\\]]", ".", tstString, perl = T)
      expect_output(print(dfuncFit), regexp = tstString)
    })

    if( !is.null(dfuncFit$covars) ){
      # The difference between stored covars and stored model frame is 
      # that covars only has distances inside the strip
      test_that(paste(testParams,"Size Covars <= size Model.frame", sep=";"), {
        nOutStrip <- sum( 
          (dfuncFit$w.lo > model.response(dfuncFit$model.frame)) | 
          (model.response(dfuncFit$model.frame) > dfuncFit$w.hi)
          )
        expect_equal(nrow(dfuncFit$covars) + nOutStrip, nrow(dfuncFit$model.frame))
      })
    }
    
    # test effectiveDistance runs first, then re-run and store it
    # Would be better to assign returned effectiveDistance to correct 
    # frame, but these run fast so okay to re-run
    test_that(paste(testParams,"Effective distance computes", sep=";"), {
      if( is.null(dfuncFit$covars) ){
        nED <- 1
      } else {
        nED <- nrow(dfuncFit$detections)
      }
      expect_length(effectiveDistance(dfuncFit), nED)
    })
    
    efd <- effectiveDistance(dfuncFit)

    test_that(paste(testParams,"Effective distance(s) not missing", sep=";"),{
      expect_true( all( !is.na(efd) & !is.nan(efd) & !is.infinite(efd) ), 
                   label = "One or more EFD are NA, Nan, or Inf")
    })
        
    test_that(paste(testParams,"Effective distance(s) > 0", sep=";"), {
      zero <- units::set_units(0, dfuncFit$outputUnits, mode = "standard")
      expect_true( all( efd > zero), label = "All EFDs > 0" )
    })

    nominalW <- param.w.hi - param.w.lo
    if( (inherits(param.x.scl, "units") && param.x.scl == param.w.lo) || is.character(param.x.scl) ){
      # if param.x.scl != param.w.lo & param.x.scl != "max", we 
      # don't know the range of effective distance because g(x) could be > 1 for some x.
      # Don't test in this case.  Warnings and red text are printed and plotted.
      test_that(paste(testParams,"Effective distance <= w.hi-w.lo", sep=";"), {
        expect_true( all(efd <= nominalW), label = "All EFDs <= W" )
      })
    }
    
    test_that(paste(testParams,"Effective distance prints", sep=";"), {
      efdString <- format(mean(efd))
      efdString <- gsub("[\\[\\]]", ".", efdString, perl = T)
      efdString <- gsub("\\+", "\\\\+", efdString, perl = T) # incase of value like 4.234e+10
      if(detectParams$pointSurvey[i]){
        tstString <- paste("[E|e]ffective detection radius \\(EDR\\):", efdString)
      } else {
        tstString <- paste("[E|e]ffective strip width \\(ESW\\):", efdString)
      }
      if(!is.null(dfuncFit$covars)){
        tstString <- paste("Average", tstString)
      }
      expect_output(print(dfuncFit), regexp = tstString)
    })
    
    if( any(efd > nominalW) ){
      # check that red text is printed
      tstString <- "<- One or more"
      expect_output(print(dfuncFit), regexp = tstString, label = "Red text re Pr()>1 did not print")
    }
    
    test_that(paste(testParams,"Scaling prints", sep=";"), {
      if( !is.character(param.x.scl) ){
        x0 <- format(param.x.scl.output)
      } else {
        x0 <- format(dfuncFit$x.scl)
      }
      x0 <- gsub("[\\[\\]]", ".", x0, perl = T)
      tstString <- paste0("Scaling: g\\(", x0, "\\) = ", detectParams$g.x.scl[i])
      expect_output(print(dfuncFit), regexp = tstString)
    })
    
    # Same deal as effectiveDistance: test first, then rerun to compute
    test_that(paste(testParams,"AICc computes", sep=";"), {
      expect_length(AIC(dfuncFit), 1)
    })
    aic <- AIC(dfuncFit)
        
    test_that(paste(testParams,"AICc prints", sep=";"), {
      expect_output(print(dfuncFit), regexp = paste0("\\nAICc: ", format(aic)))
    })

    test_that(paste(testParams,"BIC prints", sep=";"), {
      bic <- AIC(dfuncFit, criterion = "BIC")
      expect_output(print(dfuncFit, criterion = "BIC"), regexp = paste0("\\nBIC: ", format(bic)))
    })
    
    # ---- Test abundance methods for these parameters ----
    if(!is.null(abundParams)){
      test_abundEstim(abundParams = abundParams, 
                 dfuncDf = detectDf, 
                 abundDf = abundDf,
                 distFunc = dfuncFit)
    }
      
  }

}  
  
  