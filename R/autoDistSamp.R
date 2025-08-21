#' @title Automated classical distance analysis
#' 
#' @description Perform automated likelihood, expansion, and series
#' selection for a classic distance sampling analysis.  Estimate 
#' abundance using the best fitting likelihood, expansion, and series. 
#' 
#' @inheritParams dE.single
#' @inheritParams abundEstim
#' @inheritParams dfuncEstim
#' 
#' @param plot Logical scalar specifying whether to plot models during model selection. 
#'   If \code{TRUE}, a histogram with fitted distance function is plotted for every model. 
#'   The function pauses between each plot and prompts the user for whether they want to continue. 
#'   To suppress user prompts, set \code{plot} = \code{FALSE}.
#'   
#' @param criterion A string specifying the criterion to use when assessing model fit.   
#' The best fitting model, as defined by this routine, has the lowest value 
#' of this criterion.  This must be one of "AICc" (the default), 
#' "AIC", or "BIC".  See \code{\link{AIC.dfunc}} for formulas. 
#'   
#' @param likelihoods String vector specifying the likelihoods to fit. 
#' See 'likelihood' parameter of \code{\link{dfuncEstim}}.
#'   
#' @details During distance function selection, all combinations of likelihoods, series, and 
#' number of expansions is fitted. For example, if \code{likelihoods} has 3 elements, 
#' \code{series} has 2 elements, and \code{expansions} has 4 elements, 
#' this routine fits a total of 3 (likelihoods) * 2 (series) * 4 (expansions) 
#' = 24 models.  Default parameters fit 9 detection functions, i.e.,  
#' all combinations of "halfnorm", "hazrate", and "negexp" likelihoods 
#' and 0 through 3 expansions. Other combinations are specified through 
#' values of \code{likelihoods}, \code{series}, and \code{expansions}. 
#' 
#' Suppress all intermediate output using \code{plot.bs=FALSE}, 
#' \code{showProgress=FALSE}, and \code{plot=FALSE}. 
#' 
#' The returned abundance estimate object contains
#' an additional component, the fitting table (a list of models fitted and 
#' criterion values) in component \code{$fitTable}.
#' 
#' @inherit abundEstim return
#' 
#' @seealso \code{\link{dfuncEstim}}, \code{\link{abundEstim}}.
#' @examples 
#' # Load example sparrow data (line transect survey type)
#' data(sparrowDf)
#' 
#' autoDistSamp(data = sparrowDf
#'            , formula = dist ~ groupsize(groupsize)
#'            , likelihoods = c("halfnorm","negexp")
#'            , expansions = 0
#'            , plot = FALSE
#'            , ci = NULL
#'            , area = units::set_units(1, "hectare")
#' )
#' 
#' \dontrun{
#' autoDistSamp(data = sparrowDf
#'     , formula = dist ~ 1 + groupsize(groupsize)
#'     , ci = 0.95
#'     , area = units::set_units(1, "hectare")
#' )     
#' }
#' 
#'            
#' @keywords model
#' @importFrom graphics mtext
#' @export

autoDistSamp <- function (data
                          , formula
                          , likelihoods = c("halfnorm", "hazrate", "negexp")
                          , w.lo = units::set_units(0,"m")
                          , w.hi = NULL
                          , expansions = 0:3
                          , series = c("cosine")
                          , x.scl = w.lo
                          , g.x.scl = 1
                          , warn = TRUE
                          , outputUnits = NULL
                          
                          , area = NULL
                          , propUnitSurveyed = 1.0
                          , ci = 0.95
                          , R = 500
                          , plot.bs = FALSE
                          , showProgress = TRUE
                          
                          , plot = TRUE
                          , criterion = "AICc"
                          ){
  

  if(any(!(criterion %in% c("AIC","AICc","BIC")))) stop(paste0(criterion, " criterion not supported."))
  
  # function to save results
  f.save.result <- function(results
                          , dfunc
                          , like
                          , ser
                          , expan
                          , plot
                          , CRIT
                          , showProgress) {
    
    esw <- Rdistance::effectiveDistance(dfunc)
    
    if (any(!is.na(esw) & (esw > dfunc$w.hi))) {
      scl.ok <- "Not ok"
      scl.ok.flag <- 0
      aic <- NA
    } else {
      scl.ok <- "Ok"
      scl.ok.flag <- 1
      aic <- AIC.dfunc(dfunc, criterion=CRIT) 
    }
    conv <- dfunc$convergence
    if (conv != 0) {
      if (conv == -1) {
        conv.str <- "Bad"
      } else {
        conv.str <- "No"
      }
      aic <- NA
      attr(aic,"criterion")<-CRIT
      scl.ok <- "NA"
      scl.ok.flag <- NA
    } else {
      conv.str <- "Yes"
    }
    results <- rbind(results, data.frame(like = like
                                       , series = ser
                                       , expansions = expan
                                       , converge = conv
                                       , scale = scl.ok.flag
                                       , aic = aic
                                       ))
    
    if(showProgress){
      if (nchar(like) < 8) {
        sep1 <- "\t\t"
      } else {
        sep1 <- "\t"
      } 
      cat(paste(like, sep1, ser, "\t", expan, "\t", conv.str, 
              "\t\t", scl.ok, "\t", colorize(round(aic, 4)), sep = ""))
    }
    
    if (plot) {
      plot(dfunc)  # (jdc) This is the source of "Error: object of type 'symbol' is not subsettable
      k <- readline(" Next?[Enter=y,n]")
      if (length(k) == 0) 
        k <- "y"
    } else if(showProgress) {
      cat("\n")
      k <- "y"
    } else {
      k <- "y"
    }
    
    list(results = results, k = k)
  }
  
  # Fit detection functions (dfuncEstim appears 4 times below)
  
  wwarn <- options()$warn
  options(warn = -1)
  fit.table <- NULL
  
  if(showProgress){  
    cat(paste0("Likelihood\tSeries\tExpans\tConverged?\tScale?\t",criterion,"\n"))
  }
  
  for (like in likelihoods) {
    if (like == "Gamma") {
      dfunc <- Rdistance::dfuncEstim(
                            data = data
                          , formula = formula
                          , likelihood = like
                          , w.lo = w.lo
                          , w.hi = w.hi
                          , expansions = 0
                          , series = "cosine"
                          , x.scl = w.lo
                          , g.x.scl = 1
                          , warn = TRUE
                          , outputUnits = NULL
                          )
      ser <- ""  
      expan <- 0
      fit.table <- f.save.result(fit.table, dfunc, like, 
                                 ser, expan, plot, criterion, showProgress)
      cont <- fit.table$k
      fit.table <- fit.table$results
    } else {
      for (expan in expansions) {
        for(ser in series){
          
          dfunc <- Rdistance::dfuncEstim(
              data = data
            , formula = formula
            , likelihood = like
            , w.lo = w.lo
            , w.hi = w.hi
            , expansions = expan
            , series = ser
            , x.scl = w.lo
            , g.x.scl = g.x.scl
            , warn = TRUE
            , outputUnits = NULL)
          
          fit.table <- f.save.result(fit.table, dfunc, 
                                     like, ser, expan, plot, criterion, 
                                     showProgress)
          cont <- fit.table$k
          fit.table <- fit.table$results
          
          if (cont == "n"){
            break
          }
        }

        if (cont == "n"){ 
          break
        }
      }
    }
    
    if (cont == "n"){
      break
    } 
  }
  
  # Done fitting distance function, Show summary ----
  
  if (sum(fit.table$converge != 0) > 0 & showProgress) {
    cat("Note: Some models did not converge or had parameters at their boundaries.\n")
  }
    
  fit.table$aic <- ifelse(fit.table$converge == 0, fit.table$aic, Inf)
  fit.table <- fit.table[order(fit.table$aic), ]
  fit.table$deltaAIC <- fit.table$aic - min(fit.table$aic, na.rm = TRUE)

  # Refit best dfunc
  dfunc <- Rdistance::dfuncEstim(
      data = data
    , formula = formula
    , likelihood = fit.table$like[1]
    , w.lo = w.lo
    , w.hi = w.hi
    , expansions = fit.table$expansions[1]
    , series = fit.table$series[1]
    , x.scl = w.lo
    , g.x.scl = g.x.scl
    , warn = TRUE
    , outputUnits = NULL)
  if (plot) {
    plot(dfunc)
    graphics::mtext("BEST FITTING FUNCTION", side = 3, cex = 1.5, line = 3)
  }

  # Estimate abundance ----
  
  abund <- abundEstim(object = dfunc
              , area = area
              , propUnitSurveyed = propUnitSurveyed
              , ci = ci
              , R = R
              , plot.bs = plot.bs
              , showProgress = showProgress)
  
  # Store the fitting table, just in case user wants it.
  abund$fitTable <- fit.table

  if(showProgress){
    cat("\n\n------------ Abundance Estimate Based on Top-Ranked Detection Function ------------\n")
    summary(abund, criterion=criterion)
  }
  
  options(warn = wwarn)
  abund
}
