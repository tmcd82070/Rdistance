#' @name autoDistSamp
#' @aliases autoDistSamp
#' @title Automated classical distance analysis.
#' @description Perform automated classical detection function selection and 
#' estimation of abundance.
#' 
#' @param formula This parameter is passed to \code{dfuncEstim}.
#'   See \code{dfuncEstim} documentation for definition. 
#' @param detectionData This parameter is passed to \code{dfuncEstim} 
#' and \code{abundEstim}. See \code{abundEstim} documentation for definition.
#' 
#' @param siteData This parameter is passed to \code{abundEstim}.
#'   See \code{abundEstim} documentation for definition.
#'   
#' @param pointSurvey A logical scalar specifying whether input data come
#' from a point-transect survey type (TRUE),
#' or a line-transect survey type (FALSE).
#' 
#' @param w.lo This parameter is passed to \code{dfuncEstim}.
#'   See \code{dfuncEstim} documentation for definition.
#'   
#' @param w.hi This parameter is passed to \code{dfuncEstim}.
#'   See \code{dfuncEstim} documentation for definition.
#'   
#' @param warn This parameter is passed to \code{dfuncEstim}.
#'   \code{dfuncEstim} documentation for definition.
#'   
#' @param area This parameter is passed to \code{abundEstim}.
#'   See \code{abundEstim} documentation for definition.
#'   
#' @param ci This parameter is passed to \code{abundEstim}.
#'   See \code{abundEstim} documentation for definition.
#'   
#' @param R This parameter is passed to \code{abundEstim}.
#'   See \code{abundEstim} documentation for definition.
#'   
#' @param by.id This parameter is passed to \code{abundEstim}.
#'   See \code{abundEstim} documentation for definition.
#'   
#' @param plot.bs This parameter is passed to \code{abundEstim}.
#'   See \code{abundEstim} documentation for definition.
#'   
#' @param likelihoods Vector of strings specifying the 
#' likelihoods to consider during model selection. Valid values 
#' at present are "uniform", "halfnorm", "hazrate", "negexp", 
#' and "Gamma". See Details for the models this routine considers.
#' 
#' @param series Vector of series types to consider during model selection. 
#'   Valid values are 'simple', 'hermite', and 'cosine'. See Details for 
#'   the models this routine considers.
#'   
#' @param expansions Vector of the number of expansion terms to 
#' consider during model selection. Valid values are 0 through 3. 
#' See Details for the models this routine considers. 
#' 
#' @param plot Logical scalar specifying whether to plot models during model selection. 
#'   If \code{TRUE}, a histogram with fitted distance function is plotted for every fitted model. 
#'   The function pauses between each plot and prompts the user for whether they want to continue or not. 
#'   For completely automated estimation, set \code{plot} = \code{FALSE}.
#'   
#' @param ... Additional parameters passed to \code{dfuncEstim}, which in turn are passed to \code{F.gx.estim}. 
#'   These include \code{x.scl}, \code{g.x.scl}, and \code{observer} for estimating double observer probabilities.
#'   
#' @details During model selection, each series and number of expansions is crossed with 
#' each of the likelihoods. For example, if \code{likelihoods} has 3 elements, 
#' \code{series} has 2 elements, and \code{expansions} has 4 elements, 
#' the total number of models fitted is 3 (likelihoods) * 2 (series) * 4 (expansions) 
#' = 24 models.  The default specification fits 41 detection functions 
#' from the "halfnorm", "hazrate", "uniform", "negexp", and "Gamma" likelihoods 
#' (note that Gamma does not currently implement expansions, see 
#' \code{\link{Gamma.like}}).  The model with lowest AIC is choosen 
#' as 'best', and estimation of abundance proceeds using that model.
#' 
#' @return An 'abundance estimate' object (see \code{abundEstim} and \code{dfuncEstim}). 
#'   Returned abundance estimates are based on the best fitting distance function among those fitted.
#'   
#' @author Trent McDonald, WEST Inc.,  \email{tmcdonald@west-inc.com}
#'         Aidan McDonald, WEST Inc.,  \email{aidan@mcdcentral.org}
#'         Jason Carlisle, University of Wyoming and WEST Inc., \email{jcarlisle@west-inc.com}
#' @seealso \code{\link{dfuncEstim}}, \code{\link{abundEstim}}
#' @examples # Load the example datasets of sparrow detections and transects from package
#'   data(sparrow.detections)
#'   data(sparrow.transects)
#'   
#'   # Automate fitting multiple detection functions
#'   # And estimate abundance (density per ha in this case) given the 'best' detection function
#'   # Note, area=10000 converts to density per hectare (for distances measured in meters)
#'   # Note, a person should do more than R=20 iterations 
#'   autoDistSamp(detectionData=sparrow.detections, siteData=sparrow.sites,
#'                   likelihood=c("halfnorm", "hazrate", "negexp"),
#'                   series=c("cosine", "simple"),
#'                   expansions=c(0, 1), area=10000, R=20, ci=0.95, by.id=FALSE,
#'                   plot.bs=FALSE, w.hi=150, plot=TRUE)
#' @keywords model
#' @export

autoDistSamp <- function (formula, detectionData, siteData, 
                             w.lo=0, w.hi=max(dist),
                             likelihoods=c("halfnorm", "hazrate", "uniform", "negexp", "Gamma"),
                             series=c("cosine", "hermite", "simple"), expansions=0:3,
                             pointSurvey=FALSE, warn=TRUE,
                             area=1, ci=0.95, R=500, by.id=FALSE, plot.bs=FALSE,                          
                             plot=TRUE, ...){
  
  
    # Stop and print error if key columns of detectionData or siteData are missing or contain NAs
  if(!("dist" %in% names(detectionData))) stop("There is no column named 'dist' in your detectionData.")
  if(!("siteID" %in% names(detectionData))) stop("There is no column named 'siteID' in your detectionData.")
  if(!("groupsize" %in% names(detectionData))) stop("There is no column named 'groupsize' in your detectionData.")
  
  if(!("siteID" %in% names(siteData))) stop("There is no column named 'siteID' in your siteData.")
  # if(!("length" %in% names(siteData))) stop("There is no column named 'length' in your siteData.")
  
  if(any(is.na(detectionData$dist))) stop("Please remove rows for which detectionData$dist is NA.")
  if(any(is.na(detectionData$siteID))) stop("Please remove rows for which detectionData$siteID is NA.")
  if(any(is.na(detectionData$groupsize))) stop("Please remove rows for which detectionData$groupsize is NA.")
  
  if(any(is.na(siteData$siteID))) stop("Please remove NA's from siteData$siteID.")
  # if(any(is.na(siteData$length))) stop("Please remove NA's from siteData$length.")
  
  
  # extract distance vector from detectionData
  # dist <- detectionData$dist
  
  
  # function to save results
  f.save.result <- function(results, dfunc, like, ser, expan, 
                            plot) {
    
    esw <- effectiveDistance(dfunc)
    

    if (!is.na(esw) & (esw > dfunc$w.hi)) {
      scl.ok <- "Not ok"
      scl.ok.flag <- 0
      aic <- NA
    }
    else {
      scl.ok <- "Ok"
      scl.ok.flag <- 1
      aic = AIC(dfunc)
    }
    conv <- dfunc$convergence
    if (conv != 0) {
      if (conv == -1) {
        conv.str <- "Bad"
      }
      else {
        conv.str <- "No"
      }
      aic <- NA
      scl.ok <- "NA"
      scl.ok.flag <- NA
    }
    else {
      conv.str <- "Yes"
    }
    results <- rbind(results, data.frame(like = like, series = ser, 
                                         expansions = expan, converge = conv, scale = scl.ok.flag, 
                                         aic = aic))
    if (nchar(like) < 8) 
      sep1 <- "\t\t"
    else sep1 <- "\t"
    cat(paste(like, sep1, ser, "\t", expan, "\t", conv.str, 
              "\t\t", scl.ok, "\t", round(aic, 4), sep = ""))
    if (plot) {
      plot(dfunc)
      k <- readline(" Next?[entr=y,n]")
      if (length(k) == 0) 
        k <- "y"
    }
    else {
      cat("\n")
      k <- "y"
    }
    list(results = results, k = k)
  }
  
  
  # Fit detection functions (dfuncEstim appears 4 times below)
  
  wwarn <- options()$warn
  options(warn = -1)
  fit.table <- NULL
  cat("Likelihood\tSeries\tExpans\tConverged?\tScale?\tAIC\n")
  for (like in likelihoods) {
    if (like == "Gamma") {
      dfunc <- dfuncEstim(formula = formula, data = detectionData, likelihood = like, w.lo = w.lo, 
                             w.hi = w.hi, pointSurvey=pointSurvey, ...)
      ser <- ""
      expan <- 0
      fit.table <- f.save.result(fit.table, dfunc, like, 
                                 ser, expan, plot)
      cont <- fit.table$k
      fit.table <- fit.table$results
    }
    else {
      for (expan in expansions) {
        if (expan == 0) {
          ser <- "cosine"
          dfunc <- dfuncEstim(formula = formula, data = detectionData, likelihood = like, 
                                 w.lo = w.lo, w.hi = w.hi, expansions = expan, 
                                 series = ser, pointSurvey=pointSurvey, ...)
          fit.table <- f.save.result(fit.table, dfunc, 
                                     like, ser, expan, plot)
          cont <- fit.table$k
          fit.table <- fit.table$results
        }
        else {
          for (ser in series) {
            dfunc <- dfuncEstim(formula = formula, data = detectionData, likelihood = like, 
                                   w.lo = w.lo, w.hi = w.hi, expansions = expan, 
                                   series = ser, pointSurvey=pointSurvey, ...)
            fit.table <- f.save.result(fit.table, dfunc, 
                                       like, ser, expan, plot)
            cont <- fit.table$k
            fit.table <- fit.table$results
            if (cont == "n") 
              break
          }
        }
        if (cont == "n") 
          break
      }
      if (cont == "n") 
        break
    }
  }
  if (sum(fit.table$converge != 0) > 0) 
    cat("Note: Some models did not converge or had parameters at their boundaries.\n")
  fit.table$aic <- ifelse(fit.table$converge == 0, fit.table$aic, 
                          Inf)
  fit.table <- fit.table[order(fit.table$aic), ]

  dfunc <- dfuncEstim(formula = formula, data = detectionData, likelihood = fit.table$like[1], 
                         w.lo = w.lo, w.hi = w.hi, expansions = fit.table$expansions[1], 
                         series = fit.table$series[1], pointSurvey=pointSurvey, ...)
  if (plot) {
    plot(dfunc)
    mtext("BEST FITTING FUNCTION", side = 3, cex = 1.5, line = 3)
  }

  
  abund <- abundEstim(dfunc, detectionData=detectionData, siteData=siteData,
                         area=area, ci=ci, R=R, plot.bs=plot.bs, by.id=by.id)

#   }
  cat("\n\n---------------- Final Automated CDS Abundance Estimate -------------------------------\n")
  print(abund)
  options(warn = wwarn)
  abund
}