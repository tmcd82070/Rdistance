#' @name autoDistSamp
#' 
#' @title Automated classical distance analysis
#' 
#' @description Perform automated classical detection 
#' function selection and 
#' estimation of abundance.
#' 
#' @param formula This parameter is passed to \code{dfuncEstim}.
#'   See \code{dfuncEstim} documentation for definition.
#'   
#' @param detectionData This parameter is passed to \code{dfuncEstim} 
#' and \code{abundEstim}. See \code{abundEstim} documentation for definition.
#' 
#' @param siteData This parameter is passed to \code{abundEstim}.
#'   See \code{abundEstim} documentation for definition.
#'   
#' @param pointSurvey This parameter is passed to \code{dfuncEstim}.
#'   See \code{dfuncEstim} documentation for definition.
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
#'   If \code{ci} is NULL, abundance is estimated but no 
#'   bootstrapping is performed.
#'   See \code{abundEstim} documentation for definition.
#'   
#' @param R This parameter is passed to \code{abundEstim}.
#'   See \code{abundEstim} documentation for definition.
#'   
#' @param bySite This parameter is passed to \code{abundEstim}.
#'   See \code{abundEstim} documentation for definition.
#'   
#' @param models A vector of strings specifying covarites 
#' to include in the distance function. Each element in 
#' \code{models} is a set of covariates to be \emph{added}
#' to the base model specified in \code{formula}.  For example, if
#' \code{formula=dist~x} and 
#' \code{models=c("a", "b", "a+b")}, the set of models fitted 
#' is
#' \code{dist~x + a}, \code{dist~x + b}, and \code{dist~x + a + b}. 
#' To include the \code{formula} model, set one element of 
#' \code{models} equal to "1". By manipulating both 
#' \code{formula} and \code{models} over multiple calls, 
#' it is possible to manually 
#' conduct forward stepwise selection of variables. 
#' 
#' A few caviates: Covariates and expansions are not 
#' allowed in the Gamma
#' likelihood.  If 'Gamma' is among the likelihoods requested, 
#' only the intercept model is fitted and expansions are set to 0.
#' Expansions are not allowed with covariates. For all models with 
#' expansions >0, the covariate model is set to 
#' the intercept only.   
#'  
#' 
#' @param plot.bs Logical for whether to plot bootstrap 
#' iterations after the top model has been selected and 
#' during final estimation of confidence intervals.  
#' This parameter is passed unchanged to \code{abundEstim}.
#' See \code{abundEstim} help for additional information.
#'   
#' @param showProgress Logical for whether to 
#' suppress intermediate output.  If \code{showProgress=TRUE}, 
#' a table of model fitting results appears in the console as they 
#' are estimated, and a progress bar shows progress through 
#' the bootstrap iterations at the end.  If \code{showProgress=FALSE}, 
#' all intermediate output is suppressed which is handy for programming
#' and simulations.
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
#' See Details for the models this routine considers. Note, expansion terms are not currently
#' allowed in models with covariates.
#' 
#' @param includeFits Logical scaler.  If TRUE, all fitted distance 
#' function objects are returned as a list in the output object. 
#' The fitted dfunc objects are stored in the \code{dfuncFits}
#' component of the output.  Length of this list is 
#' \code{nrow(output$fitTable)} and numbers in 
#' \code{output$fitTable$modNumber} correspond to positions in 
#' this list.  For example, the model referenced on row 
#' \code{i} of \code{output$fitTable} is 
#' \code{output$dfuncFits[[output$fitTable$modNumber[i]]]}.
#' 
#' @param plot Logical scalar specifying whether to plot models during model selection. 
#'   If \code{TRUE}, a histogram with fitted distance function is plotted for every fitted model. 
#'   The function pauses between each plot and prompts the user for whether they want to continue or not. 
#'   For completely automated estimation, set \code{plot} = \code{FALSE}.
#'   
#' @param criterion A string specifying the criterion to use when assessing model fit.   
#' The best fitting model from this routine is the one with lowest value 
#' of this fit criterion.  This must be one of "AICc" (the default), 
#' "AIC", or "BIC".  See \code{\link{AIC.dfunc}} for formulas. 
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
#' \code{\link{Gamma.like}}). Note, expansion terms are not currently
#' allowed in models with covariates.  The model with lowest AIC is selected 
#' as 'best', and estimation of abundance proceeds using that model.
#' 
#' Suppress all intermediate output using \code{plot.bs=FALSE}, 
#' \code{showProgress=FALSE}, and \code{plot=FALSE}. 
#' 
#' @return An 'abundance estimate' object with one 
#' or two additional components.  See \code{abundEstim} 
#' and \code{dfuncEstim} for an explanation of base components. 
#' Abundance estimates in the output are based 
#' on the best fitting (according to \code{criterion}) 
#' distance function among those fitted.
#' 
#' In addition to the base components, a 'fit table' 
#' with one row per fitted model, sorted by 
#' \code{criterion}, is returned as component
#' \code{$fitTable}.  The fit table component contains 
#' the following fields:
#' \itemize{
#'   \item \code{model} : covariates in the model 
#'   \item \code{like} : likelihood used, 
#'   \item \code{series} : series of the expansions, if any
#'   \item \code{expansions} : number of expansion terms
#'   \item \code{converged} : whether the model converged (0=converged,1=not)
#'   \item \code{scale} : whether the model passed the 
#'      scale check (1=passed scale check,0=did not pass)
#'   \item \code{AICc} : the criterion used. 
#'   \item \code{pHat} : average probability of detection for 
#'      the model
#'   \item \code{K} : the number of covariates in the model
#'   \item \code{modNumber} : the number of the model reference 
#'   on that row. This can be used to resort the fit table into
#'   the order that models were fitted or to reference fitted 
#'   objects if \code{includeFits == TRUE}.    
#' }
#' 
#' If \code{includeFits == TRUE}, an additional component 
#' named \code{$dfuncFits} is included in the output object.
#' \code{$dfuncFits} is a list with length equal to the number
#' of fitted models and containing the actual fitted distance 
#' function objects.  The order of \code{$dfuncFits} is the 
#' order in which models were fitted and can be reference 
#' using the \code{modNumber} column of the fit table. I.e., 
#' the dfunc object corresponding to row \code{i} of the fit table
#' is in position \code{modNumber[i]} of \code{dfuncFits}.  
#'   
#' @author Trent McDonald, WEST Inc.,  \email{tmcdonald@west-inc.com}\cr
#'         Aidan McDonald, WEST Inc.,  \email{aidan@mcdcentral.org}\cr
#'         Jason Carlisle, University of Wyoming and WEST Inc., \email{jcarlisle@west-inc.com}
#' @seealso \code{\link{dfuncEstim}}, \code{\link{abundEstim}}.
#' @examples 
#' # Load example sparrow data (line transect survey type)
#' data(sparrowDetectionData)
#' data(sparrowSiteData)
#' 
#' # Automate fitting multiple detection functions, and estimate abundance
#' # (density per ha in this case), given the 'best' detection function
#' # Note, area=10000 converts to density per ha (for distances measured in m)
#' # Note, users should do more than R=20 iterations of the bootstrap
#' autoDistSamp(formula=dist ~ 1,
#'              detectionData=sparrowDetectionData, siteData=sparrowSiteData,
#'              likelihood=c("halfnorm", "hazrate"), w.hi=100,
#'              series=c("cosine", "simple"), expansions=c(0, 1),
#'              area=10000, R=20, ci=0.95, bySite=FALSE,
#'              plot.bs=TRUE, plot=FALSE, pointSurvey=FALSE)
#' @keywords model
#' @export
#' @importFrom stats terms
#' @importFrom graphics mtext

autoDistSamp <- function (formula, detectionData, siteData, 
                          models = "1",
                          likelihoods=c("halfnorm", "hazrate", "uniform", "negexp", "Gamma"),
                          series=c("cosine", "hermite", "simple"), 
                          expansions=0:3,
                          w.lo=0, w.hi=NULL,
                          pointSurvey=FALSE, warn=TRUE,
                          area=1, ci=0.95, R=500, 
                          bySite=FALSE, includeFits=FALSE,
                          plot.bs=FALSE, showProgress=TRUE,
                          plot=TRUE, criterion="AICc", ...){
  
  
  
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
  
  if(any(!(criterion %in% c("AIC","AICc","BIC")))) stop(paste0(criterion, " criterion not supported."))

  if(any(expansions < 0 | expansions >3)) stop("Expansions must be between 0 and 3")  

  # =========================================
  # function to show results
  f.show.result <- function(i, results, dfunc, plot, CRIT) {
    
    esw <- effectiveDistance(dfunc)

    if (!is.na(esw) & (esw > dfunc$w.hi)) {
      scl.ok <- "Not ok"
      scl.ok.flag <- 0
      aic <- NA
    } else {
      scl.ok <- "Ok"
      scl.ok.flag <- 1
      aic = AIC.dfunc(dfunc, criterion=CRIT) 
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

    if (nchar(like) < 8) {
      sep1 <- "\t\t"
    } else {
      sep1 <- "\t"
    } 
    out <- paste(results[i,],collapse="\t")
    cat(paste(out,"\n"))

    if (plot) {
      plot(dfunc)  # (jdc) This is the source of "Error: object of type 'symbol' is not subsettable
      k <- readline(" Next?[entr=y,n]")
      if (length(k) == 0) 
        k <- "y"
    } else {
      cat("\n")
      k <- "y"
    } 
    
    k
  }
  
  # ===============================
  
  
  
  # Fit detection functions (dfuncEstim appears 4 times below)
  
  wwarn <- options()$warn
  options(warn = -1)
  fits <- vector("list",nrow(aicTable))
  
  if(showProgress){  
    cat(paste0("Likelihood\tSeries\tExpans\tConverged?\tScale?\t",criterion,"\n"))
  }
  
  aicTable <- expand.grid(model=models,
                          likelihood=likelihoods,
                          series=series,
                          expansion=expansions)
  
  # Covars not allowed with Gamma likelihood
  gammaMod <- grep("Gamma", aicTable$likelihood)
  if(length(gammaMod)>0){
    aicTable <- aicTable[-gammaMod[-1],]
    gammaMod <- gammaMod[1]  
    aicTable$model[gammaMod[1]] <- "1"
    aicTable$expansion[gammaMod[1]] <- 0
  }

  # Expansions not allowed with covars. Do this to kill warning generated during fit
  expanMod <- which(aicTable$expansion > 0)
  if(length(expanMod)>0){
    aicTable$model[expanMod] <- "1"
    aicTable <- unique(aicTable)
  }
  
  aicTable$pHat <- NA
  aicTable$AICc <- NA
  aicTable$K <- NA
  aicTable$converged <- NA
  aicTable$modNumber <- NA
  
  for(i in 1:nrow(aicTable)) {
    
    like <- aicTable$likelihood[i]
    ser <- aicTable$series[i]
    expan <- aicTable$expansion[i]
    if(like=="Gamma" | expan > 0){
      form <- update(formula, paste("~", aicTable$model[i])) # replace with intercept only
    } else {
      form <- update(formula, paste("~.+", aicTable$model[i]))
    }
    
    dfunc <- dfuncEstim(formula = formula, 
                        detectionData = detectionData,
                        siteData = siteData, 
                        likelihood = like, 
                        w.lo = w.lo, 
                        w.hi = w.hi, 
                        expansions = expan, 
                        series = ser, 
                        pointSurvey=pointSurvey, 
                        ...)    
    

    aicTable$AICc[i] <- AIC(dfunc, criterion=criterion)
    aicTable$pHat[i] <- mean(ESW(dfunc)/(w.hi - w.lo))
    aicTable$convergence[i] <- dfunc$convergence
    aicTable$K[i] <- length(coef(dfunc))
    aicTable$modNumber[i] <- i

    # Store dfunc so don't have to refit top model
    fits[[i]] <- dfunc  
    
    if(showProgress){
      continue <- f.show.result(i,aicTablefit.table, 
                                 dfunc, plot, criterion )
      if (continue == "n"){
        break
      }
    }

  }

  
  # ------------  

  if (sum(aicTable$converged != 0) > 0 & showProgress) {
    cat("Note: Some models did not converge or had parameters at their boundaries.\n")
  }
    
  
  # Sort by criterion
  aicTable$aic <- ifelse(aicTable$converged == 0, aicTable$aic, 
                          Inf)
  aicTable <- aicTable[order(aicTable$aic), ]

  
  # best dfunc is fits[[aicTable$modNumber[1]]]
  dfunc <- fits[[aicTable$modNumber[1]]]
  
  if (plot) {
    plot(dfunc)
    mtext("BEST FITTING FUNCTION", side = 3, cex = 1.5, line = 3)
  }

  
  abund <- abundEstim(dfunc, detectionData=detectionData, siteData=siteData,
                      area=area, ci=ci, R=R, plot.bs=plot.bs, 
                      bySite=bySite, showProgress = showProgress)
  
  # Store the fitting table, just in case user wants it.
  abund$fitTable <- aicTable
  
  # Store all fitted models if requested
  if(includeFits){
    abund$dfuncFits <- fits
  }

  if(showProgress){
    cat("\n\n------------ Abundance Estimate Based on Top-Ranked Detection Function ------------\n")
    print(abund, criterion=criterion)
  }
  
  options(warn = wwarn)
  abund
}