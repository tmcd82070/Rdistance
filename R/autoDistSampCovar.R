#' @name autoDistSampCovar
#' @aliases autoDistSampCovar
#' @title Fit multiple distance sampling detection functions with different covariates
#' @description Runs multiple combinations of likelihoods and covariates, and creates an AICc table
#' 
#' @param detectionData This parameter is passed to \code{dfuncEstim} 
#' and \code{abundEstim}. See \code{abundEstim} documentation for definition.
#' 
#' @param siteData This parameter is passed to \code{abundEstim}.
#'   See \code{abundEstim} documentation for definition.
#'   
#' @param covars Character vector, names of cols in detectionData to use as covariates.   
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
#'   See \code{abundEstim} documentation for definition.
#'   
#' @param R This parameter is passed to \code{abundEstim}.
#'   See \code{abundEstim} documentation for definition.
#'   
#' @param bySite This parameter is passed to \code{abundEstim}.
#'   See \code{abundEstim} documentation for definition.
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
#' @return A list with R+2 elements where R is the number of models run.
#' Elements 1:R are the dfunc objects fit using Rdistance, element R+1 is the data.frame of the AICc table,
#' and element R+2 is the detectionData used in the detection functions (which had truncation applied, including the
#' re-scaling of distances if w.lo > 0).
#'   
#' @author Trent McDonald, WEST Inc.,  \email{tmcdonald@west-inc.com}\cr
#'         Aidan McDonald, WEST Inc.,  \email{aidan@mcdcentral.org}\cr
#'         Jason Carlisle, University of Wyoming and WEST Inc., \email{jcarlisle@west-inc.com}\cr
#'         Abigail Hoffman, Univeristy of Wyoming and WEST Inc., \email{ahoffm12@uwyo.edu}
#' @seealso \code{\link{dfuncEstim}}, \code{\link{abundEstim}}.
#' @examples
#' # Automate fitting detection functions for different comninations of likelihoods and covariates
#' # Load example sparrow data (line transect survey type)
#' data(sparrowDetectionData)
#' data(sparrowSiteData)
#' 
#' ms<- dfuncModSel(detectionData = sparrowDetectionData, siteData = sparrowSiteData,  covars=c("1", "bare", "shrub", "herb", "height",
#'  "bare + observer", "shrub + observer", "herb + observer", "height + observer"),likelihoods=c("halfnorm", "hazrate"), 
#'  w.lo=0, w.hi=125)
#' @keywords model
#' @export
#' @importFrom stats terms
#' @importFrom graphics mtext
#' @param covars Character vector, names of cols in detectionData to use as covariates.
#'

  
  autoDistSampCovar <- function (detectionData, siteData, covars,
                                  likelihoods=c("halfnorm", "hazrate", "uniform", "negexp", "Gamma"),
                                  series=c("cosine", "hermite", "simple"), 
                                  expansions=0:3,
                                  w.lo=0, w.hi=NULL,
                                  pointSurvey=FALSE, warn=TRUE,
                                  area=1, ci=0.95, R=500, 
                                  bySite=FALSE, includeFits=FALSE,
                                  plot.bs=FALSE, showProgress=TRUE,
                                  plot=TRUE, criterion="AICc", ...){
    
    detectionData<- merge(detectionData, siteData, by="siteID")
    
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
  
  
  if(length(w.lo) != 1) {
    stop("Need to specify only one value of w.lo")
  }
  
  if(length(w.hi) != 1) {
    stop("Need to specify only one value of w.hi")
  }
  
  
  
  
  # The built-in truncation with w.lo and w.hi arguments doesn't appear to be working well in Rdistance
  # Manually adjust distances such that dist(w.lo) = 0
  #detectionData <- detectionData[detectionData$dist >= w.lo & detectionData$dist <= w.hi, ]
  # hist(detectionData[, distCol])
  #detectionData$newDist <- detectionData$dist - w.lo
  # hist(detectionData$newDist)
  
  
  
  # Rdistance drops rows if the covariate has an NA value
  # Must make sure those rows are dropped for all models, or else AICc values aren't comparable across models with different amounts of input data
  # Extract unique character names of covars
  (covarVec <- gsub(" ", "", covars, fixed = TRUE))  # remove spaces often inserved around "+" symbol in formulas
  (covarVec <- strsplit(covarVec, "\\+"))  # split each term out
  (covarVec <- do.call(c, covarVec))
  (covarVec <- unique(covarVec))
  (covarVec <- covarVec[covarVec != "1"])  # drop intercept term
  
  # Check for any NAs in those columns in detectionData
  if(anyNA(detectionData[covarVec])) {
    
    # Indicate which rows have NAs in any covariate column
    detectionData$nas <- rowSums(is.na(detectionData[covarVec]))
    
    # Keep only rows with no NAs in any covariate column
    detectionData <- detectionData[detectionData$nas == 0, ]
    
  }
  
   
  
  # Develop unique combos of models to run
  (aicTable <- expand.grid(likelihood=likelihoods,
                           covar=covars,
                           w.lo=w.lo,
                           w.hi=w.hi))
  
  
  
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
  
  
  
  # Empty list to fill (this will be the returned object)
  output <- list()
  
  
  
  
  # Define function to fit dfunc models and calculate probability of detection
  # This function will then be run using lapply 
  fitDistSampModels <- function(aicTable, i) {
    
    form <- paste0("dist ~ ", aicTable$covar[i])
    
    # Fit  models using RDistance package
    fit <- dfuncEstim(formula = form,
                                 detectionData = detectionData,
                                 likelihood = aicTable$likelihood[i],
                                 w.lo = 0,
                                 w.hi = aicTable$w.hi[i] - aicTable$w.lo[i])
 
    
    # Add probability of detection for each row (each detection) to the dfunc object returned
    # Adding a vector of length n (number of detections) that is the probability of detection for that detection
    fit[[length(fit)+1]] <- ESW(fit, newdata=detectionData) / fit$w.hi
    names(fit)[length(fit)] <- "probDetection"
    
    # For dfuncs with no covariates, the length of probDetection is 1 (same p for all rows)
    # Replicate n number of times, since the probDetection vector will be of length n for dfuncs with covariates
    if(length(fit$probDetection) == 1) {
      fit$probDetection <- rep(fit$probDetection, length(fit$dist))
    

    }
    

    return(fit)
    
  }
  
  
    
  output <- lapply(1:nrow(aicTable), function(i) fitDistSampModels(aicTable, i))

  
  # Add columns to fill with output
  aicTable$nDetections <- NA
  aicTable$meanProbDetection <- NA
  aicTable$AICc <- NA
  aicTable$K <- NA
  aicTable$convergence <- NA
  
  
  
  # Loop through and extract info from each model
  for(i in 1:nrow(aicTable)) {
    
    # collect AICc and predicted values
    aicTable$nDetections[i] <- length(output[[i]]$dist)
    aicTable$AICc[i] <- AIC(output[[i]], criterion="AICc")
    aicTable$meanProbDetection[i] <- mean(output[[i]]$probDetection)
    aicTable$convergence[i] <- output[[i]]$convergence
    aicTable$K[i] <- length(coef(output[[i]]))
    
    # Store NA for AICc if evidence of false convergence
    if(aicTable$AICc[i] == -Inf | aicTable$convergence[i] != 0) {
      aicTable$AICc[i] <- NA
    }
    
  }
  
  
  # Add a unique name to each dfunc in the list
  names(output) <- paste0("mod", 1:i)
  
  # And add the unique name to the AICc table and move to first column
  aicTable$modID <- names(output)
  aicTable <- aicTable[, c(ncol(aicTable), 1:(ncol(aicTable)-1))]
  

  
  
  # Sort by criterion
  aicTable$AICc <- ifelse(aicTable$convergence == 0, aicTable$AICc, 
                         Inf)
  aicTable <- aicTable[order(aicTable$AICc), ]
  
  # Append aicTable to the end of the data.frame
  output[[length(output)+1]] <- aicTable
  
  # Name the last element, the AICc table
  names(output)[length(output)] <- "aicTable"
  
  #best fitting function
  dfunc<- ds[as.numeric(gsub("[^0-9]", "", aicTable[1,1]))]
  
  if (plot) {
    plot(dfunc[[1]])
    mtext("BEST FITTING FUNCTION", side = 3, cex = 1.5, line = 3)
  }
  
  # Add the detectionData data.frame to the dfunc object returned
  output[[length(output)+1]] <- detectionData
  names(output)[length(output)] <- "detectionData"
  
  #cannot find covariate objects
  #abund <- abundEstim(dfunc[[1]], detectionData=detectionData, siteData=siteData,
  #                    area=area, ci=ci, R=R, plot.bs=plot.bs, 
  #                    bySite=bySite, showProgress = showProgress)
  
  print(dfunc)
  
  return(output)
  
}  # end function