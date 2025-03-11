#' @title Estimate abundance from distance-sampling data
#'
#' @description Estimate abundance (or density) given an estimated detection
#'   function and supplemental information on observed group sizes, transect
#'   lengths, area surveyed, etc.  Also computes confidence intervals on
#'   abundance (or density) using a the bias corrected bootstrap method.
#'
#' @param dfunc An estimated 'dfunc' object produced by \code{dfuncEstim}.
#' 
#' @inheritParams dfuncEstim 
#' 
#' @param area A scalar containing the total area of 
#' inference. Commonly, this is study area size.  
#' If \code{area} is NULL (the default), 
#' \code{area} will be set to 1 square unit of the output units and this
#' produces abundance estimates equal density estimates. 
#' If \code{area} is not NULL, it must have measurement units 
#' assigned by the \code{units} package. 
#' The units on \code{area} must be convertible
#' to squared output units. Units 
#' on \code{area} must be two-dimensional. 
#' For example, if output units are "foo", 
#' units on area must be convertible to "foo^2" by the \code{units}
#' package. 
#' Units of "km^2", "cm^2", "ha", "m^2", "acre", "mi^2", and many
#' others are acceptable.  
#'   
#' @param singleSided Logical scalar. If only one side of the transect was 
#' observed, set \code{singleSided} = TRUE. If both sides of line-transects 
#' were observed, \code{singleSided} = FALSE. Some surveys
#' observe only one side of transect lines for a variety of logistical reasons. 
#' For example, some aerial line-transect surveys place observers on only one
#' side of the aircraft. This parameter effects only line-transects.  When 
#' \code{singleSided} = TRUE, surveyed area is halved and the density 
#' estimator's denominator (see \bold{Details})
#' is \eqn{(ESW)(L)}, not \eqn{2(ESW)(L)}.
#' 
#' @param ci A scalar indicating the confidence level of confidence intervals. 
#'   Confidence intervals are computed using a bias corrected bootstrap
#'   method. If \code{ci = NULL}, confidence intervals are not computed.
#'   
#' @param R The number of bootstrap iterations to conduct when \code{ci} is not
#'   NULL.
#'   
#' @param lengthColumn Character string specifying the (single) column in 
#'   \code{siteData} that contains transect lengths. This is ignored if 
#'   \code{pointSurvey} = TRUE. This column must have measurement units. 
#'   
#' 
#' @param plot.bs A logical scalar indicating whether to plot individual
#'   bootstrap iterations.
#'   
#' @param showProgress A logical indicating whether to show a text-based
#'   progress bar during bootstrapping. Default is \code{TRUE}. 
#'   It is handy to shut off the 
#'   progress bar if running this within another function. Otherwise, 
#'   it is handy to see progress of the bootstrap iterations.
#'   
#' @details The abundance estimate for line-transect surveys (if no covariates
#'    are included in the detection function and both sides of the transect 
#'    were observed) is 
#'    \deqn{N =\frac{n(A)}{2(ESW)(L)}}{%
#'          N = n*A / (2*ESW*L)} 
#'    where \emph{n} is total number of sighted individuals 
#'   (i.e., \code{sum(dfunc$detections$groupSizes)}), \emph{L} is the total 
#'   length of surveyed transect (i.e., \code{sum(siteData[,lengthColumn])}),
#'   and \emph{ESW} is effective strip width
#'   computed from the estimated distance function (i.e., \code{ESW(dfunc)}).
#'   If only one side of transects were observed, the "2" in the denominator 
#'   is not present (or, replaced with a "1"). 
#'   
#'   The abundance estimate for point transect surveys (if no covariates are
#'   included) is 
#'    \deqn{N =\frac{n(A)}{\pi(ESR^2)(P)}}{%
#'          N = n*A / ((3.1415)*ESR^2*(P))} 
#'    where \emph{n} is total number of sighted individuals,
#'    \emph{P} is the total number of surveyed points, 
#'    and \emph{ESR} is effective search radius 
#'    computed from the estimated distance function (i.e., \code{ESR(dfunc)}).
#'
#'  Setting \code{plot.bs=FALSE} and \code{showProgress=FALSE} 
#'     suppresses all intermediate output.  
#'     
#' @section Bootstrap Confidence Intervals:
#' 
#'   The bootstrap confidence interval for abundance 
#'   assumes that the fundamental units of
#'   replication (lines or points, hereafter "sites") are independent.
#'   The bias corrected bootstrap
#'   method used here resamples the units of replication (sites), 
#'   refits the distance function, and estimates abundance using 
#'   the resampled counts and re-estimated distance function. 
#'   The original data frames, \code{detectionData} and \code{siteData}, 
#'   are needed here for bootstrapping because they contain the transect 
#'   and detection information.
#'   If a double-observer data
#'   frame is included in \code{dfunc}, rows of the double-observer data frame
#'   are re-sampled each bootstrap iteration. 
#'   
#'   This routine does not 
#'   re-select the distance model fitted to resampled data.  The 
#'   model in the input object is re-fitted every iteration.  
#'   
#'   By default, \code{R} = 500 iterations are performed, after which the bias
#'   corrected confidence intervals are computed (Manly, 1997, section 3.4).
#'   
#'   During bootstrap iterations, the distance function can fail 
#'   to converge on the resampled data.   An iteration can fail 
#'   to converge for a two reasons:
#'   (1) no detections on the iteration, and (2) bad configuration 
#'   of distances on the iteration which pushes parameters to their 
#'   bounds. When an iteration fails to produce a valid 
#'   distance function, \code{Rdistance} 
#'   simply skips the iteration, effectively ignoring these 
#'   non-convergent iterations. 
#'   If the proportion of non-convergent iterations is small 
#'   (less than 20% by default), the resulting confidence interval 
#'   on abundance is 
#'   probably valid.  If the proportion of non-convergent iterations 
#'   is not small (exceeds 20% by default), a warning is issued.  
#'   The print method (\code{print.abund}) is the routine that  issues this 
#'   warning. The warning can be 
#'   turned off by setting \code{maxBSFailPropForWarning} in the 
#'   print method to 1.0, or by modifying the code in \code{RdistanceControls()}
#'   to re-set the default threshold and storing the modified 
#'   function in your \code{.GlobalEnv}.  Additional iterations may be needed 
#'   to achieve an adequate number. Check number of convergent iterations by 
#'   counting non-NA rows in output data frame 'B'.  
#'   
#' @section Missing Transect Lengths:
#' 
#'   \bold{Line transects}: The transect length column of \code{siteData} can contain missing values. 
#'   NA length transects are equivalent
#'   to 0 [m] transects and do not count toward total surveyed units.  NA length
#'   transects are handy if some off-transect distance observations should be included
#'   when estimating the distance function, but not when estimating abundance. 
#'   To do this, include the "extra" distance observations in the detection data frame, with valid
#'   site IDs, but set the length of those site IDs to NA in the site data frame. 
#'   Group sizes associated with NA length transects are dropped and not counted toward density
#'   or abundance. Among other things, this allows estimation of abundance on one 
#'   study area using off-transect distance observations from another.  
#'   
#'   \bold{Point transects}: Point transects do not have length. The "length" of point transects
#'   is the number of points on the transect. \code{Rdistance} treats individual points as independent 
#'   and bootstrap resamples them to estimate variance. To include distance observations
#'   from some points but not the number of targets seen, include a separate "length" column 
#'   in the site data frame with NA for the "extra" points. Like NA length line transects, 
#'   NA "length" point transects are dropped from the count of points and group sizes on these 
#'   transects are dropped from the counts of targets.  This allows users to estimate their distance 
#'   function on one set of observations while inflating counts from another set of observations.  
#'   A transect "length" column is not required for point transects. Values in the \code{lengthColumn}
#'   do not matter except for NA (e.g., a column of 1's mixed with NA's is acceptable). 
#' 
#'   
#' @return An 'abundance estimate' object, which is a list of
#'   class \code{c("abund", "dfunc")}, containing all the components of a "dfunc"
#'   object (see \code{\link{dfuncEstim}}), plus the following: 
#'   
#'   \item{density}{Estimated density on the sampled area with units. The \emph{effectively}
#'   sampled area is 2*L*ESW (not 2*L*w.hi). Density has squared units of the 
#'   requested output units.  Convert density to other units with  
#'   \code{units::set_units(x$density, "<units>").}} 
#'   
#'   \item{n.hat}{Estimated abundance on the study area (if \code{area} >
#'   1) or estimated density on the study area (if \code{area} = 1), without units.}
#'  
#'   \item{n}{The number of detections (not individuals, unless all group sizes = 1) 
#'   on non-NA length transects
#'   used to compute density and abundance.}
#'   
#'   \item{n.seen}{The total number of individuals seen on transects with non-NA
#'   length. Sum of group sizes used 
#'   to estimate density and abundance.}
#'  
#'   \item{area}{Total area of inference in squared output units.}
#'   
#'   \item{surveyedUnits}{The total length of sampled transect with units. This is the sum 
#'   of the \code{lengthColumn} column of \code{siteData}. }
#'   
#'   \item{avg.group.size}{Average group size on transects with non-NA length transects.}
#'   
#'   \item{rng.group.size}{Minimum and maximum groupsizes observed on non-NA length transects.}
#'   
#'   \item{effDistance}{A vector containing effective sample distance.  If covariates
#'   are not included, length of this vector is 1 because effective sampling distance 
#'   is constant over detections. If covariates are included, this vector has length
#'   equal to the number of detections (i.e., \code{x$n}).  This vector was produced 
#'   by a call to \code{effectiveDistance()} with \code{newdata} set to NULL.}
#'   
#'   \item{n.hat.ci}{A vector containing the lower and upper limits of the 
#'   bias corrected bootstrap confidence interval for
#'   abundance. } 
#'   
#'   \item{density.ci}{A vector containing the lower and upper limits of the 
#'   bias corrected bootstrap confidence interval for
#'   density, with units.
#'   }
#'
#'   \item{effDistance.ci}{A vector containing the lower and upper limits of the 
#'   bias corrected bootstrap confidence interval for \emph{average}
#'   effective sampling distance.
#'   }
#'   
#'   \item{B}{A data frame containing bootstrap values of coefficients, 
#'   density, and effective distances.  Number of rows is always 
#'   \code{R}, the requested number of bootstrap 
#'   iterations.  If a particular iteration did not converge, the
#'   corresponding row in \code{B} is \code{NA} (hence, use 'na.rm = TRUE' 
#'   when computing summaries). Columns 1 through \code{length(coef(dfunc))}
#'   contain bootstrap realizations of the distance function's coefficients. 
#'   The second to last column contains bootstrap values of
#'   density (with units).  The last column of B contains bootstrap 
#'   values of effective sampling distance or radius (with units). If the 
#'   distance function contains covariates,
#'   the effective sampling distance column is the average 
#'   effective distance over detections 
#'   used during the associated bootstrap iteration. }
#'   
#'   \item{nItersConverged}{The number of bootstrap iterations that converged.  }
#'   
#'   \item{alpha}{The (scalar) confidence level of the
#'   confidence interval for \code{n.hat}.} 
#'
#'   
#' @references Manly, B.F.J. (1997) \emph{Randomization, bootstrap, and 
#'   Monte-Carlo methods in biology}, London: Chapman and Hall.
#'   
#'   Buckland, S.T., D.R. Anderson, K.P. Burnham, J.L. Laake, D.L. Borchers,
#'    and L. Thomas. (2001) \emph{Introduction to distance sampling: estimating
#'    abundance of biological populations}. Oxford University Press, Oxford, UK.
#'   
#' @seealso \code{\link{dfuncEstim}}, \code{\link{autoDistSamp}}.
#' 
#' @examples
#' # Load example sparrow data (line transect survey type)
#' data(sparrowDetectionData)
#' data(sparrowSiteData)
#' 
#' # Fit half-normal detection function
#' dfunc <- dfuncEstim(formula=dist ~ groupsize(groupsize)
#'                     , detectionData=sparrowDetectionData
#'                     , likelihood="halfnorm"
#'                     , w.hi=units::set_units(100, "m")
#'                     )
#' 
#' # Estimate abundance given a detection function
#' # No variance on density or abundance estimated here 
#' # due to time constraints.  Set ci=0.95 (or another value)
#' # to estimate bootstrap variances on ESW, density, and abundance.
#' 
#' fit <- abundEstim(dfunc
#'                 , detectionData = sparrowDetectionData
#'                 , siteData = sparrowSiteData
#'                 , area = units::set_units(4105, "km^2")
#'                 , ci = NULL
#'                 )
#'          
#' @keywords model
#' @export
#' @importFrom stats coef qnorm pnorm quantile
#' @importFrom graphics lines par
#' @importFrom utils txtProgressBar setTxtProgressBar

abundEstim <- function(dfunc
                     , detectionData
                     , siteData
                     , area=NULL
                     , singleSided = FALSE
                     , ci=0.95
                     , R=500
                     , lengthColumn = "length"
                     , plot.bs=FALSE
                     , showProgress=TRUE
                     , control = RdistanceControls()
                     ){

  # A note on 'bysite' ----
  # I left vestages of the 'bysite' code in this function because I may want 
  # to re-include bysite capabilities in a future release.  For now, can't do
  # bysite = TRUE.
  bySite=FALSE
  
  # Check for transectID columns (specified in dfuncEstim) ----
  siteID.cols <- dfunc$siteID.cols
  if(!all(siteID.cols %in% names(detectionData))) {
    mess <- paste0("Transect ID column(s) '"
                   , paste( siteID.cols[!(siteID.cols %in% names(detectionData))], collapse = ", ")
                   , "' not found in data frame "
                   , deparse(substitute(detectionData))
                   )
    stop(mess)
  }
  
  if(!all(siteID.cols %in% names(siteData))) {
    mess <- paste0("Transect ID column(s) '"
                   , paste( siteID.cols[!(siteID.cols %in% names(siteData))], collapse = ", ")
                   , "' not found in data frame "
                   , deparse(substitute(siteData))
    )
    stop(mess)
  }
  
  # It is okay to have NA in distance column

  # Check for NA in transect id columns of detectionData
  if(any(is.na(detectionData[, siteID.cols]))){
    stop(paste0("Please remove NA rows from transect ID column(s) "
              , paste(siteID.cols, collapse = ", ")
              , " in data frame "
              , deparse(substitute(detectionData))
              , "."))
  }
  
  # Check for NA in transect id columns of siteData
  if(any(is.na(siteData[, siteID.cols]))){
    stop(paste0("Please remove NA rows from transect ID column(s) "
                , paste(siteID.cols, collapse = ", ")
                , " in data frame "
                , deparse(substitute(siteData))
                , "."))
  }

  # Check for presence of length column ----
  if(!dfunc$pointSurvey){
    if(!(lengthColumn %in% names(siteData))){
      stop(paste0("Transect length column, '"
                  , lengthColumn
                  , "', is not present in data frame "
                  , deparse(substitute(siteData))
                  , ". Did you forget to specify 'lengthColumn'?"
                  )) 
    }
  }
  
  # Check if siteIDs are unique
  if((d <- anyDuplicated(siteData[, siteID.cols])) > 0){
    stop(paste0("Site IDs must be unique in data frame "
              , deparse(substitute(siteData))
              , ". Values on row "
              , d
              , " are duplicated somewhere."))
  }
  
  # ---- Check CI - bySite combination ----
  # (jdc) this function isn't setup to generate bootstrap CIs of site-level abundance
  # (jdc) so allowing bySite=TRUE and !is.null(ci) is misleading, and throws an error
  # (jdc) in the bias-correction stage because ans$n.hat has the wrong dimensions
  if (bySite & !is.null(ci)) {
    warning("The CI option in this function generates CIs for an overall
            abundance estimate, not CIs for site-level abundance estimates.
            If you specify bySite = TRUE, ci must equal NULL.
            ci has been set to NULL.")
    ci <- NULL
  }
  
  
  
  if (plot.bs) {
    like <- match.fun(paste(dfunc$like.form, ".like", sep = ""))
    par( xpd=TRUE )
    plotObj <- plot(dfunc)
  }
  

  
  # Site-specific abundance
  # No option to bootstrap, and simplified data.frame returned from estimateN
  # is all that needs to be returned
  
  if (bySite) {
    
    # Estimate abundance
    # THIS SHOULD CALL THE NEW PREDICT METHOD
    # abund <- estimateN(dfunc=dfunc, detectionData=detectionData, 
    #                    siteData=siteData, area=area, bySite=bySite)
    # 
    # ans <- abund
    
  } else {
    # Overall abundance, possibly with bootstrap
    
    # ---- Make new composite siteID, and name it 'siteID' for convenience in bootstrap ----
    # Next version, use tidyr::unite() here
    # Here, we convert to data.frames in case detectionData is a tibble. i.e. this method only works for true data.frames
    siteID <- as.character(data.frame(detectionData)[, siteID.cols[1] ])
    if( length(siteID.cols) > 1){
      for(j in 2:length(siteID.cols) ){
        siteID <- paste(siteID, data.frame(detectionData)[, siteID.cols[j] ], sep = "_")
      }
    }
    detectionData$siteID <- siteID
    
    siteID <- as.character(data.frame(siteData)[, siteID.cols[1] ])
    if( length(siteID.cols) > 1){
      for(j in 2:length(siteID.cols) ){
        siteID <- paste(siteID, data.frame(siteData)[, siteID.cols[j] ], sep = "_")
      }
    }
    siteData$siteID <- siteID
    
    # ---- Set number of sides ----
    surveyedSides <- as.numeric(!singleSided) + 1  # 2 = dbl sided; 1 = single
    
    # ---- Merge detections and sites, make sure to preserve sites without detections ----
    # Sites without detections get assigned distance = NA, which is okay. dfuncEstim takes 
    # missing distances (i.e., it drops them)
    
    mergeData <- merge(detectionData, siteData, by="siteID", all.y = TRUE)
    
    abund <- estimateN(dfunc = dfunc
                      , data = mergeData
                      , area = area
                      , surveyedSides = surveyedSides
                      , lengthColumn = lengthColumn
                      , control = control
                      )
    
    # store output returned by this function
    # (will be added to in later sections)
    
    # dfunc is already stored in abund returned above, 
    # but the print.abund and print.dfunc were not working
    # when I just stored ans <- abund.  This is clunky, but resolves the issue.
    ans <- dfunc
    ans$density <- abund$density
    ans$n.hat <- abund$abundance
    ans$n <- abund$n.groups
    ans$n.seen <- abund$n.seen
    ans$area <- abund$area
    ans$surveyedUnits <- abund$surveyedUnits
    ans$avg.group.size <- abund$avg.group.size
    ans$rng.group.size <- abund$range.group.size
    # Note: could call effectiveDistance(dfunc) here, but that does the 
    # integration over and is slightly less efficient
    if(dfunc$pointSurvey){
      ans$effDistance <- sqrt(abund$w^2 * abund$pDetection)  # for points
    } else {
      ans$effDistance <- abund$pDetection * abund$w  # for lines
    }
    
    if (!is.null(ci)) {
      # Compute bootstrap CI by resampling transects
      
      g.x.scl.orig <- dfunc$call.g.x.scl  # g(0) or g(x) estimate
      
      B <- rep(NA, R)  # preallocate space for bootstrap replicates of nhat
      coefCols <- 1:length(coef(dfunc))
      coefB <- data.frame(matrix(B, nrow=R, ncol=max(coefCols)))
      names(coefB) <- names(coef(dfunc))
      B <- data.frame(
               coefB
             , density = B
             , effDistance = B)
      
      # now including utils as import,
      if(showProgress){
        pb <- txtProgressBar(1, R, style=3)
        cat("Computing bootstrap confidence interval on N...\n")
      }
      
      # Deal with factors in the model
      factorsInModel <- attr(terms(dfunc$model.frame), "factors")
      if( !is.null(factorsInModel) && length(factorsInModel)>0 ){
        factorNames <- dimnames(factorsInModel)[[2]]
        factorLevels <- vector("list", length(factorNames))
        names(factorLevels) <- factorNames
        for(j in 1:length(factorNames)){
          factorLevels[[j]] <- unique(dfunc$model.frame[, factorNames])
        }
      } else {
        factorNames <- NULL
      }
      

      # Bootstrap
      lastConvergentDfunc <- dfunc
      convergedCount <- 0
      nTransects <- nrow(siteData)
      for (i in 1:R) {
        # sample rows, with replacement, from transect data
        new.siteData <- siteData[sample(nTransects, nTransects, replace=TRUE),,drop=FALSE]
        
        new.trans <- as.character(new.siteData$siteID)  # which transects were sampled?
        trans.freq <- data.frame(table(new.trans))  # how many times was each represented in the new sample?
        
        # subset distance data from these transects
        if ( inherits(new.siteData$siteID, "factor") ) {
          # this never happens because we convert siteID to character before bootstrap loop
          new.trans <- unique(droplevels(new.siteData$siteID))
        } else {
          new.trans <- unique(new.siteData$siteID)
        }
        new.detectionData <- detectionData[detectionData$siteID %in% new.trans, ]  # this is incomplete, since some transects were represented > once
        
        # It is possible that we detected no targets on these BS transects. 
        # Fine, but we need to check cause the rep statement below throws an error 
        # if there are no detections
        if(nrow(new.detectionData) > 0) {
          # If we have detections on this iteration, replicate according 
          # to frequency of transects in new BS sample
          # First, merge to add Freq column to indicate how many times to repeat each row
          red <- merge(new.detectionData, trans.freq, by.x = "siteID", by.y = "new.trans")
          # expand this reduced set by replicating rows
          new.detectionData <- red[rep(seq.int(1, nrow(red)), red$Freq), -ncol(red)]
        }
        
        # And merge on site-level covariates
        # Not needed if no covars, but cost in time should be negligible
        # Need to merge now to get covars in siteData that has unduplicated siteID.
        new.mergeData <- merge(new.detectionData, siteData, by="siteID", all.x = TRUE)

        # If factor in the model and we get only fewer than all levels, 
        # the model "fails to converge".  Toss it. 
        if(!is.null(factorNames)){
          bsFactLevsOK <- rep(TRUE, length(factorLevels))
          for( j in 1:length(factorLevels)){
            bsFactorLevels <- unique(new.mergeData[, names(factorLevels)[j]])
            bsFactLevsOK[j] <- length(bsFactorLevels) == length(factorLevels[[j]])
          }
          if( any(!bsFactLevsOK) ){
            # rather than simply 'next' here, set new.mergedata to NULL
            # so that progress bar updates at bottom of loop
            # next
            new.mergeData <- data.frame(NULL)
          }
        }
        
        #update g(0) or g(x) estimate.
        if (is.data.frame(g.x.scl.orig)) {
          g.x.scl.bs <- g.x.scl.orig[sample(1:nrow(g.x.scl.orig), 
                                            replace = TRUE), ]
        } else {
          g.x.scl.bs <- g.x.scl.orig
        }
        
        
        # Eventually, we will get smoothed the function into 
        # dfuncEstim or one function.
        # This if statement is kluncky
        if( dfunc$like.form == "smu" ){
          dfunc.bs <- dfuncSmu(dfunc$formula, 
                               detectionData=new.mergeData,
                               w.lo = dfunc$w.lo,
                               w.hi = dfunc$w.hi,
                               bw = dfunc$fit$call[["bw"]],
                               adjust = dfunc$fit$call[["adjust"]], 
                               kernel = dfunc$fit$call[["kernel"]],
                               x.scl = dfunc$call.x.scl, 
                               g.x.scl = g.x.scl.bs,
                               observer = dfunc$call.observer,
                               pointSurvey = dfunc$pointSurvey, 
                               warn = FALSE )
        } else if(nrow(new.mergeData) > 0){
          dfunc.bs <- dfuncEstim(formula = dfunc$formula,  
                               detectionData = new.mergeData,
                               likelihood = dfunc$like.form, 
                               w.lo = dfunc$w.lo,
                               w.hi = dfunc$w.hi,
                               expansions = dfunc$expansions, 
                               series = dfunc$series,
                               x.scl = dfunc$call.x.scl, 
                               g.x.scl = g.x.scl.bs,
                               observer = dfunc$call.observer,
                               pointSurvey = dfunc$pointSurvey, 
                               outputUnits = dfunc$outputUnits,
                               warn = FALSE)
        } else {
          dfunc.bs <- list(convergence = 1)
        }
        
        # Store ESW if it converged
        if(dfunc$like.form == "smu" || dfunc.bs$convergence == 0) {
          convergedCount <- convergedCount + 1

          # Note: there are duplicate siteID's in newSiteData.  This is okay
          # because number of unique siteIDs is never computed in estimateN. 
          # Number of sites in estiamteN is nrow(newSiteData)
          
          # Estimate Bootstrap abundance
          abund.bs <- estimateN(dfunc = dfunc.bs
                             , data = new.mergeData
                             , area = area
                             , surveyedSides = surveyedSides
                             , lengthColumn = lengthColumn
                             , control = control
          )
          
          if(dfunc$pointSurvey){
            efd <- abund.bs$w * sqrt(abund.bs$pDetection)  # for points
          } else {
            efd <- abund.bs$w * abund.bs$pDetection # for lines
          }
          
          B[i,coefCols] <- coef(dfunc.bs)
          B$effDistance[i] <- mean(efd, na.rm = TRUE)
          B$density[i] <- abund.bs$density

          if (plot.bs ) {
            lines(dfunc.bs
                , newdata = plotObj$predCovValues
                , col = "blue"
                , lwd = 0.5
                )  
          }
          
          
        }  # end if smu or dfunc.bs converged

        if (showProgress){
          setTxtProgressBar(pb, i)
        } 
        
      }  # end bootstrap
      
      
      # close progress bar  
      if (showProgress) {
        close(pb)
      }
      
      # plot red line of original fit again (over bs lines)
      if (plot.bs) {
        lines(dfunc
              , newdata = plotObj$predCovValues
              , col = "red"
              , lwd = 3)
      } 

      # Density units do not get assigned to B. Assign them now      
      B$density <- units::set_units(B$density, units(ans$density), mode = "standard")
      B$effDistance <- units::set_units(B$effDistance, units(ans$effDistance), mode = "standard")
      
      # function to calculate CI from bootstrap replicates using bias-corrected bootstrap method in Manly text
      bcCI <- function(x.bs, x, ci){
        p <- mean(x.bs > x, na.rm = TRUE)
        z.0 <- qnorm(1 - p)
        z.alpha <- qnorm(1 - ((1 - ci)/2))
        p.L <- pnorm(2 * z.0 - z.alpha)
        p.H <- pnorm(2 * z.0 + z.alpha)
        ans <- quantile(x.bs[!is.na(x.bs)], p = c(p.L, p.H))
        names(ans) <- paste0(100*c( (1 - ci)/2, 1 - (1-ci)/2 ), "%")
        ans
      }
      
      # abundance estimates are unitless, counts
      abundEsts <- units::drop_units(B$density * ans$area)

      ans$n.hat.ci <- bcCI(abundEsts, ans$n.hat, ci)
      ans$density.ci <- bcCI(B$density, ans$density, ci)
      ans$effDistance.ci <- bcCI(B$effDistance, mean(ans$effDistance, na.rm = TRUE), ci)
      ans$B <- B
      ans$nItersConverged <- convergedCount
      
      if (!(dfunc$like.form=="smu") && (convergedCount < R) && showProgress){
        cat(paste( R - convergedCount, "of", R, "iterations did not converge.\n"))
      }
      
    } else {
      # Don't compute CI if ci is null
      ans$B <- NULL
      ans$density.ci <- NULL
      ans$n.hat.ci <- NULL
      ans$effDistance.ci <- NULL
      ans$nItersConverged <- NULL
    }  # end else
    
    
    # Output
    ans$alpha <- ci
    class(ans) <- c("abund", class(dfunc))
    
    
  }  # end else of if (bySite)
  
  
  
  
  return(ans)
  
  
  
}  # end function
