#' @title Estimate abundance from distance-sampling data
#'   
#' @description Estimate abundance (or density) given an estimated detection
#'   function and supplemental information on observed group sizes, transect
#'   lengths, area surveyed, etc.  Also computes confidence intervals of
#'   abundance (or density) using the bias corrected bootstrap method.
#'   
#' @param dfunc An estimated 'dfunc' object produced by \code{dfuncEstim}.
#' 
#' @inheritParams dfuncEstim 
#' 
#' @param area A scalar containing total area of 
#' inference. Commonly, this is study area size.  
#' If \code{area} is NULL (the default), 
#' area is set to 1 square unit of the output units. The default
#' produces abundance estimates that equal density estimates
#' (density estimates are always produced). 
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
#' @param ci A scalar indicating the confidence level of confidence intervals. 
#'   Confidence intervals are computed using the bias corrected bootstrap
#'   method. If \code{ci = NULL}, confidence intervals are not computed.
#'   
#' @param R The number of bootstrap iterations to conduct when \code{ci} is not
#'   NULL.
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
#  DEFUNCT BYSITE PARAM:
#' @param bySite A logical scalar indicating whether to compute site-level
#'   estimates of abundance. The default (\code{bySite=FALSE}) returns only one
#'   overall abundance estimate. This routine does not calculate confidence
#'   intervals for these site-level abundance estimates, so \code{ci} is set to
#'   \code{NULL} if \code{bySite = TRUE}. See \code{\link{estimateN}}.
#'   
#' @details The abundance estimate for line transect surveys (if no covariates
#'    are included in the detection function) is 
#'    \deqn{N =\frac{n(A)}{2(ESW)(L)}}{%
#'          N = n*A / (2*ESW*L)} 
#'    where \emph{n} is total number of sighted individuals 
#'   (i.e., \code{sum(group.sizes)}), \emph{L} is the total length of 
#'   surveyed transect,
#'   and \emph{ESW} is effective strip width
#'   computed from the estimated distance function (i.e., \code{ESW(dfunc)}).
#'   
#'   The abundance estimate for point transect surveys (if no covariates are
#'   included) is 
#'    \deqn{N =\frac{n(A)}{\pi ESR^2 P}}{%
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
#'   simply skips the intration, effectively ignoring these 
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
#'   
#' @return An 'abundance estimate' object, which is a list of
#'   class \code{c("abund", "dfunc")}, containing all the components of a "dfunc"
#'   object (see \code{dfuncEstim}), plus the following: 
#'   
#'   \item{density}{Estimated density on the sampled area. The \emph{effectively}
#'   sampled area is 2*L*ESW (not 2*L*w.hi). Density has squared units of the 
#'   requested output units.  E.g., if \code{outputUnits} = "km" in the call 
#'   to \code{dfuncEstim}}, units on density are 1/km^2. No flexibility exists
#'   to have obtain different distance and density units (e.g., "m" and "ha", or "km" and 
#'   "acre").  To convert density to other units, use 
#'   \code{units::set_units(x$density, "<units>")}. 
#'   
#'  \item{abundance}{Estimated abundance in the study area (if \code{area} >
#'  1) or estimated density in the study area (if \code{area} = 1).}
#'  
#'   \item{n}{The number of detections
#'  (not individuals, unless all group sizes = 1) used in the estimate of
#'  abundance.}
#'  
#'   \item{area}{Total area of inference. Study area size}
#'   
#'   \item{effDistance}{Effective strip width for line-transects, effective
#'   radius for point-transects.  Both derived from \code{dfunc}. 
#'   See \code{\link{ESW}} or \code{\link{EDR}} for formulas.}
#'   
#'   \item{n.sites}{Total number of transects for line-transects, 
#'   total number of points for point-transects.}
#'   
#'   \item{tran.len}{Total transect length. NULL for point-transects.}
#'   
#'   \item{avg.group.size}{Average group size}
#'   
#'   \item{n.hat.ci}{The bias corrected bootstrap confidence interval for
#'   abundance (i.e., \code{n.hat}).  The names of this component 
#'   give the quantiles of the bootstrap distribution used during computation of
#'   the bias corrected interval.} 
#'   
#'   \item{density.ci}{The bias corrected bootstrap confidence interval for
#'   density (i.e., \code{density}).
#'   }
#'
#'   \item{effDistance.ci}{The bias corrected bootstrap confidence interval for
#'   effective sampling distance (i.e., \code{effDistance}).
#'   }
#'   
#'   \item{B}{A data frame containing bootstrap density and effective distance 
#'   values.  Number of rows is always \code{R}, the number of bootstrap 
#'   requested iterations.  If a particular iteration did not converge, the
#'   corresponding row in \code{B} is all \code{NA} (e.g., use 'na.rm = TRUE' 
#'   when computing summaries). One column of the data frame 
#'   contains bootstrap values of \code{density}.  Another column contains bootstrap 
#'   values of \code{effDistance}. Bootstrap abundance values are 'B$density' * area.}
#'   
#'   \item{nItersConverged}{The number of bootstrap iterations that converged.  }
#'   
#'   \item{alpha}{The (scalar) confidence level of the
#'   confidence interval for \code{n.hat}.} 
#'
#   MOVE THIS DOCUMENTATION TO BYSITE ROUTINE
#
#   If \code{bySite} is TRUE, a data frame containing site-level 
#   estimated abundance.  The data frame is an exact copy of \code{siteData}
#   with the following columns tacked onto the end:
#    
#   \item{effDist}{The effective sampling distance at the site.  For line-
#   transects, this is ESW at the site.  For points, this is EDR. } 
#   \item{pDetection}{Average probability of detection at the site. 
#   If only site-level covariates appear in the distance function, 
#   pDetection is constant within a site. When detection-level 
#   covariates are present, pDetection is the average at the site.}
#   \item{observedCount}{The total number of individuals detected at a site.}
#   \item{abundance}{Estimated abundance at the site. This is the sum
#   of inflated group sizes at the site. i.e., each group size 
#   at the site is divided by its pDetection, and then summed.    }
#   \item{density}{Estimated density at the site. This is abundance 
#   at the site divided by the sampled area at the site.  E.g., for 
#   line transects, this is abundance divided by \eqn{2*w*length}. For 
#   points, this is abundance divided by \eqn{pi*w^2}.}
#   \item{effArea}{The effective area sampled at the site. This could be used
#   as an offset in a subsequent linear model. For 
#   line transects, this is \eqn{2*ESW*length}. For 
#   points, this is \eqn{pi*EDR^2}.}
#   
#'   
#' @author Trent McDonald, \email{trent@mcdonalddatasciences.com}\cr 
#'   Aidan McDonald, \email{aidan@mcdcentral.org}\cr 
#'   Jason Carlisle, Wyoming Game and Fish Department, 
#'   \email{jason.carlisle@wyo.gov}
#'   
#' @references Manly, B.F.J. (1997) \emph{Randomization, bootstrap, and 
#'   monte-carlo methods in biology}, London: Chapman and Hall.
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
#' dfunc <- dfuncEstim(formula=dist~1
#'                     , detectionData=sparrowDetectionData
#'                     , likelihood="halfnorm"
#'                     , w.hi=units::set_units(100, "m")
#'                     , pointSurvey=FALSE)
#' 
#' # Estimate abundance given a detection function
#' # Note, a person should do more than R=20 iterations
#' fit <- abundEstim(dfunc
#'                 , detectionData=sparrowDetectionData
#'                 , siteData=sparrowSiteData
#'                 , R=20
#'                 , ci=0.95
#'                 , area=units::set_units(10000, "m^2")
#'                 , plot.bs=TRUE
#'                 , bySite=FALSE)
#' 
#' # Print results
#' fit
#'          
#' @keywords model
#' @export
#' @importFrom stats coef qnorm pnorm quantile
#' @importFrom graphics lines par
#' @importFrom utils txtProgressBar setTxtProgressBar

abundEstim <- function(dfunc, 
                       detectionData, 
                       siteData,
                       area=NULL, 
                       ci=0.95, 
                       R=500, 
                       plot.bs=FALSE, 
                       bySite=FALSE,
                       showProgress=TRUE, 
                       control = RdistanceControls()){
  
  # Stop and print error if key columns of detectionData or siteData are missing or contain NAs
  if(!("siteID" %in% names(detectionData))) stop("There is no column named 'siteID' in your detectionData.")

  if(!("siteID" %in% names(siteData))) stop("There is no column named 'siteID' in your siteData.")
  if(!dfunc$pointSurvey){
    if(!("length" %in% names(siteData))) stop("There is no column named 'length' in your siteData.") 
  }
  
  if(any(is.na(detectionData$dist))) stop("Please remove rows for which detectionData$dist is NA.")
  if(any(is.na(detectionData$siteID))) stop("Please remove rows for which detectionData$siteID is NA.")

  if(any(is.na(siteData$siteID))) stop("Please remove NA's from siteData$siteID.")
  if(!dfunc$pointSurvey){
    if(any(is.na(siteData$length))) stop("Please remove NA's from siteData$length.") 
  }
  
  # Stop and print error if siteIDs are not unique
  if(anyDuplicated(siteData$siteID) > 0){
    stop("Site IDs must be unique.")
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
    par(xpd=TRUE)
    plot(dfunc)
    # if( dfunc$pointSurvey ){
    #   lines(dfunc, col="red", lwd=3)
    # } 
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
    
    # Estimate abundance
    if( dfunc$pointSurvey ){
      totSurveyedUnits <- nrow(siteData)
    } else {
      totSurveyedUnits <- sum(siteData$length)
    }
    abund <- estimateN(dfunc = dfunc
                      , surveyedUnits = totSurveyedUnits
                      , area = area
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
    ans$area <- abund$area
    ans$surveyedUnits <- abund$surveyedUnits
    ans$avg.group.size <- abund$avg.group.size
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
      B <- data.frame(density = B, effDistance = B)
      
      # now including utils as import,
      if(showProgress){
        pb <- txtProgressBar(1, R, style=3)
        cat("Computing bootstrap confidence interval on N...\n")
      }

      # Bootstrap
      lastConvergentDfunc <- dfunc
      convergedCount <- 0
      for (i in 1:R) {
        # sample rows, with replacement, from transect data
        new.siteData <- siteData[sample(nrow(siteData), nrow(siteData), replace=TRUE),,drop=FALSE]
        
        new.trans <- as.character(new.siteData$siteID)  # which transects were sampled?
        trans.freq <- data.frame(table(new.trans))  # how many times was each represented in the new sample?
        
        # subset distance data from these transects
        if ( class(new.siteData$siteID) == "factor" ) {
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
          # to freqency of transects in new BS sample
          # First, merge to add Freq column to indicate how many times to repeat each row
          red <- merge(new.detectionData, trans.freq, by.x = "siteID", by.y = "new.trans")
          # expand this reduced set by replicating rows
          new.detectionData <- red[rep(seq.int(1, nrow(red)), red$Freq), -ncol(red)]
        }
        
        # And merge on site-level covariates
        # Not needed if no covars, but cost in time should be negligible
        # Need to merge now to get covars in siteData that has unduplicated siteID.
        new.mergeData <- merge(new.detectionData, siteData, by="siteID")

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
        } else {
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
        }
        
        # Store ESW if it converged
        if(dfunc$like.form == "smu" || dfunc.bs$convergence == 0) {
          convergedCount <- convergedCount + 1

          # Note: there are duplicate siteID's in newSiteData.  This is okay
          # because number of unique siteIDs is never computed in estimateN. 
          # Number of sites in estiamteN is nrow(newSiteData)
          
          # Estimate Bootstrap abundance
          if( dfunc$pointSurvey ){
            totSurveyedUnits <- nrow(new.siteData)
          } else {
            totSurveyedUnits <- sum(new.siteData$length)
          }
          abund.bs <- estimateN(dfunc = dfunc.bs
                             , surveyedUnits = totSurveyedUnits
                             , area = area
                             , control = control
                             )
          if(dfunc$pointSurvey){
            efd <- abund.bs$w * sqrt(abund.bs$pDetection)  # for points
          } else {
            efd <- abund.bs$w * abund.bs$pDetection # for lines
          }
          
          B$effDistance[i] <- efd
          B$density[i] <- abund.bs$density

          if (plot.bs ) {
            lines(dfunc.bs, col = "blue", lwd = 0.5)  
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
        lines(dfunc, col = "red", lwd = 3)
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
      ans$effDistance.ci <- bcCI(B$effDistance, ans$effDistance, ci)
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
