#' @title Estimate abundance from distance-sampling data
#'   
#' @description Estimate abundance (or density) given an estimated detection
#'   function and supplemental information on observed group sizes, transect
#'   lengths, area surveyed, etc.  Also computes confidence intervals of
#'   abundance (or density) using the bias corrected bootstrap method.
#'   
#' @param dfunc An estimated 'dfunc' object produced by \code{dfuncEstim}.
#' 
#  Inherit 'detectionData' and 'siteData' documentation
#' @inheritParams dfuncEstim 
#' 
#'     
#' @param area Total study area size. If \code{area} is NULL (the default), 
#'   area is set to 1 square unit of the output units. The default
#'   is mostly for the case when interest lies in density. If \code{area}
#'   is not NULL, it must have measurement units. 
#'   The units on \code{area} must be convertible
#'   to squared output units (if output units are "m", but be convertable 
#'   to "m^2"). Hence, units on \code{area} must be two-dimensional.
#'   Units of "km^2", "cm^2", "ha", "m^2", "acre", "mi^2", and 
#'   others all work.  If the \code{units}
#'   package can convert \code{area} to the squared output units, 
#'   it can be used. 
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
#' @param bySite A logical scalar indicating whether to compute site-level
#'   estimates of abundance. The default (\code{bySite=FALSE}) returns only one
#'   overall abundance estimate. This routine does not calculate confidence
#'   intervals for these site-level abundance estimates, so \code{ci} is set to
#'   \code{NULL} if \code{bySite = TRUE}. See \code{\link{estimateN}}.
#'   
#' @details The abundance estimate for line transect surveys (if no covariates
#'    are included in the detection function) is \deqn{N =
#'   \frac{n.indiv(area)}{2(ESW)(tot.trans.len)}}{N = n.indiv*area /
#'   (2*ESW*tot.trans.len)} where \code{n.indiv} is either \code{avg.group.size * n} or
#'   \code{sum(group.sizes)}, and \code{ESW} is the effective strip width
#'   computed from the estimated distance function (i.e., \code{ESW(dfunc)}).
#'
#'  Setting \code{plot.bs=FALSE} and \code{showProgress=FALSE} 
#'     suppresses all intermediate output.  This is good when calling 
#'     \code{abundEstim} from within other functions or during simulations.
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
#'   uses the last convergent distance function to estimate 
#'   abundance on the iteration rather than ignore these 
#'   non-convergent iterations and continue. This approach includes 
#'   all variation associated with counts, but may not include 
#'   all the variation associated with distance function estimation.  
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
#'   function in your \code{.GlobalEnv}.  
#'   
#'   
#' @return If \code{bySite} is FALSE, an 'abundance estimate' object, a list of
#'   class \code{c("abund", "dfunc")}, containing all the components of a "dfunc"
#'   object (see \code{dfuncEstim}), plus the following: 
#'   
#'   \item{density}{Estimated density on the sampled area. The \emph{effectively}
#'   sampled area is 2*L*ESW (not 2*L*w.hi). Density has squared units of the 
#'   requested output units.  E.g., if \code{outputUnits} = "km" in the call 
#'   to \code{dfuncEstim}}, units on density are 1/km^2. There exists no flexibility
#'   to have differring distance and density units (e.g., "m" and "ha", or "km" and 
#'   "acre").  To convert density to other units, use 
#'   \code{units::set_units(x$density, "<units>")}. 
#'  \item{abundance}{Estimated abundance in the study area (if \code{area} >
#'  1) or estimated density in the study area (if \code{area} = 1).}
#'   \item{n}{The number of detections
#'  (not individuals, unless all group sizes = 1) used in the estimate of
#'  abundance.}
#'   \item{area}{Total area of inference. Study area size}
#'   \item{esw}{Effective strip width for line-transects, effective
#'   radius for point-transects.  Both derived from \code{dfunc}. 
#'   See \code{\link{ESW}}} or \code{\link{EDR}} for formulas.
#'   \item{n.sites}{Total number of transects for line-transects, 
#'   total number of points for point-transects.}
#'   \item{tran.len}{Total transect length. NULL for point-transects.}
#'   \item{avg.group.size}{Average group size}
#'   \item{ci}{The bias corrected bootstrap confidence interval for
#'   \code{n.hat}.  The names of this component give the quantiles of the
#'   bootstrap distribution used to compute the bias corrected interval.} 
#'   \item{B}{A vector or length \code{R} containing all bootstrap estimated
#'   population sizes. If a particular iteration did not converge, the
#'   corresponding entry in \code{B} will be \code{NA}. The bootstrap
#'   distribution of \code{n.hat} can be plotted with \code{hist(x$B)}, where
#'   \code{x} is an 'abundance estimate' object. The confidence interval in
#'   \code{ci} can be reproduced with \code{quantile(x$B[!is.na(x$B)],
#'   p=names(x$ci) )}.}
#'   \item{nItersConverged}{The number of bootstrap iterations that converged.  }
#'   \item{alpha}{The (scalar) confidence level of the
#'   confidence interval for \code{n.hat}.} 
#'   
#'   If \code{bySite} is TRUE, a data frame containing site-level 
#'   estimated abundance.  The data frame is an exact copy of \code{siteData}
#'   with the following columns tacked onto the end:
#'    
#'   \item{effDist}{The effective sampling distance at the site.  For line-
#'   transects, this is ESW at the site.  For points, this is EDR. } 
#'   \item{pDetection}{Average probability of detection at the site. 
#'   If only site-level covariates appear in the distance function, 
#'   pDetection is constant within a site. When detection-level 
#'   covariates are present, pDetection is the average at the site.}
#'   \item{observedCount}{The total number of individuals detected at a site.}
#'   \item{abundance}{Estimated abundance at the site. This is the sum
#'   of inflated group sizes at the site. i.e., each group size 
#'   at the site is divided by its pDetection, and then summed.    }
#'   \item{density}{Estimated density at the site. This is abundance 
#'   at the site divided by the sampled area at the site.  E.g., for 
#'   line transects, this is abundance divided by \eqn{2*w*length}. For 
#'   points, this is abundance divided by \eqn{pi*w^2}.}
#'   \item{effArea}{The effective area sampled at the site. This could be used
#'   as an offset in a subsequent linear model. For 
#'   line transects, this is \eqn{2*ESW*length}. For 
#'   points, this is \eqn{pi*EDR^2}.}
#'   
#'   
#' @author Trent McDonald, WEST Inc.,  \email{tmcdonald@west-inc.com}\cr 
#'   Aidan McDonald, WEST Inc.,  \email{aidan@mcdcentral.org}\cr 
#'   Jason Carlisle, University of Wyoming and WEST Inc., 
#'   \email{jcarlisle@west-inc.com}
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
#' dfunc <- dfuncEstim(formula=dist~1,
#'                     detectionData=sparrowDetectionData,
#'                     likelihood="halfnorm", w.hi=units::as_units(100, "m"), pointSurvey=FALSE)
#' 
#' # Estimate abundance given a detection function
#' # Note, a person should do more than R=20 iterations
#' fit <- abundEstim(dfunc, detectionData=sparrowDetectionData,
#'                   siteData=sparrowSiteData, area=10000, R=20, ci=0.95,
#'                   plot.bs=TRUE, bySite=FALSE)
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
  if(!("dist" %in% names(detectionData))) stop("There is no column named 'dist' in your detectionData.")
  if(!("siteID" %in% names(detectionData))) stop("There is no column named 'siteID' in your detectionData.")
  if(!("groupsize" %in% names(detectionData))) stop("There is no column named 'groupsize' in your detectionData.")
  
  if(!("siteID" %in% names(siteData))) stop("There is no column named 'siteID' in your siteData.")
  if(!dfunc$pointSurvey){
    if(!("length" %in% names(siteData))) stop("There is no column named 'length' in your siteData.") 
  }
  
  if(any(is.na(detectionData$dist))) stop("Please remove rows for which detectionData$dist is NA.")
  if(any(is.na(detectionData$siteID))) stop("Please remove rows for which detectionData$siteID is NA.")
  if(any(is.na(detectionData$groupsize))) stop("Please remove rows for which detectionData$groupsize is NA.")
  
  if(any(is.na(siteData$siteID))) stop("Please remove NA's from siteData$siteID.")
  if(!dfunc$pointSurvey){
    if(any(is.na(siteData$length))) stop("Please remove NA's from siteData$length.") 
  }
  
  # Stop and print error if siteIDs are not unique
  if(anyDuplicated(siteData$siteID) > 0){
    stop("Site IDs must be unique.")
  }
  
  # ---- Measurement units check. ----
  if( !dfunc$pointSurvey ){
    if( !inherits(siteData$length, "units") & control$requireUnits ){
      dfName <- deparse(substitute(siteData))
      stop(paste("Transect length measurement units are required.", 
                 "Assign units to transect length by attaching 'units' package then:\n", 
                 paste0("units(",dfName,"$length)"), "<- '<units of measurment>',\n", 
                 "For example, 'm' (meters) or 'ft' (feet). See units::valid_udunits()"))
    } else if( control$requireUnits ){
      # if we are here, length has units. convert them to units used during estimation.
      # Input dfunc must have a $outputUnits component.
      siteData$length <-  units::set_units(siteData$length, dfunc$outputUnits, mode = "standard")
    }
  }
  
  if( !inherits(area, "units") & control$requireUnits ){
    if( !is.null(area) ){
      # If we are here, area did not come with units, we require units, and it's not null: ERROR
      stop(paste("Study area measurement units are required.", 
                 "Assign units to area by attaching 'units' package then:\n", 
                 "units(area) <- '<units of measurment>'.", 
                 "For example, '<units of measure>' = 'm^2' (sq meters), 'ha' (hectares), 'km^2', or 'acre'.\nSee units::valid_udunits()"))
    }
    # if we are here, area is NULL: Report abundance in 1 square unit of measure
    area <- units::set_units(1, dfunc$outputUnits, mode = "standard")^2
  } else if( control$requireUnits ){
    # if we are here, area has units and it is not null (because you cannot 
    # assign units to NULL) but it could be NA)
    # Convert area units to square of outputUnits.
    # This converts units like "hectare" to "m^2". If cannot convert, and error is thrown here
    squareOutputUnits <- units::set_units(1, dfunc$outputUnits, mode = "standard")^2
    area <- units::set_units(area, squareOutputUnits, mode = "standard")
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
    abund <- estimateN(dfunc=dfunc, detectionData=detectionData, 
                       siteData=siteData, area=area, bySite=bySite)
    
    ans <- abund
    
  } else {
    # Overall abundance, possibly with bootstrap
    
    # Estimate abundance
    abund <- estimateN(dfunc=dfunc, detectionData=detectionData, 
                       siteData=siteData, area=area, bySite=bySite)
    
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
    ans$tran.len <- abund$tran.len
    ans$avg.group.size <- abund$avg.group.size
    # Note: could call effectiveDistance(dfunc) here, but that does the 
    # integration over and is slightly less efficient
    if(dfunc$pointSurvey){
      ans$esw <- sqrt(abund$w^2 * abund$pDetection)  # for points
    } else {
      ans$esw <- abund$pDetection * abund$w  # for lines
    }
    
    
    if (!is.null(ci)) {
      # Compute bootstrap CI by resampling transects
      
      g.x.scl.orig <- dfunc$call.g.x.scl  # g(0) or g(x) estimate
      
      B <- rep(NA, R)  # preallocate space for bootstrap replicates of nhat
      B <- data.frame(density = B, abundance = B)
      
      # now including utils as import,
      if(showProgress){
        pb <- txtProgressBar(1, R, style=3)
        cat("Computing bootstrap confidence interval on N...\n")
      }

      # Bootstrap
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
        if (dfunc$like.form == "smu" || dfunc.bs$convergence == 0) {

          convergedCount <- convergedCount + 1
          
          # Note: there are duplicate siteID's in newSiteData.  This is okay
          # because number of unique siteIDs is never computed in estimateN. 
          # Number of sites in estiamteN is nrow(newSiteData)
          
          # Estimate abundance
          # (jdc) this function isn't setup to generate bootstrap CIs of site-level abundance, so this is always bySite=FALSE
          # (jdc) so bySite=TRUE and !is.null(ci) is perhaps misleading
          abund.bs <- estimateN(dfunc = dfunc.bs,
                                detectionData = new.detectionData,
                                siteData = new.siteData, 
                                area = area, 
                                bySite = FALSE)
          
          B$abundance[i] <- abund.bs$abundance
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

      # Calculate CI from bootstrap replicates using bias-corrected bootstrap method in Manly text
      # Abundance first
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
      ans$n.hat.ci <- bcCI(B$abundance, ans$n.hat, ci)
      ans$density.ci <- bcCI(B$density, ans$density, ci)
      ans$B <- B
      ans$nItersConverged <- convergedCount
      if (!(dfunc$like.form=="smu") && (convergedCount < R) && showProgress){
        cat(paste( R - convergedCount, "of", R, "iterations did not converge.\n"))
      }
      
    } else {
      # Don't compute CI if ci is null
      ans$B <- NULL
      ans$density.ci <- c(NULL, NULL)
      ans$n.hat.ci <- c(NULL, NULL)
      ans$nItersConverged <- NULL
    }  # end else
    
    
    # Output
    ans$alpha <- ci
    class(ans) <- c("abund", class(dfunc))
    
    
  }  # end else of if (bySite)
  
  
  
  
  return(ans)
  
  
  
}  # end function
