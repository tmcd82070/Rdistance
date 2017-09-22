#' @title Estimate abundance from distance-sampling data.
#'   
#' @description Estimate abundance (or density) given an estimated detection
#'   function and supplemental information on observed group sizes, transect
#'   lengths, area surveyed, etc.  Also computes confidence intervals of
#'   abundance (or density) using the bias corrected bootstrap method.
#'   
#' @param dfunc An estimated 'dfunc' object produced by \code{dfuncEstim}.
#' 
#' @param detectionData A data.frame with each row representing one detection
#'   (see example dataset, \code{\link{sparrowDetectionData}}) and with at least
#'   the following three columns: 
#'   \itemize{ 
#'     \item \code{siteID} = ID of the transect or point. 
#'     \item \code{groupsize} = the number of individuals in the detected group. 
#'     \item \code{dist} = the perpendicular, off-transect distance or radial
#'       off-point distance to the detected group. 
#'   }
#'   
#' @param siteData A data.frame with each row representing one site 
#'   (transect or point) (see example dataset, 
#'   \code{\link{sparrowSiteData}}). If the data in \code{detectionData}
#'   is from line transects, \code{siteData} must have at 
#'   least the following two columns:
#'   \itemize{ 
#'     \item \code{siteID} = ID of the transect or point. This vector 
#'     is used during bootstrapping to resample sites. 
#'     \item \code{length} = the length of the transect. 
#'   }
#'   If the data in \code{detectionData}
#'   is from point transects, \code{siteData} must have a
#'   \code{siteID} column only.  For both data types, \code{siteID} 
#'     is used during bootstrapping to resample sites. 
#'    
#'     
#' @param area Total study area size.  If \code{area} = 1, density is estimated.
#'   Density has units (number of animals) per (squared 
#'   units of the distance measurements).  For example, if distance values fitted
#'   in \code{dfunc} are meters, density is number of individuals per
#'   square meter.  If distances are miles, density is number of
#'   individuals per square mile.  If \code{area} > 1, total abundance on the
#'   study area is estimated and units are (number of animals).  This can also
#'   be used to convert units for density. For example, if distance values fitted
#'   in \code{dfunc} are meters, and area is set to 10,000, density is number
#'   of individuals per hectare (ha; 1 ha = 10,000 square meters).
#'   square meter.
#'   
#' @param ci A scaler indicating the confidence level of confidence intervals. 
#'   Confidence intervals are computed using the bias corrected bootstrap
#'   method. If \code{ci = NULL}, confidence intervals are not computed.
#'   
#' @param R The number of bootstrap iterations to conduct when \code{ci} is not
#'   NULL.
#'   
#' @param plot.bs A logical scalar indicating whether to plot individual
#'   bootstrap iterations.
#'   
#' @param bySite A logical scalar indicating whether to compute site-level
#'   estimates of abundance. The default (\code{bySite=FALSE}) returns only one
#'   overall abundance estimate. This routine does not calculate confidence
#'   intervals for these site-level abundance estimates, so \code{ci} is set to
#'   \code{NULL} if \code{bySite = TRUE}. See \code{\link{estimateN}}.
#'   
#' @details The abundance estimate is \deqn{N =
#'   \frac{n.indiv(area)}{2(ESW)(tot.trans.len)}}{N = n.indiv*area /
#'   (2*ESW*tot.trans.len)} where n.indiv is either \code{avg.group.size * n} or
#'   \code{sum(group.sizes)}, and \code{ESW} is the effective strip width
#'   computed from the estimated distance function (i.e., \code{ESW(dfunc)}).
#'   
#'   The confidence interval for abundance assumes that the fundamental units of
#'   replication (transects) are independent. The bias corrected bootstrap
#'   method used here resamples the units of replication (transects) and
#'   recalculates the model's parameter estimates.  If a double-observer data
#'   frame is included in \code{dfunc}, rows of the double-observer data frame
#'   are re-sampled each bootstrap iteration. No model selection is performed.
#'   By default, \code{R} = 500 iterations are performed, afterwhich the bias
#'   corrected confidence intervals are computed using the method given in Manly
#'   (1997, section 3.4).
#'   
#' @return If \code{bySite} is FALSE, an 'abundance estimate' object, a list of
#'   class c("abund", "dfunc"), containing all the components of a "dfunc"
#'   object (see \code{dfuncEstim}), plus the following: 
#'   
#'  \item{abundance}{Estimated abundance in the study area (if \code{area} >
#'  1) or estimated density in the study area (if \code{area} = 1).}
#'   \item{n}{The number of detections
#'  (not individuals, unless all group sizes = 1) used in the estimate of
#'  abundance.}
#'   \item{area}{Total area of inference. Study area size}
#'   \item{esw}{Effective strip width for line-transects, effective
#'   radius for point-transects.  Both derived from \code{dfunc}}.
#'   \item{n.sites}{Total number of transects for line-transects, 
#'   total number of points for point-transects.}
#'   \item{tran.len}{Total transect length. NULL for point-transects.}
#'   \item{avg.group.size}{Average group size}
#'   \item{ci}{The bias corrected bootstrap confidence interval for
#'   \code{n.hat}.  The names of this component give the quantiles of the
#'   bootstrap distribution used to compute the bias corrected interval.} 
#'   \item{B}{A vector or length \code{R} containing all bootstrap estimated
#'   population sizes. If a particular interation did not converge, the
#'   corresponding entry in \code{B} will be \code{NA}. The bootstrap
#'   distribution of \code{n.hat} can be plotted with \code{hist(x$B)}, where
#'   \code{x} is an 'abundance estimate' object. The confidence interval in
#'   \code{ci} can be reproduced with \code{quantile(x$B[!is.na(x$B)],
#'   p=names(x$ci) )}.}
#'   \item{alpha}{The (scalar) confidence level of the
#'   confidence interval for \code{n.hat}.} 
#'   
#'   If \code{bySite} is TRUE, a data frame containing site-level 
#'   estimated abundance.  The data frame is an exact copy of \code{siteData}
#'   with the following columns tacked onto the end:
#'    
#'   \item{effDist}{The effective sampling distance at the site.  For line-
#'   transects, this is ESW at the site.  For points, this is EDR. } 
#'   \item{pDetection}{Average probability of deteciton at the site. 
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
#' @references Manly, B.F.J. (1997) \emph{Randomization, bootstrap, and monte
#'   carlo methods in biology}, London: Chapman and Hall.
#'   
#'   Buckland, S.T., D.R. Anderson, K.P. Burnham, J.L. Laake, D.L. Borchers,
#'    and L. Thomas. (2001) \emph{Introduction to distance sampling: estimating
#'    abundance of biological populations}. Oxford University Press, Oxford, UK.
#'   
#' @seealso \code{\link{dfuncEstim}}
#' 
#' @examples # Load the example datasets of sparrow detections and transects from package
#'   data(sparrowDetectionData)
#'   data(sparrowSiteData)
#'   
#'   # Fit detection function to perpendicular, off-transect distances
#'   dfunc <- dfuncEstim(sparrowDetectionData, w.hi=150)
#'   
#'   # Estimate abundance given a detection function
#'   # Note, area=10000 converts to density per hectare (for distances measured in meters)
#'   # Note, a person should do more than R=20 iterations 
#'   fit <- abundEstim(dfunc, detectionData=sparrowDetectionData, 
#'          siteData=sparrowSiteData, area=10000, R=20, ci=0.95, 
#'          plot.bs=TRUE, by.id=FALSE)
#'          
#' @keywords model
#' @export

abundEstim <- function(dfunc, detectionData, siteData,
                          area=1, ci=0.95, R=500, 
                          plot.bs=FALSE, bySite=FALSE){
  
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
  
  
  
  # (jdc) (we should split f.plot.bs out as a separate .R file, yes?)
  # Plotting 
  f.plot.bs <- function(x,  ...) {
    x.seq <- seq(x$w.lo, x$w.hi, length = 200)
    g.at.x0 <- x$g.x.scl
    x0 <- x$x.scl
    y <- like(x$parameters, x.seq - x$w.lo, series = x$series, 
              expansions = x$expansions, w.lo = x$w.lo, w.hi = x$w.hi,
              pointSurvey=x$pointSurvey)
    if(!x$pointSurvey){
      f.at.x0 <- like(x$parameters, x0 - x$w.lo, series = x$series, 
                    expansions = x$expansions, w.lo = x$w.lo, w.hi = x$w.hi, 
                    pointSurvey=x$pointSurvey)
      yscl <- g.at.x0/f.at.x0
    } else {
      f.max <- F.maximize.g(x, covars = NULL) 
      yscl <- g.at.x0/f.max
    }
    
    lines(x.seq, y * yscl, ...)
    # lines(x.seq, y , ...)
  }
  
  if (plot.bs) {
    tmp <- plot(dfunc) 
    like <- match.fun(paste(dfunc$like.form, ".like", sep = ""))
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
    ans$n.hat <- abund$abundance
    ans$n <- abund$n.groups
    ans$area <- abund$area
    ans$esw <- abund$pDetection * abund$w
    ans$tran.len <- abund$tran.len
    ans$avg.group.size <- abund$avg.group.size
    
    
    
    
    
    if (!is.null(ci)) {
      # Compute bootstrap CI by resampling transects
      
      g.x.scl.orig <- dfunc$call.g.x.scl  # g(0) or g(x) estimate
      
      n.hat.bs <- rep(NA, R)  # preallocate space for bootstrap replicates of nhat
      
      # Turn on progress bar (if utils is installed)
      if ("utils" %in% installed.packages()[, "Package"]) {
        pb <- txtProgressBar(1, R, style=3)
        show.progress = TRUE
      } else {
        show.progress = FALSE
      } 
      
      # Bootstrap
      cat("Computing bootstrap confidence interval on N...\n")
      for (i in 1:R) {
        # sample rows, with replacement, from transect data
        new.siteData <- siteData[sample(nrow(siteData), nrow(siteData), replace=TRUE), ]
        
        new.trans <- as.character(new.siteData$siteID)  # which transects were sampled?
        trans.freq <- data.frame(table(new.trans))  # how many times was each represented in the new sample?
        
        # subset distance data from these transects
        if ( class(new.siteData$siteID) == "factor" ) {
          new.trans <- unique(droplevels(new.siteData$siteID))
        } else {
          new.trans <- unique(new.siteData$siteID)
        }
        new.detectionData <- detectionData[detectionData$siteID %in% new.trans, ]  # this is incomplete, since some transects were represented > once
        
        # new.mergeData <- merge(new.detectionData, siteData, by="siteID")
        
        # replicate according to freqency in new sample
        # merge to add Freq column to indicate how many times to repeat each row
        red <- merge(new.detectionData, trans.freq, by.x="siteID", by.y="new.trans")
        # expand this reduced set my replicating rows
        new.detectionData <- red[rep(seq.int(1, nrow(red)), red$Freq), -ncol(red)]
        
        # And merge on site-level covariates
        # Not needed if no covars, but cost in time should be negligible
        # Need to merge now to get covars siteData that has unduplicated siteID.
        new.mergeData <- merge(new.detectionData, siteData, by="siteID")
        # unique(droplevels(new.detectionData$siteID))
        # unique(droplevels(new.siteData$siteID))
        # length(new.trans)
        
        # Extract distances
        # new.x <- new.detectionData$dist
        
        #update g(0) or g(x) estimate.
        if (is.data.frame(g.x.scl.orig)) {
          g.x.scl.bs <- g.x.scl.orig[sample(1:nrow(g.x.scl.orig), 
                                            replace = TRUE), ]
        } else {
          g.x.scl.bs <- g.x.scl.orig
        }
        
        
        # Re-fit detection function -- same function, new data
        # fmla <- as.formula(dfunc$call[["formula"]])  # (jdc) when called from autoDistSamp, dfunc$call[["formula"]] is "formula"
        
        dfunc.bs <- dfuncEstim(formula = dfunc$formula,  # (jdc) updated dfuncEstim to store formula separate from call
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
                               warn = FALSE)

        # Store ESW if it converged
        if (dfunc.bs$convergence == 0) {
          
          # Note: there are duplicate siteID's in newSiteData.  This is okay
          # because number of unique siteIDs is never computed in estimateN. 
          # Number of sites in estiamteN is nrow(newSiteData)
          
          # Estimate abundance
          # (jdc) this function isn't setup to generate bootstrap CIs of site-level abundance, so this is always bySite=FALSE
          # (jdc) so bySite=TRUE and !is.null(ci) is perhaps misleading
          abund.bs <- estimateN(dfunc=dfunc.bs,
                                detectionData=new.detectionData,
                                siteData=new.siteData, area=area, 
                                bySite=FALSE)
          
          n.hat.bs[i] <- abund.bs$abundance
          
          
          if (plot.bs ) {
            # (jdc) - this is plotting the prob of detection, doesn't match scaling of dfunc plot for points
            f.plot.bs(dfunc.bs, col = "blue", lwd = 0.5)  
          }
          
          
          if (show.progress) setTxtProgressBar(pb, i)
        }  # end if dfunc.bs converged
      }  # end bootstrap
      
      
      # close progress bar  
      if (show.progress) close(pb)
      
      # plot red line of original fit again (over bs lines)
      if (plot.bs) {
        f.plot.bs(dfunc, col = "red", lwd = 3)
      } 
      
      
      # Calculate CI from bootstrap replicates using bias-corrected bootstrap method in Manly text
      p <- mean(n.hat.bs > ans$n.hat, na.rm = TRUE)
      z.0 <- qnorm(1 - p)
      z.alpha <- qnorm(1 - ((1 - ci)/2))
      p.L <- pnorm(2 * z.0 - z.alpha)
      p.H <- pnorm(2 * z.0 + z.alpha)
      ans$ci <- quantile(n.hat.bs[!is.na(n.hat.bs)], p = c(p.L, p.H))
      ans$B <- n.hat.bs
      if (any(is.na(n.hat.bs))){
        cat(paste(sum(is.na(n.hat.bs)), "of", R, "iterations did not converge.\n"))
      }
      
    } else {
      # Don't compute CI if ci is null
      ans$B <- NA
      ans$ci <- c(NA, NA)
    }  # end else
    
    
    # Output
    ans$alpha <- ci
    class(ans) <- c("abund", class(dfunc))
    
    
  }  # end else of if (bySite)
  
  
  
  
  return(ans)
  
  
  
}  # end function
