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
#'   study area is estimated and units are (number of animals).
#'   
#' @param ci A scaler indicating the confidence level of confidence intervals. 
#'   Confidence intervals are computed using the bias corrected bootstrap
#'   method. If \code{ci} = NULL, confidence intervals are not computed.
#'   
#' @param R The number of bootstrap iterations to conduct when \code{ci} is not
#'   NULL.
#'   
#' @param by.id A logical scalar indicating whether to compute transect-level
#'   estimates of abundance. The default (\code{by.id=FALSE}) returns only one
#'   overall abundance estimate).
#'   
#' @param plot.bs A logical scalar indicating whether to plot individual
#'   bootstrap iterations.
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
#' @return An 'abundance estimate' object, a list of class c("abund", "dfunc"),
#'   containing all the components of a "dfunc" object (see \code{dfuncEstim}),
#'   plus, 
#'   
#'   \item{n.hat}{Estimated abundance in the study area (if \code{area} >
#'   1) or estimated density in the study area (if \code{area} = 1).} 
#'   
#'   \item{ci}{The bias corrected bootstrap confidence interval for
#'   \code{n.hat}.  The names of this component give the quantiles of the
#'   bootstrap distribution used to compute the bias corrected interval.} 
#'   
#'   \item{B}{A vector or length \code{R} containing all bootstrap estimated
#'   population sizes. If a particular interation did not converge, the
#'   corresponding entry in \code{B} will be \code{NA}. The bootstrap
#'   distribution of \code{n.hat} can be plotted with \code{hist(x$B)}, where
#'   \code{x} is an 'abundance estimate' object. The confidence interval in
#'   \code{ci} can be reproduced with \code{quantile(x$B[!is.na(x$B)],
#'   p=names(x$ci) )}.   } 
#'   
#'   \item{alpha}{The (scalar) confidence level of the
#'   confidence interval for \code{n.hat}.} 
#'   
#'   \item{n}{The number of detections
#'   (not individuals, unless all group sizes = 1) used in the estimate of
#'   abundance.} 
#'   
#'   \item{area}{The study area size used in the estimate of
#'   abundance.} 
#'   
#'   \item{tran.len}{The total length of transects used in the
#'   estimate of abundance.} 
#'   
#'   \item{esw}{Effective strip width(s) used in the
#'   estimate of abundance.  This can be computed with \code{ESW(dfunc)}.} 
#'   
#'   \item{avg.group.size}{The average group size used in the estimate.} 
#'   
#'   \item{nhat.df}{A data.frame of transect-level abundance (or density)
#'   estimates (if \code{by.id = TRUE}).}
#'   
#' @author Trent McDonald, WEST Inc.,  \email{tmcdonald@west-inc.com}\cr 
#'   Aidan McDonald, WEST Inc.,  \email{aidan@mcdcentral.org}\cr 
#'   Jason Carlisle, University of Wyoming and WEST Inc., 
#'   \email{jcarlisle@west-inc.com}
#'   
#' @references Manly, B. F. J. (1997) \emph{Randomization, bootstrap, and monte
#'   carlo methods in biology}, London: Chapman and Hall.
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
                          area=1, ci=0.95, R=500, by.id=FALSE,
                          plot.bs=FALSE){
  
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
  
  # (jdc) (we should split f.plot.bs out as a separate .R file, yes?)
  # Plotting 
  f.plot.bs <- function(x, xscl, yscl, ...) {
    x.seq <- seq(x$w.lo, x$w.hi, length = 200)
    g.at.x0 <- x$g.x.scl
    x0 <- x$x.scl
    y <- like(x$parameters, x.seq - x$w.lo, series = x$series, 
              expansions = x$expansions, w.lo = x$w.lo, w.hi = x$w.hi)
    f.at.x0 <- like(x$parameters, x0 - x$w.lo, series = x$series, 
                    expansions = x$expansions, w.lo = x$w.lo, w.hi = x$w.hi)
    yscl <- g.at.x0/f.at.x0
    lines(x.seq, y * yscl, ...)
  }
  if (plot.bs) {
    tmp <- plot(dfunc) 
    x.scl.plot <- tmp$xscl.plot
    y.scl.plot <- tmp$yscl
    like <- match.fun(paste(dfunc$like.form, ".like", sep = ""))
  }
  

  
  # Estimate abundance
  abund <- estimateN(dfunc=dfunc, detectionData=detectionData, 
                        siteData=siteData, area=area)
  


  # store output returned by this function
  # (will be added to in later sections)

  # ans <- dfunc
  # ans$n.hat <- n.hat
  # ans$n <- n
  # ans$area <- area
  # ans$esw <- esw
  # ans$tran.len <- tot.trans.len
  # ans$avg.group.size <- avg.group.size
  
  # dfunc is already stored in abund returned above, 
  # but the print.abund and print.dfunc were not working
  # when I just stored ans <- abund.  This is clunky, but resolves the issue.
  ans <- dfunc
  ans$n.hat <- abund$n.hat
  ans$n <- abund$n
  ans$area <- abund$area
  ans$esw <- abund$esw
  ans$tran.len <- abund$tot.trans.len
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
    for(i in 1:R){
      # sample rows, with replacement, from transect data
      new.siteData <- siteData[sample(nrow(siteData), nrow(siteData), replace=TRUE), ]
      
      new.trans <- as.character(new.siteData$siteID)  # which transects were sampled?
      trans.freq <- data.frame(table(new.trans))  # how many times was each represented in the new sample?
      
      # subset distance data from these transects
      if( class(new.siteData$siteID) == "factor" ){
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
      fmla <- as.formula(dfunc$call[["formula"]])
      
      dfunc.bs <- dfuncEstim(formula = fmla,
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
         abund.bs <- estimateN(dfunc=dfunc.bs,
                                   detectionData=new.detectionData,
                                   siteData=new.siteData, area=area)
         
         n.hat.bs[i] <- abund.bs$n.hat
      

         if (plot.bs & !dfunc$pointSurvey) {
           # (jdc) - this is plotting the prob of detection, doesn't match scaling of dfunc plot for points
           f.plot.bs(dfunc.bs, x.scl.plot, y.scl.plot, col = "blue", lwd = 0.5)  
         }
      
      
         if (show.progress) setTxtProgressBar(pb, i)
      }  # end if dfunc.bs converged
    }  # end bootstrap
    
    
    # close progress bar  
    if (show.progress) close(pb)
    
    # plot red line of original fit again (over bs lines)
    if (plot.bs) {
      f.plot.bs(dfunc, x.scl.plot, y.scl.plot, col = "red", lwd = 3)
    } 
    
    
    # Calculate CI from bootstrap replicates using bias-corrected bootstrap method in Manly text
    p <- mean(n.hat.bs > abund$n.hat, na.rm = TRUE)
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
  

  #%%%%%%%%%%%%%%%%%#
  # (jdc) The by.id option needs to be updated for points and covars
  
  # The cols in the returned nhat.df should include siteID, rawcount, nhat, p, and ESW
  
  
  # Compute transect-level densities
  if (by.id) {
    
    # Starting df
    # nhat.df <- siteData[, c("siteID", "length")]
    nhat.df <- data.frame(siteID = siteData$siteID, nhat=NA)
    
    # Summarize raw count by transect
    # Apply truncation specified in dfunc object (including dist equal to w.lo and w.hi)
    detectionData <- detectionData[detectionData$dist >= dfunc$w.lo & detectionData$dist <= dfunc$w.hi, ]
    rawcount <- data.frame(rawcount = tapply(detectionData$groupsize, detectionData$siteID, sum))
    rawcount <- cbind(siteID = rownames(rawcount), rawcount)

    # Merge and replace NA with 0 for 0-count transects
    nhat.df <- merge(nhat.df, rawcount, by="siteID", all.x=TRUE)
    nhat.df$rawcount[is.na(nhat.df$rawcount)] <- 0

    # # Calculate transect-level abundance (density)
    # nhat.df$nhat <- (nhat.df$rawcount * area) / (2 * esw * nhat.df$length)
    # 
    # # Check that transect-level abundances match total abundance
    # #mean(nhat.df$nhat)
    # #ans$n.hat
    # 
    # # Remove the length column
    # nhat.df <- nhat.df[, -2]
    
    
    # Calculate transect-level abundance (density)
    # Loop through each transect (site)
    # This loop could be replaced with something faster
    # For example, rawcount is already calculated in estimateNhat
    for (i in 1:nrow(nhat.df)) {
      
      # nhat is 0 where the count is 0
      if (nhat.df[i, "rawcount"] == 0) {
        nhat.df[i, "nhat"] <- 0
        next()
      }
      
      site <- nhat.df[i, "siteID"]
      # Subset both input datasets to only that site
      dd <- detectionData[detectionData$siteID == site, ]
      sd <- siteData[siteData$siteID == site, ]
      ad <- area # (jdc), placeholder for now, what would the appropriate area be?
      
      # Estimate abundance
      nhat.df[i, "nhat"] <- estimateNhat(dfunc=dfunc, detectionData=dd, siteData=sd, area=ad)$n.hat
      
    }
    

    # Save in output list
    ans$nhat.df <- nhat.df
  }  # end if by.id
  
  
  
  # Output
  ans$alpha <- ci
  class(ans) <- c("abund", class(dfunc))
  
  return(ans)
  
  
}  # end function
