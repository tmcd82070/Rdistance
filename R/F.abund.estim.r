#' @name F.abund.estim
#' @aliases F.abund.estim
#' 
#' @title Estimate abundance from distance-sampling data.
#' @description Estimate abundance (or density) given an estimated detection function and supplemental information 
#'   on observed group sizes, transect lengths, area surveyed, etc.  Also computes confidence intervals of abundance 
#'   (or density) using the bias corrected bootstrap method.
#'   
#' @param dfunc An estimated 'dfunc' object produced by \code{F.dfunc.estim}.
#' @param detection.data A data.frame where each row represents one detection (see example dataset, \code{\link{sparrow.detections}})
#'   and with at least the following three columns with the names \code{siteID}, \code{groupsize}, and \code{dist}:
#'   \itemize{
#'     \item \code{siteID} = the name of the transect.
#'     \item \code{groupsize} = the number of individuals in the detected group.
#'     \item \code{dist} = the perpendicular, off-transect distance.
#'   }
#' @param transect.data A data.frame where each transect surveyed is represented as one row (see example dataset, 
#' \code{\link{sparrow.transects}}) and with at least the following two columns with the names \code{siteID} and \code{length}:
#'   \itemize{
#'     \item \code{siteID} = the name of the transect.  This vector is used during bootstrapping to resample transects.
#'   \item \code{length} = the length of the transect.
#'   }
#' @param area Total study area size.  If \code{area} = 1, density is estimated. 
#'   Density has units (number of animals) per (z), where (z) is the square 
#'   units of the distance measurements.  For example, if distance values 
#'   fitted in \code{dfunc} were in meters, density will be number of individuals 
#'   per square meter.  If distances are miles, density will be number of individuals 
#'   per square mile.  If \code{area} > 1, total abundance on the study 
#'   area is estimated and units are (number of animals).
#' @param ci A scaler indicating the confidence level of confidence intervals.  
#'   Confidence intervals are computed using the bias corrected bootstrap method.  
#'   If \code{ci} = NULL, confidence intervals are not computed. 
#' @param R The number of bootstrap iterations to conduct when \code{ci} is not NULL.
#' @param by.id A logical scalar indicating whether to compute transect-level estimates of abundance.  
#'   The default (\code{by.id=FALSE}) returns only one overall abundance estimate).
#' @param plot.bs A logical scalar indicating whether to plot individual bootstrap iterations.
#' @details The abundance estimate is 
#'   \deqn{N = \frac{n.indiv*area}{2*ESW*tot.trans.len}}{N = n.indiv*area / (2*ESW*tot.trans.len)}
#'   where n.indiv is either \code{avg.group.size * n} or \code{sum(group.sizes)}, and \code{ESW} 
#'   is the effective strip width computed from the estimated distance function (i.e., \code{ESW(dfunc)}). 
#'   
#'   The confidence interval for abundance assumes that the fundamental units of replication 
#'   (transects) are independent. The bias corrected bootstrap method used here resamples the units of replication (transects) and recalculates the model's parameter estimates.  If a double-observer data frame is included in \code{dfunc}, rows of the double-observer data frame are re-sampled each bootstrap iteration.  
#'   No model selection is performed. By default, \code{R} = 500 
#'   iterations are performed, afterwhich the bias corrected confidence intervals are computed 
#'   using the method given in Manly (1997, section 3.4).
#' @return An 'abundance estimate' object, a list of class c("abund", "dfunc"), containing 
#'   all the components of a "dfunc" object (see \code{F.dfunc.estim}), plus, 
#'   \item{n.hat}{Estimated abundance in the study area (if \code{area} > 1) 
#'     or estimated density in the study area (if \code{area} = 1).}
#'   \item{ci}{The bias corrected bootstrap confidence interval for \code{n.hat}.  The names of this component 
#'     give the quantiles of the bootstrap distribution used to compute the bias corrected interval.}
#'   \item{B}{A vector or length \code{R} containing all bootstrap estimated population sizes. If a particular interation
#'     did not converge, the corresponding entry in \code{B} will be \code{NA}. The bootstrap distribution of 
#'     \code{n.hat} can be plotted with \code{hist(x$B)}, where \code{x} is an 'abundance estimate' object. The 
#'     confidence interval in \code{ci} can be reproduced with \code{quantile(x$B[!is.na(x$B)], p=names(x$ci) )}.   }
#'   \item{alpha}{The (scalar) confidence level of the confidence interval for \code{n.hat}.}  
#'   \item{n}{The number of detections (not individuals, unless all group sizes = 1) used in the estimate of abundance.}
#'   \item{area}{The study area size used in the estimate of abundance.}
#'   \item{tran.len}{The total length of transects used in the estimate of abundance.}
#'   \item{esw}{Effective strip width used in the estimate of abundance.  This can be computed with \code{ESW(dfunc)}.}
#'   \item{avg.group.size}{The average group size used in the estimate.}
#'   \item{nhat.df}{A data.frame of transect-level abundance (or density) estimates (if \code{by.id = TRUE}).}
#' @author Trent McDonald, WEST Inc.,  \email{tmcdonald@west-inc.com}
#'         Aidan McDonald, WEST Inc.,  \email{aidan@mcdcentral.org}
#'         Jason Carlisle, University of Wyoming, \email{jason.d.carlisle@gmail.com}
#' @references Manly, B. F. J. (1997) \emph{Randomization, bootstrap, and monte carlo methods in biology}, London: Chapman and Hall.
#' @seealso \code{\link{F.dfunc.estim}}
#' @examples # Load the example datasets of sparrow detections and transects from package
#'   data(sparrow.detections)
#'   data(sparrow.transects)
#'   
#'   # Fit detection function to perpendicular, off-transect distances
#'   dfunc <- F.dfunc.estim(sparrow.detections, w.hi=150)
#'   
#'   # Estimate abundance given a detection function
#'   # Note, area=10000 converts to density per hectare (for distances measured in meters)
#'   # Note, a person should do more than R=20 iterations 
#'   fit <- F.abund.estim(dfunc, detection.data=sparrow.detections, transect.data=sparrow.transects,
#'                        area=10000, R=20, ci=0.95, plot.bs=TRUE, by.id=FALSE)
#' @keywords model
#' @export

F.abund.estim <- function(dfunc, detection.data, transect.data,
                          area=1, ci=0.95, R=500, by.id=FALSE,
                          plot.bs=FALSE){
  
  # Stop and print error if key columns of detection.data or transect.data are missing or contain NAs
  if(!("dist" %in% names(detection.data))) stop("There is no column named 'dist' in your detection.data.")
  if(!("siteID" %in% names(detection.data))) stop("There is no column named 'siteID' in your detection.data.")
  if(!("groupsize" %in% names(detection.data))) stop("There is no column named 'groupsize' in your detection.data.")
  
  if(!("siteID" %in% names(transect.data))) stop("There is no column named 'siteID' in your transect.data.")
  # if(!("length" %in% names(transect.data))) stop("There is no column named 'length' in your transect.data.") # OUTDATED ERROR CHECK: NOT COMPATIBLE WITH POINT TRANSECTS
  
  if(any(is.na(detection.data$dist))) stop("Please remove rows for which detection.data$dist is NA.")
  if(any(is.na(detection.data$siteID))) stop("Please remove rows for which detection.data$siteID is NA.")
  if(any(is.na(detection.data$groupsize))) stop("Please remove rows for which detection.data$groupsize is NA.")
  
  if(any(is.na(transect.data$siteID))) stop("Please remove NA's from transect.data$siteID.")
  # if(any(is.na(transect.data$length))) stop("Please remove NA's from transect.data$length.") # OUTDATED ERROR CHECK: NOT COMPATIBLE WITH POINT TRANSECTS
  
  
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
  
  # Apply truncation specified in dfunc object (including dist equal to w.lo and w.hi)
  (detection.data <- detection.data[detection.data$dist >= dfunc$w.lo & detection.data$dist <= dfunc$w.hi, ])
  
  # sample size (number of detections, NOT individuals)
  (n <- nrow(detection.data))
  
  # group sizes
  (avg.group.size <- mean(detection.data$groupsize))
  
  # total transect length and ESW
  (tot.trans.len <- sum(transect.data$length))
  
  if(dfunc$point.transects){
    esw <- effective.radius(dfunc)
  }
  else{
    esw <- ESW(dfunc)  # get effective strip width
  }
  
  if(is.null(dfunc$covars)){temp <- matrix(nrow = 0, ncol = 0)}else{temp <- dfunc$covars}
  if(ncol(temp) > 1 | dfunc$point.transects){ # If line transects + covariates or point transects, estimate abundance the general way
    f.like <- match.fun(paste( dfunc$like.form, ".like", sep=""))
    s <- 0
    for(i in 1:nrow(detection.data)){
      if(is.null(dfunc$covars)){temp <- NULL}else{temp <- t(as.matrix(dfunc$covars[i,]))}
      new.term <- detection.data$groupsize[i]/integration.constant(dist = dfunc$dist[i],
                                                                   density = paste(dfunc$like.form, ".like", sep=""),
                                                                   w.lo = dfunc$w.lo,
                                                                   w.hi = dfunc$w.hi,
                                                                   covars = temp,
                                                                   a = dfunc$parameters,
                                                                   expansions = dfunc$expansions,
                                                                   point.transects = dfunc$point.transects,
                                                                   series = dfunc$series)
      if(!is.na(new.term)){
        s <- s + new.term
      }
    }
    if(dfunc$point.transects){
      a <- pi*esw*n
    }
    else{
      a <- 2 * tot.trans.len
    }
    
    n.hat <- s * area/a
  }
  else{ # Shorter abundance estimation for line transects without covariates
    n.hat <- avg.group.size * n * area/(2 * esw * tot.trans.len)
  }
  
  # store output
  ans <- dfunc
  ans$n.hat <- n.hat
  ans$n <- n
  ans$area <- area
  ans$esw <- esw
  ans$tran.len <- tot.trans.len
  ans$avg.group.size <- avg.group.size
  
  
  # %%%%%%%%%%%%%%%%% NEEDS TO BE UPDATED FOR COVARIATES %%%%%%%%%%%%%%%%%
  if (!is.null(ci)) {
    # Compute bootstrap CI by resampling transects
    
    g.x.scl.orig <- dfunc$call.g.x.scl  # g(0) or g(x) estimate
    
    n.hat.bs <- rep(NA, R)  # preallocate space for bootstrap replicates of nhat
    
    # Turn on progress bar (if utils is installed)
    if ("utils" %in% installed.packages()[, "Package"]) {
      pb <- txtProgressBar(1, R)
      show.progress = TRUE
    } else show.progress = FALSE
    
    
    # Bootstrap
    cat("Computing bootstrap confidence interval on N...\n")
    for(i in 1:R){
      # sample rows, with replacement, from transect data
      new.transect.data <- transect.data[sample(nrow(transect.data), nrow(transect.data), replace=TRUE), ]
      
      new.trans <- as.character(new.transect.data$siteID)  # which transects were sampled?
      trans.freq <- data.frame(table(new.trans))  # how many times was each represented in the new sample?
      
      # subset distance data from these transects
      if( class(new.transect.data$siteID) == "factor" ){
        new.trans <- unique(droplevels(new.transect.data$siteID))
      } else {
        new.trans <- unique(new.transect.data$siteID)
      }
      new.detection.data <- detection.data[detection.data$siteID %in% new.trans, ]  # this is incomplete, since some transects were represented > once
      
      # replicate according to freqency in new sample
      # merge to add Freq column to indicate how many times to repeat each row
      red <- merge(new.detection.data, trans.freq, by.x="siteID", by.y="new.trans")
      # expand this reduced set my replicating rows
      new.detection.data <- red[rep(seq.int(1, nrow(red)), red$Freq), -ncol(red)]
      
      # Extract distances
      new.x <- new.detection.data$dist
      
      #update g(0) or g(x) estimate.
      if (is.data.frame(g.x.scl.orig)) {
        g.x.scl.bs <- g.x.scl.orig[sample(1:nrow(g.x.scl.orig), 
                                          replace = TRUE), ]
      } else g.x.scl.bs <- g.x.scl.orig
      
      
      
      # estimate distance function
      dfunc.bs <- F.dfunc.estim2(new.x ~ 1, likelihood = dfunc$like.form, 
                                 w.lo = dfunc$w.lo, w.hi = dfunc$w.hi, expansions = dfunc$expansions, 
                                 series = dfunc$series, x.scl = dfunc$call.x.scl, 
                                 g.x.scl = g.x.scl.bs, observer = dfunc$call.observer, point.transects = dfunc$point.transects, 
                                 warn = FALSE)
      
      # Store ESW if it converged
      if (dfunc.bs$convergence == 0) {
        esw.bs <- ESW(dfunc.bs)
        if (esw.bs <= dfunc$w.hi) {
          
          # Calculate observed metrics
          # sample size
          n.bs <- nrow(new.detection.data)
          
          # group sizes
          avg.group.size.bs <- mean(new.detection.data$groupsize)
          
          # Store observed metrics
          #esw <- ESW(dfunc.bs)  #get effective strip width
          tot.trans.len.bs <- sum(new.transect.data$length)
          n.hat.bs[i] <- avg.group.size.bs * n.bs * area/(2 * esw.bs * tot.trans.len.bs)  # area stays same as original?   
          
        }  # end if esw.bs <= w.hi
        if (plot.bs) 
          f.plot.bs(dfunc.bs, x.scl.plot, y.scl.plot, col = "blue", lwd = 0.5)
        
        if (show.progress) setTxtProgressBar(pb, i)
      }  # end if dfunc.bs converged
    }  # end bootstrap
    
    
    # close progress bar  
    if (show.progress) close(pb)
    
    # plot red line of original fit again (over bs lines)
    if (plot.bs) f.plot.bs(dfunc, x.scl.plot, y.scl.plot, col = "red", lwd = 3)
    
    
    # Calculate CI from bootstrap replicates using bias-corrected bootstrap method in Manly text
    p <- mean(n.hat.bs > n.hat, na.rm = TRUE)
    z.0 <- qnorm(1 - p)
    z.alpha <- qnorm(1 - ((1 - ci)/2))
    p.L <- pnorm(2 * z.0 - z.alpha)
    p.H <- pnorm(2 * z.0 + z.alpha)
    ans$ci <- quantile(n.hat.bs[!is.na(n.hat.bs)], p = c(p.L, p.H))
    ans$B <- n.hat.bs
    if (any(is.na(n.hat.bs))) cat(paste(sum(is.na(n.hat.bs)), "of", R, "iterations did not converge.\n"))
    
  }  else {
    # Don't compute CI if ci is null
    ans$B <- NA
    ans$ci <- c(NA, NA)
  }  # end else
  
  
  
  # Compute transect-level densities
  if (by.id) {
    
    # Starting df
    nhat.df <- transect.data[, c("siteID", "length")]
    
    # Summarize raw count (truncated observations excluded previously) by transect
    rawcount <- data.frame(rawcount = tapply(detection.data$groupsize, detection.data$siteID, sum))
    rawcount <- cbind(siteID = rownames(rawcount), rawcount)
    
    # Merge and replace NA with 0 for 0-count transects
    nhat.df <- merge(nhat.df, rawcount, by="siteID", all.x=TRUE)
    nhat.df$rawcount[is.na(nhat.df$rawcount)] <- 0
    
    # Calculate transect-level abundance (density)
    nhat.df$nhat <- (nhat.df$rawcount * area) / (2 * esw * nhat.df$length)   
    
    # Check that transect-level abundances match total abundance
    #mean(nhat.df$nhat)
    #ans$n.hat
    
    # Remove the length column
    nhat.df <- nhat.df[, -2]
    
    # Save in output list
    ans$nhat.df <- nhat.df
  }  # end if by.id
  
  
  
  # Output
  ans$alpha <- ci
  class(ans) <- c("abund", class(dfunc))
  ans
  
  
}  # end function
