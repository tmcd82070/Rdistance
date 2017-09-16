#' @title Abundance point estimates
#' 
#' @description Estimate abundance given a distance function, 
#' detection data, site data, and area.  This is called internally 
#' by abundEstim during bootstrapping. 
#' 
#' @param dfunc An estimate distance function (see \code{dfuncEstim}).
#' 
#' @param detectionData A data frame containing information on 
#' detections, such as distance and which transect or point they occured
#' on (see \code{dfuncEstim}).
#' 
#' @param siteData A data frame containing information on the 
#' transects or points surveyed  (see \code{dfuncEstim}).
#' 
#' @param area Total area of inference. Study area size.
#' 
#' @return A list containing the following components:
#'    \item{dfunc}{The input distance function}
#'    \item{n.hat}{The estimate of abundance}
#'    \item{n}{Number of groups detected}
#'    \item{area}{Total area of inference. Study area size}
#'    \item{esw}{Effective strip width for line-transects, effective
#'    radius for point-transects.  Both derived from \code{dfunc}}
#'    \item{n.sites}{Total number of transects for line-transects, 
#'    total number of points for point-transects.}
#'    \item{tran.len}{Total transect length. NULL for point-transects.}
#'    \item{avg.group.size}{Average group size}
#'    
#' @author Trent McDonald, WEST Inc.,  \email{tmcdonald@west-inc.com}\cr
#'         Jason Carlisle, University of Wyoming and WEST Inc, \email{jcarlisle@west-inc.com}\cr
#'         Aidan McDonald, WEST Inc., \email{aidan@mcdcentral.org}
#'     
#' @seealso \code{\link{dfuncEstim}}, \code{\link{abundEstim}}
#' 
#' @examples # Load the example datasets of sparrow detections and transects from package
#'   data(sparrow.detections)
#'   data(sparrow.transects)
#'   
#'   # Fit detection function to perpendicular, off-transect distances
#'   dfunc <- dfuncEstim(sparrow.detections, w.hi=150)
#'   
#'   # Estimate abundance given a detection function
#'   n.hat <- estimate.nhat(dfunc, sparrow.detections, sparrow.sites)
#'   
#' @keywords model
#'
#' @export 

estimateN <- function(dfunc, detectionData, siteData, area){
  # Truncate detections and calculate some n, avg.group.isze, 
  # tot.trans.len, and esw
  
  # Apply truncation specified in dfunc object (including dist 
  # equal to w.lo and w.hi)
  (detectionData <- detectionData[detectionData$dist >= dfunc$w.lo 
                                    & detectionData$dist <= dfunc$w.hi, ])
  
  # sample size (number of detections, NOT individuals)
  (n <- nrow(detectionData))
  
  # group sizes
  (avg.group.size <- mean(detectionData$groupsize))
  
  # total transect length and ESW
  tot.sites <- nrow(siteData)  # number of points or number of transects
  if (dfunc$pointSurvey) {
    tot.trans.len <- NULL  # no transect length
    esw <- effectiveDistance(dfunc)  # point count equivalent of effective strip width
  } else {
    tot.trans.len <- sum(siteData$length)  # total transect length
    esw <- effectiveDistance(dfunc)  # get effective strip width
  }
  
  
  # Estimate abundance
  if (is.null(dfunc$covars)) {
    temp <- matrix(nrow = 0, ncol = 0)  # (jdc) isn't this just getting overwritten with NULL ~10 lines below?
  } else {
    temp <- dfunc$covars
  }
  
  # If covariates (for line or point transects) estimate abundance the general way
  # If no covariates, use the faster, standard equations (see after else)
  if (ncol(temp) > 1) { 
    f.like <- match.fun(paste( dfunc$like.form, ".like", sep=""))
    s <- 0
    for (i in 1:nrow(detectionData)) {
      if (is.null(dfunc$covars)) {
        temp <- NULL
      } else {
        temp <- t(as.matrix(dfunc$covars[i,]))
      }
      new.term <- detection.data$groupsize[i] / ESW(dfunc)
      # new.term <- detection.data$groupsize[i]/integration.constant(dist = dfunc$dist[i],  # (jdc) the integration constant doesn't change for different dist values (tested w/line data)
      #                                                              density = paste(dfunc$like.form, ".like", sep=""),
      #                                                              w.lo = dfunc$w.lo,
      #                                                              w.hi = dfunc$w.hi,
      #                                                              covars = temp,
      #                                                              a = dfunc$parameters,
      #                                                              expansions = dfunc$expansions,
      #                                                              point.transects = dfunc$point.transects,
      #                                                              series = dfunc$series)
      if (!is.na(new.term)) {
        s <- s + new.term
      }
    }
    if (dfunc$pointSurvey) {
      a <- pi * esw^2 * tot.sites  # area for point transects  
    } else {
      a <- 2 * esw * tot.trans.len  # area for line transects
    }
    n.hat <-  s * area/a
    
    
  } else {
    
    # Standard (and faster) methods when there are no covariates
    if (dfunc$pointSurvey) {
      # Standard method for points with no covariates
      n.hat <- (avg.group.size * n * area) / (pi * (esw^2) * tot.sites)
    } else {
      # Standard method for lines with no covariates
      n.hat <- (avg.group.size * n * area) / (2 * esw * tot.trans.len)
    }
  }
  
  
  # Output to return as list
  abund <- list(dfunc = dfunc,
                n.hat = n.hat,
                n = n,
                area = area,
                esw = esw,
                n.sites = tot.sites,
                tran.len = tot.trans.len,
                avg.group.size = avg.group.size)
  
  return(abund)
}  # end estimate.nhat function


