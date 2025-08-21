#' @title Abundance point estimates
#' 
#' @description Estimate abundance from an Rdistance fitted model. 
#' This function is called internally by \code{abundEstim}.  Most users will call 
#' \code{abundEstim} to estimate abundance unless they are running simulations or 
#' bootstrapping.
#' 
#' @inheritParams predict.dfunc 
#' @inheritParams abundEstim
#'  
#' @inherit abundEstim details
#'   
#' @return A list containing the following components:
#' 
#'    \item{density}{Estimated density in the surveyed area.}
#'    
#'    \item{abundance}{Estimated abundance on the study area. Equals density if 
#'    area is not specified.}
#'    
#'    \item{n.groups}{The number of detected groups (not individuals, unless all group sizes = 1).}
#'    
#'    \item{n.seen}{The number of individuals (sum of group sizes).}
#'    
#'    \item{area}{Total area of inference. Study area size}
#'    
#'    \item{surveyedUnits}{Number of surveyed sites.  This is total transect length
#'    for line-transects or number of points for point-transects. This total transect
#'    length does not include transects with missing lengths.}
#'    
#'    \item{propUnitSurveyed}{Proportion of the standard survey unit
#'    that was observed}
#'    
#'    \item{avg.group.size}{Average group size on non-NA transects}
#'    
#'    \item{w}{Strip width. }
#'    
#'    \item{pDetection}{Probability of detection.}
#'    
#' For line-transects that do not involve covariates, object$density  
#' is object$n.seen / (2 * propUnitSurveyed * object$w * object$pDetection * object$surveyedUnits)
#'    
#'         
#'    
#' @seealso \code{\link{dfuncEstim}}, \code{\link{abundEstim}}
#' 
#' @export 

estimateN <- function(object
                      , area = NULL
                      , propUnitSurveyed = 1.0
                      ){

  w <- object$w.hi - object$w.lo
  
  # ---- Find observations on NA length transects inside the strip ----
  # We need these regardless whether dfunc converges or not
  groupSz <- Rdistance::groupSizes(object) # length = num distance obs (could include NA)
  eff <- Rdistance::effort(object) # length = num non-missing plus missing transects
  totSurveyedUnits <- sum(eff, na.rm = TRUE) # na.rm CRITICAL here: remove transects with NA length
  if( !Rdistance::is.points(object) ){
    if(units(totSurveyedUnits) != object$outputUnits){
      # w has units we want; but, effort came from user and has not been converted yet
      totSurveyedUnits <- units::set_units(totSurveyedUnits, object$outputUnits, mode="standard")
    }
  }  # Point effort vector has no units b/c it's number of points
  
  # ---- Estimate numerator of abundance ----
  if( object$convergence == 0 ){
    # REMEMBER: component $mf is the model frame and has been truncated to (w.lo, w.hi) and
    #           potentially has distance observations from transects without lengths
    #           component $data has NOT been truncated to the strip
  
    # esw is always a vector of length n. 
    esw <- effectiveDistance(object)
    
    if (Rdistance::is.points(object)) {
      phat <- (esw / w)^2  # for points
    } else {
      phat <- esw / w  # for lines
    }
  
    # phat should be unit-less; check just to be sure, if so drop "1" units
    # Odd: sometimes phat has units "1", sometimes "m/m". Either way, remove,
    # but don't use units::drop_units which only works if phat has units to drop.
    # Assigning NULL units always works
    phat <- units::set_units(phat, NULL)
    nhat <- groupSz / phat # inflated counts one per detection
    
    # ---- Compute density ----
    if(Rdistance::is.points(object)){
      dens <- sum(nhat, na.rm = TRUE) / (propUnitSurveyed * pi * w^2 * totSurveyedUnits) # na.rm CRITICAL here; missing groupsizes on missing transects
    } else {
      dens <- sum(nhat, na.rm = TRUE) / (propUnitSurveyed * 2 * w * totSurveyedUnits)
    }
    
    # ---- Compute abundance ----
    oneSqUnit <- units::set_units(1, object$outputUnits, mode = "standard")^2 
    if( is.null(area) ){
      area <- oneSqUnit
    } else if( units(area) != units(oneSqUnit) ){
      area <- units::set_units(area, units(oneSqUnit), mode="standard")
    }
    
    nhat.df <- dens * area
    if( units(nhat.df) != units(units::set_units(1, "1")) ){
      warning(paste("Units on N are not 1 (unitless). Some units did not convert correctly."
                    , "Manually convert all measurements"
                    , "to the same units outside Rdistance, and re-run."))
    } else {
      nhat.df <- units::set_units(nhat.df, NULL)
    }
    
  } else {
    # if we are here, object did not converge
    dens <- NA
    nhat.df <- NA
    phat <- NA
  }

  Coefs <- data.frame(matrix(stats::coef(object), nrow = 1))
  names(Coefs) <- names(stats::coef(object))
  
  if(Rdistance::is.points(object)){
    avgEDD <- mean( sqrt(phat) * w, na.rm = TRUE)
  } else {
    avgEDD <- mean( phat * w, na.rm = TRUE)
  }
  
  # ---- Make output data frame ----
  nhat.df <- tibble::tibble(
    Coefs
    , density = dens
    , abundance = nhat.df
    , nGroups = sum(!is.na(groupSz))
    , nSeen = sum(groupSz, na.rm = TRUE)
    , avgGroupSize = mean(groupSz, na.rm = TRUE)
    , area = area
    , surveyedUnits = totSurveyedUnits
    , propUnitSurveyed = propUnitSurveyed
    , w = w
    , avgEffDistance = avgEDD
  )
  
  # nhat.df <- list(density = dens
  #                 , abundance = nhat.df
  #                 , n.groups = sum(!is.na(groupSz))
  #                 , n.seen = sum(groupSz, na.rm = TRUE)
  #                 , area = area
  #                 , surveyedUnits = totSurveyedUnits
  #                 , propUnitSurveyed = propUnitSurveyed
  #                 , avg.group.size = mean(groupSz, na.rm = TRUE)
  #                 # , range.group.size = range(groupSz)
  #                 , w = w
  #                 , pDetection = phat
  #                 )
  
  # Code from here to **** came from bottom of 'oneBsIter' ----
  # Coefs <- data.frame(matrix(coef(object), nrow = 1))
  # names(Coefs) <- names(coef(object))
  # 
  # if(Rdistance::is.points(object)){
  #   avgEDD <- mean( sqrt(phat) * w, na.rm = TRUE)
  # } else {
  #   avgEDD <- mean( pDetection * w, na.rm = TRUE)
  # }
  # 
  # out <- tibble::tibble(
  #   Coefs
  #   , density = nEst$density
  #   , abundance = nEst$abundance
  #   , nGroups = nEst$n.groups
  #   , nSeen = nEst$n.seen
  #   , area = nEst$area
  #   , surveyedUnits = nEst$surveyedUnits
  #   , avgGroupSize = nEst$avg.group.size
  #   , avgEffDistance = avgEDD
  # )
  
  # ****

  # some interesting tidbits:
  #  sampled area = tot.trans.len * 2 * (dfunc$w.hi - dfunc$w.lo)
  #  sum(1/phat) = what Distance calls "N in covered region".  This is
  #    estimated number of groups in the sampled area.
  #  sum(1/phat)*mean(dfunc$detections$groupsize) = an estimate of individuals
  #    in the sampled area. This is probably how Distance would do it.
  #  sum(dfunc$detections$groupsize/phat) = the HT estimate of individuals 
  #    in the sampled area.  How RDistance does it. This and the Distance 
  #    estimate are very close.  Only difference is ratio of sums or sum of 
  #    ratios.
  #  sum(detectionData$groupsize) / (sum(1/phat)*mean(detectionData$groupsize)) = 
  #    n.indivs / (nhat.groups*mean.grp.size) = n.groups / nhat.groups = 
  #    what Distance calls "Average p".  This is different than mean(phat) 
  #    the way Rdistance calculates it.
  
  

  return(nhat.df)
}  


