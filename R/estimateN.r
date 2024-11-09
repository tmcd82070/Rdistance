#' @title estimateN - Abundance point estimates
#' 
#' @description Estimate abundance from an Rdistance fitted model. 
#' This function is called internally by \code{abundEstim}.  Most users will call 
#' \code{abundEstim} to estimate abundance unless they are running simulations or 
#' bootstrapping.
#' 
#' @inheritParams predict.dfunc 
#' 
#' @param surveyedSides The number of sides of the transect that were surveyed. Either 
#' 1 or 2.  Only applies to line transects. 
#' 
#' @inheritParams abundEstim
#' 
#' @inheritParams dfuncEstim  
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
#'    \item{surveyedSides}{Number of sides (1 or 2) of transects surveyed. 
#'    Only relevant for line-transects.}
#'    
#'    \item{avg.group.size}{Average group size on non-NA transects}
#'    
#'    \item{w}{Strip width. }
#'    
#'    \item{pDetection}{Probability of detection.}
#'    
#' For line-transects that do not involve covariates, x$density  
#' is x$n.seen / (x$surveyedSides * x$w * x$pDetection * x$surveyedUnits)
#'    
#'         
#'    
#' @seealso \code{\link{dfuncEstim}}, \code{\link{abundEstim}}
#' 
#' @export 

estimateN <- function(x
                      , area = NULL
                      , surveyedSides = 2
                      ){

  w <- x$w.hi - x$w.lo
  
  # ---- Find observations on NA length transects inside the strip ----
  # We need these regardless whether dfunc converges or not
  groupSz <- Rdistance::groupSizes(x) # length = num distance obs (could include NA)
  eff <- Rdistance::effort(x) # length = num non-missing plus missing transects
  totSurveyedUnits <- sum(eff, na.rm = TRUE) # na.rm CRITICAL here: remove transects with NA length
  if( !Rdistance::is.points(dfunc) ){
    if(units(totSurveyedUnits) != x$outputUnits){
      # w has units we want; but, effort came from user and has not been converted yet
      totSurveyedUnits <- units::set_units(totSurveyedUnits, x$outputUnits, mode="standard")
    }
  }
  
  # ---- Estimate numerator of abundance ----
  if( x$convergence == 0 ){
    # REMEMBER: component $mf is the model frame and has been truncated to (w.lo, w.hi) and
    #           potentially has distance observations from transects without lengths
    #           component $data has NOT been truncated to the strip
  
    # esw is always a vector of length n. 
    # If dfunc is pointSurveys, esw is EDR.  If line surveys, esw is ESW.
    esw <- effectiveDistance(x)
    
    if (Rdistance::is.points(x)) {
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
    if(Rdistance::is.points(x)){
      dens <- sum(nhat, na.rm = TRUE) / (pi * w^2 * totSurveyedUnits) # na.rm CRITICAL here; missing groupsizes on missing transects
    } else {
      dens <- sum(nhat, na.rm = TRUE) / (surveyedSides * w * totSurveyedUnits)
    }
    
    # ---- Compute abundance ----
    oneSqUnit <- units::set_units(1, x$outputUnits, mode = "standard")^2 
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
    # if we are here, x did not converge
    dens <- NA
    nhat.df <- NA
    phat <- NA
  }

  # ---- Make output data frame ----
  nhat.df <- list(density = dens
                  , abundance = nhat.df
                  , n.groups = sum(!is.na(groupSz))
                  , n.seen = sum(groupSz, na.rm = TRUE)
                  , area = area
                  , surveyedUnits = totSurveyedUnits
                  , surveyedSides = surveyedSides
                  , avg.group.size = mean(groupSz, na.rm = TRUE)
                  # , range.group.size = range(groupSz)
                  , w = w
                  , pDetection = phat
                  )

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


