#' @title Abundance point estimates
#' 
#' @description Estimate abundance given a distance function, 
#' total number of sampled units (length or points), and area.  
#' This is called internally 
#' by \code{abundEstim}.  Most users will call 
#' \code{abundEstim} to estimate abundance. 
#' 
#' @param dfunc An estimate distance function (see \code{dfuncEstim}).
#' 
#' @param surveyedUnits A scalar containing either the total length 
#' of surveyed transects (for line transects) or total number of surveyed
#' points (for point transects). This number is either $L$ or $P$ in the 
#' abundance formulas (Details).
#' 
#' @inheritParams abundEstim
#' 
#' @inheritParams dfuncEstim  # for 'control'
#' 
#' @inherit abundEstim details
#'   
#' @return If \code{bySite} is FALSE, a list containing the following components:
#'    \item{dfunc}{The input distance function.}
#'    \item{abundance}{Estimated abundance in the study area (if \code{area} >
#'   1) or estimated density in the study area (if \code{area} = 1).}
#'    \item{n}{The number of detections
#'   (not individuals, unless all group sizes = 1) used in the estimate of
#'   abundance.}
#'    \item{area}{Total area of inference. Study area size}
#'    \item{esw}{Effective strip width for line-transects, effective
#'    radius for point-transects.  Both derived from \code{dfunc}}.
#'    \item{n.sites}{Total number of transects for line-transects, 
#'    total number of points for point-transects.}
#'    \item{tran.len}{Total transect length. NULL for point-transects.}
#'    \item{avg.group.size}{Average group size}
#'    
#'    If \code{bySite} is TRUE, a data frame containing site-level 
#'    estimated abundance.  The data frame is an exact copy of \code{siteData}
#'    with the following columns tacked onto the end:
#'     
#'    \item{effDist}{The effective sampling distance at the site.  For line-
#'    transects, this is ESW at the site.  For points, this is EDR. } 
#'    \item{pDetection}{Average probability of detection at the site. 
#'    If only site-level covariates appear in the distance function, 
#'    pDetection is constant within a site. When detection-level 
#'    covariates are present, pDetection is the average at the site.}
#'    \item{observedCount}{The total number of individuals detected at a site.}
#'    \item{abundance}{Estimated abundance at the site. This is the sum
#'    of inflated group sizes at the site. i.e., each group size 
#'    at the site is divided by its pDetection, and then summed.    }
#'    \item{density}{Estimated density at the site. This is abundance 
#'    at the site divided by the sampled area at the site.  E.g., for 
#'    line transects, this is abundance divided by \eqn{2*w*length}. For 
#'    points, this is abundance divided by \eqn{pi*w^2}.}
#'    \item{effArea}{The effective area sampled at the site. This could be used
#'    as an offset in a subsequent linear model. For 
#'    line transects, this is \eqn{2*ESW*length}. For 
#'    points, this is \eqn{pi*EDR^2}.}
#'
#  MOVE THIS TO NEW BY SITE ROUTINE    
# @details 
# If \code{x} is the data frame returned when \code{bySite} = TRUE, 
# the following is true: 
# \enumerate{
#   \item For line transects, \code{sum(x$abundance)*area/(2*w*sum(x$length))}
#     is the estimate of abundance on the study area or the 
#     abundance estimate when \code{bySite} = FALSE.
#     
#   \item \code{area*sum(x$density)/nrow(x)} is the estimate of abundance 
#   on the study area or the abundance estimate when \code{bySite} = FALSE.
# 
# }
#'         
#'    
#' @seealso \code{\link{dfuncEstim}}, \code{\link{abundEstim}}
#' 
#' @keywords model
#'
#' @export 

estimateN <- function(dfunc
                      , surveyedUnits
                      , area = NULL
                      , control = RdistanceControls()
                      ){
  
  
  # ---- Measurement units check. ----
  if( !dfunc$pointSurvey ){
    if( !inherits(surveyedUnits, "units") & control$requireUnits ){
      stop(paste("Transect length units are required.\n" 
                 , "Assign units to transect lengths in site data frame with statements like:\n"
                 , "siteData$length <- units::set_units(siteData$length, '<units>')\n"
                 , "'m' (meters), 'ft' (feet), 'km' (kilometers), etc. are acceptable.\n"
                 , "See units::valid_udunits()"))
    } else if( control$requireUnits ){
      # if we are here, length has units. convert them to units used during estimation.
      # Input dfunc must have a $outputUnits component.
      surveyedUnits <-  units::set_units(surveyedUnits, dfunc$outputUnits, mode = "standard")
    }
  } else if( inherits(surveyedUnits, "units") ){
    # Point surveys should not have units on surveyedUnits; 
    # suveyedUnits should just be number of points
    stop(paste("Measurement units on 'surveyedUnits' not allowed when analyzing\n" 
               , "point surveys.  Drop units with statements like:\n"
               , "surveyedUnits <- units::drop_units(surveyedUnits)\n"
               ))
  }
  
  
  if( !inherits(area, "units") & control$requireUnits ){
    if( !is.null(area) ){
      # If we are here, area did not come with units, we require units, and it's not null: ERROR
      stop(paste("Study area measurement units are required."
                 , "Assign units to area by attaching 'units' package then:\n"
                 , "units(area) <- '<units of measurment>'."
                 , "'m^2' (sq meters), 'ha' (hectares), 'km^2', and 'acre' are acceptable.\n"
                 , "See units::valid_udunits()"))
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
  
  # ---- Estimate numerator of abundance ----
  # REMEMBER: component $detections of dfunc has been truncated to (w.lo, w.hi)
  #           component $model.frame has NOT been truncated to the strip

  # sample size (number of detections, NOT individuals)
  n <- nrow(dfunc$detections)
  
  # If dfunc has covariates, esw is a vector of length n. No covars, esw is scalar
  # If dfunc is pointSurveys, esw is EDR.  If line surveys, esw is ESW.
  esw <- effectiveDistance(dfunc)
    
  w <- dfunc$w.hi - dfunc$w.lo
  
  if (dfunc$pointSurvey) {
    phat <- (esw / w)^2  # for points
  } else {
    phat <- esw / w  # for lines
  }

  # phat should be unit-less; check just to be sure, if so drop "1" units
  # Odd: sometimes phat has units "1", sometimes "m/m". Either way, test and remove.
  
  if( isUnitless(phat)  ){
    phat <- units::drop_units(phat)
  }
  
  nhat <- dfunc$detections$groupSize / phat # inflated counts one per detection
  
  # ---- Compute denominator of abundance, and abundance itself ----
  
  if (FALSE) {
    
    # MOVE ALL THIS TO A PREDICT METHOD
    
    if(!is.null(dfunc$covars)){
      covarsInModel <- attr(terms(dfunc$model.frame), "term.labels")
      allSiteLevelCovs <- all( covarsInModel %in% names(siteData) )
      if( !allSiteLevelCovs ) {
        nonSiteLevCovs <- paste("'",covarsInModel[!(covarsInModel %in% names(siteData))], 
                                "'",collapse = ", ", sep="")
        mess <-"Cannot estimate site-level abundance. bySite is TRUE but detection-level "
        if(regexpr(",", nonSiteLevCovs) < 0) {
          mess <- paste0(mess, "covariate ",
                       nonSiteLevCovs,
                       " is in the model. Options: remove this covariate from dfunc; ",
                       "set bySite=FALSE; or, summarize it to the site-level,", 
                       " place it in siteData under same name, and re-run ", 
                       "abundEstim WITHOUT re-running dfuncEstim.")
        } else {
          mess <- paste0(mess, "covariates ",
                         nonSiteLevCovs,
                         " are in the model. Options: remove these covariates from dfunc; ",
                         "set bySite=FALSE; or, summarize them to the site-level,", 
                         " place them in siteData under same names, and re-run ", 
                         "abundEstim WITHOUT re-running dfuncEstim.")
          
        }
        stop(mess)
      }
    }
    
    # ---- sum statistics by siteID

    nhat.df <- data.frame(siteID = detectionData$siteID, abundance=nhat)
    nhat.df <- tapply(nhat.df$abundance, nhat.df$siteID, sum)
    nhat.df <- data.frame(siteID = names(nhat.df), abundance=nhat.df)

    observedCount <- tapply(detectionData$groupsize, detectionData$siteID, sum)
    observedCount <- data.frame(siteID = names(observedCount), observedCount=observedCount)

    nhat.df <- merge(observedCount, nhat.df)  # should match perfectly

    # If detectionData$siteID is a factor and has all levels, even zero
    # sites, don't need to do this.  But, to be safe merge back to 
    # get zero transects and replace NA with 0 

    # Must do this to get pDetection on 0-sites.  Site must have 
    # non-missing covars if dfunc has covars
    esw <- effectiveDistance(dfunc, siteData)
    siteData <- data.frame(siteData, esw=esw)
    
    if (dfunc$pointSurvey) {
      siteData$pDetection <- (siteData$esw / w)^2  # for points
    } else {
      siteData$pDetection <- siteData$esw / w  # for lines
    }
    
    
    nhat.df <- merge(siteData, nhat.df, by="siteID", all.x=TRUE)
    nhat.df$observedCount[is.na(nhat.df$observedCount)] <- 0
    nhat.df$abundance[is.na(nhat.df$abundance)] <- 0
    
    # Calculate density
    if (dfunc$pointSurvey) {
      sampledArea <- pi * w^2  # area samled for single point  
    } else {
      sampledArea <- 2 * w * nhat.df$length  # area sampled for single line
    }
    nhat.df$density <- (nhat.df$abundance * area) / sampledArea
    
    
    # Calculate effective area ("effective" as in ESW)
    # This is suggested as the offset term in a GLM model of density
    if (dfunc$pointSurvey) {
      nhat.df$effArea <- (pi * nhat.df$esw^2) / area  # for points
    } else {
      nhat.df$effArea <- (nhat.df$length * nhat.df$esw * 2) / area  # for lines
    }
    
    
    # Replace the column name for "esw", which is edr for points
    if (dfunc$pointSurvey) {
      names(nhat.df)[names(nhat.df)=="esw"] <- "EDR"  # for points
    } else {
      names(nhat.df)[names(nhat.df)=="esw"] <- "ESW"  # for lines
    }
    
    
  } else {
    
    # ----  not bySite ----
    if(dfunc$pointSurvey){
      dens <- sum(nhat) / (pi * w^2 * surveyedUnits)
    } else {
      dens <- sum(nhat) / (2 * w * surveyedUnits)
    }
    
    nhat.df <- dens * area
    
    # nhat.df should be unitless
    if( isUnitless(nhat.df) ){
      nhat.df <- units::drop_units(nhat.df)
    } else {
      warning(paste("Strange measurement units detected because abundance is not unitless.\n", 
      "Check measurement unit compatability among distances, surveyedUnits, and area."))
    }
    
    nhat.df <- list(density = dens
                    , abundance = nhat.df
                    , n.groups = n
                    , area = area
                    , surveyedUnits = surveyedUnits
                    , avg.group.size = mean(dfunc$detections$groupSize)
                    , w = w
                    , pDetection = phat
                    # , nhat.sampleArea = sum(nhat)
                    # , observedCount = sum(dfunc$detections$groupSize)
                    # , esw = esw
                    )
  }
    
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
}  # end estimate.nhat function


