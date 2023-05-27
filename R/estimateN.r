#' @title Abundance point estimates
#' 
#' @description Estimate abundance given a distance function, 
#' a "merged" data frame containing detections and transect lengths, area, 
#' and the number of sides surveyed (if line-transects).   
#' This is called internally by \code{abundEstim}.  Most users will call 
#' \code{abundEstim} to estimate abundance. 
#' 
#' @param dfunc An estimate distance function (see \code{dfuncEstim}).
#' 
#' @param surveyedUnits A scalar containing either the total length 
#' of surveyed transects (for line transects) or total number of surveyed
#' points (for point transects). This number is either $L$ or $P$ in the 
#' abundance formulas (Details).
#' 
#' @param data A data frame containing distance observations, transects, 
#' and lengths.  This data frame must have a column named 'siteID' that identifies
#' unique sites (transects or points). If observations on line-transects, this 
#' data frame must have a column named 
#' in the \code{lengthColumn} parameter that contains transect lengths. NA
#' length transects are accepted and are dropped when computing total 
#' transect length in denominator of density.
#' Only observations on non-NA transects are included in the numerator of density.
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
#'    \item{abundance}{Estimated abundance on the study area.}
#'    
#'    \item{n.groups}{The number of detections (not individuals, unless all group sizes = 1) 
#'    used to estimate density and abundance.}
#'    
#'    \item{n.seen}{The number of individuals (sum of group sizes) used to 
#'    estimate density and abundance.}
#'    
#'    \item{area}{Total area of inference. Study area size}
#'    
#'    \item{surveyedUnits}{Number of surveyed sites.  This is total transect length
#'    for line-transects and number of points for point-transects. This total transect
#'    length does not include NA transects.}
#'    
#'    \item{surveyedSides}{Number of sides (1 or 2) of transects surveyed. Only relevant for line-transects.}
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
#' @keywords model
#'
#' @export 

estimateN <- function(dfunc
                      , data
                      , area = NULL
                      , surveyedSides
                      , lengthColumn
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
  
  # ---- Find observations on NA length transects inside the strip ----
  mt <- terms(dfunc$model.frame)
  distColumn <- all.vars(mt)[attr(mt, "response")]
  inStrip <- (dfunc$w.lo <= data[,distColumn]) &
    (data[,distColumn] <= dfunc$w.hi)
  onPosTransect <- !is.na(data[, lengthColumn ])
  distPresent <- !is.na(data[, distColumn])
  obsInd <- inStrip & onPosTransect & distPresent
  
  # ---- Estimate numerator of abundance ----
  # REMEMBER: component $detections of dfunc has been truncated to (w.lo, w.hi)
  #           component $model.frame has NOT been truncated to the strip

  # If dfunc has covariates, esw is a vector of length n. No covars, esw is scalar
  # If dfunc is pointSurveys, esw is EDR.  If line surveys, esw is ESW.
  esw <- effectiveDistance(dfunc
                         , newdata = data[ obsInd, , drop = FALSE])
    
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

  if( !is.null(attr(mt, "offset")) ){
    gsCol <- all.vars(mt)[attr(mt, "offset")]
    gs <- data[, gsCol]
    gs <- gs[ obsInd ]
  } else {
    gs <- rep(1, sum(obsInd))
  }
  
  nhat <- gs / phat # inflated counts one per detection
  
  # ---- Compute denominator of abundance, and abundance itself ----
  
  if (FALSE) {
    
    # MOVE ALL THIS TO A PREDICT METHOD
    
    # if(!is.null(dfunc$covars)){
    #   covarsInModel <- attr(terms(dfunc$model.frame), "term.labels")
    #   allSiteLevelCovs <- all( covarsInModel %in% names(siteData) )
    #   if( !allSiteLevelCovs ) {
    #     nonSiteLevCovs <- paste("'",covarsInModel[!(covarsInModel %in% names(siteData))], 
    #                             "'",collapse = ", ", sep="")
    #     mess <-"Cannot estimate site-level abundance. bySite is TRUE but detection-level "
    #     if(regexpr(",", nonSiteLevCovs) < 0) {
    #       mess <- paste0(mess, "covariate ",
    #                    nonSiteLevCovs,
    #                    " is in the model. Options: remove this covariate from dfunc; ",
    #                    "set bySite=FALSE; or, summarize it to the site-level,", 
    #                    " place it in siteData under same name, and re-run ", 
    #                    "abundEstim WITHOUT re-running dfuncEstim.")
    #     } else {
    #       mess <- paste0(mess, "covariates ",
    #                      nonSiteLevCovs,
    #                      " are in the model. Options: remove these covariates from dfunc; ",
    #                      "set bySite=FALSE; or, summarize them to the site-level,", 
    #                      " place them in siteData under same names, and re-run ", 
    #                      "abundEstim WITHOUT re-running dfuncEstim.")
    #       
    #     }
    #     stop(mess)
    #   }
    # }
    # 
    # # ---- sum statistics by siteID
    # 
    # nhat.df <- data.frame(siteID = detectionData$siteID, abundance=nhat)
    # nhat.df <- tapply(nhat.df$abundance, nhat.df$siteID, sum)
    # nhat.df <- data.frame(siteID = names(nhat.df), abundance=nhat.df)
    # 
    # observedCount <- tapply(detectionData$groupsize, detectionData$siteID, sum)
    # observedCount <- data.frame(siteID = names(observedCount), observedCount=observedCount)
    # 
    # nhat.df <- merge(observedCount, nhat.df)  # should match perfectly
    # 
    # # If detectionData$siteID is a factor and has all levels, even zero
    # # sites, don't need to do this.  But, to be safe merge back to 
    # # get zero transects and replace NA with 0 
    # 
    # # Must do this to get pDetection on 0-sites.  Site must have 
    # # non-missing covars if dfunc has covars
    # esw <- effectiveDistance(dfunc, siteData)
    # siteData <- data.frame(siteData, esw=esw)
    # 
    # if (dfunc$pointSurvey) {
    #   siteData$pDetection <- (siteData$esw / w)^2  # for points
    # } else {
    #   siteData$pDetection <- siteData$esw / w  # for lines
    # }
    # 
    # 
    # nhat.df <- merge(siteData, nhat.df, by="siteID", all.x=TRUE)
    # nhat.df$observedCount[is.na(nhat.df$observedCount)] <- 0
    # nhat.df$abundance[is.na(nhat.df$abundance)] <- 0
    # 
    # # Calculate density
    # if (dfunc$pointSurvey) {
    #   sampledArea <- pi * w^2  # area samled for single point  
    # } else {
    #   sampledArea <- 2 * w * nhat.df$length  # area sampled for single line
    # }
    # nhat.df$density <- (nhat.df$abundance * area) / sampledArea
    # 
    # 
    # # Calculate effective area ("effective" as in ESW)
    # # This is suggested as the offset term in a GLM model of density
    # if (dfunc$pointSurvey) {
    #   nhat.df$effArea <- (pi * nhat.df$esw^2) / area  # for points
    # } else {
    #   nhat.df$effArea <- (nhat.df$length * nhat.df$esw * 2) / area  # for lines
    # }
    # 
    # 
    # # Replace the column name for "esw", which is edr for points
    # if (dfunc$pointSurvey) {
    #   names(nhat.df)[names(nhat.df)=="esw"] <- "EDR"  # for points
    # } else {
    #   names(nhat.df)[names(nhat.df)=="esw"] <- "ESW"  # for lines
    # }
    
    
  } else {

    # ----  not bySite ----
    
    # ---- Compute number of points or transect length ----
    if( dfunc$pointSurvey ){
      totSurveyedUnits <- length(unique(data$siteID))
    } else {
      # Note: drop NA length transects here, which may have detections
      # There are dups because data has one row per detection and there 
      # are multiple detections per transect.
      dups <- duplicated(data$siteID)
      totSurveyedUnits <- sum(data[!dups,,drop=FALSE][,lengthColumn], na.rm = TRUE)
    }
    
    if(dfunc$pointSurvey){
      dens <- sum(nhat) / (pi * w^2 * surveyedUnits)
    } else {
      dens <- sum(nhat) / (surveyedSides * w * surveyedUnits)
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
                    , n.groups = length(gs)
                    , n.seen = sum(gs)
                    , area = area
                    , surveyedUnits = surveyedUnits
                    , surveyedSides = surveyedSides
                    , avg.group.size = mean(gs)
                    , range.group.size = range(gs)
                    , w = w
                    , pDetection = phat
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


