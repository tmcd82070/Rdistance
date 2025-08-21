#' @title Density on transects
#' 
#' @description
#' An internal prediction method for computing density 
#' on the sampled transects. 
#' 
#' @inheritParams predict.dfunc 
#' @inheritParams abundEstim
#' 
#' @return A data frame containing the original data used 
#' to fit the distance function, plus an additional column
#' containing the density of individuals on each transect. 
#' 
#' @examples
#' data(sparrowDfuncObserver)
#' predict(sparrowDfuncObserver, type="density")
#' 
#'  
#' @export
predDensity <- function(object
                      , propUnitSurveyed = 1.0
                       ){
  
  if( any(0 > propUnitSurveyed) | any(propUnitSurveyed > 1) ){
    stop(paste0("Proportion of unit surveyed must be between 0 and 1. "
              , "Found "
              , sum((0 > propUnitSurveyed) | (propUnitSurveyed > 1))
              , " values outside this range."
        ))
  }
  if( length(propUnitSurveyed) != 1 && length(propUnitSurveyed) != nrow(object$data) ){
    stop(paste0("Length of 'propUnitSurveyed' must either be 1 or "
              , nrow(object$data)
              , ". Found length "
              , length(propUnitSurveyed)
              , "."))
  }
  
  # Get siteId column(s) ----
  siteIDs <- object$data |> dplyr::group_vars()

  # Merge counts ----
  # Note: We would like to use object$mf which has observations ONLY
  # inside the observation window (w.hi - w.lo). But, object$mf
  # does not have the siteID columns nor does it have missing 
  # missing distances.  So, we use unnest(object$data), which has 
  # all observations, and we filter to the right ones. 
  
  mt <- stats::terms(object$mf)
  distVar <- all.vars(mt)[attributes(mt)$response]
  groupSizeVar <- all.vars(mt)[attributes(mt)$offset]
  effVar <- attr(object$data, "effortColumn")
  
  # Gotta use object$data because object$mf has no missing distances
  # nor the zero transects. $data has these
  df <- Rdistance::unnest(object$data) 
  
  # Must add group size to df if not specified in formula. ----
  #  If not specified (i.e., assumed 1), fake groupsizes are in object$mf
  #  but not in object$data. 
  
  # Note: to use the [missing distance but sighted group] functionality,
  # users must specify groupsize() in the formula. i.e., set some groupsizes
  # to positive, set their distances to missing, then specify group sizes 
  # in the original formula.  Otherwise, all groups are assumed size 1 and 
  # missing distances (from unnest) are assumed to be zero transects. 
  if( length(groupSizeVar) == 0 || !(groupSizeVar %in% names(df)) ){
    # I don't think length(groupSizeVar) == 0 ever happens b/c there is always an offset
    # BUT, groupSizeVar is not in df if we made up the groupsize in parseModel
    ..groupSizes.. <- dplyr::if_else(
        is.na(df[[distVar]]),
        0, # assumed zero transects, non-missing group sizes overwritten
        1
      ) 
    df[,groupSizeVar] <- ..groupSizes.. # needed for effectiveDistance
  } else {
    # Gotta set missing group sizes to 0, in df, or model.frame inside 
    # predict method (that is inside effectiveDistance(), below) tosses 
    # the missings. ( I have na.action set to 
    # na.omit in model.frame, and group size is in the Terms so any 
    # missing are dropped)
    df <- df |> 
      dplyr::mutate(dplyr::across(dplyr::all_of(groupSizeVar)
                                  , .fns = function(.x){
                                    .x[is.na(.x)] <- 0
                                    .x
                                  }))
    ..groupSizes.. <- df[[groupSizeVar]]
  }
  
  # Note: at this point, df has the group size variable, and 
  # there are no missing groupsizes. Zero transects have 0 groupsize.
  
  # Compute ESW/EDR for every observation
  # at this point, df contains missing distances.
  # missing distances are zero transects and missing distances 
  # coupled with non-missing group sizes
  effDist <- Rdistance::effectiveDistance(object
                         , newdata = df)
  
  # Now can pull columns for convenience
  ..distances.. <- df[[distVar]] # don't call distances() b/c need zero transects
  ..effort.. <- effort(object) # nTransects long
  
  # Compute index of observations in strip OR those with 
  # missing distance but non-missing groupsizes
  ..instrip.. <- (is.na(..distances..) & !is.na(..groupSizes..)) |
             ((object$w.lo <= ..distances..) & (..distances.. <= object$w.hi))
  
  # Compute estimates by transect
  w <- object$w.hi - object$w.lo # length 1
  if(is.points(object)){
    ..pDetect.. <- (effDist / w)^2
    ..effort.. <- pi * propUnitSurveyed * w^2 * ..effort..
  } else {
    ..pDetect.. = effDist / w
    ..effort.. = 2 * propUnitSurveyed * w * ..effort..
  }
  # NOTE: At this point, there cannot be any missing ..groupSizes.. or ..pDetect..
  ..nAdjusted.. <- ..groupSizes.. / ..pDetect..  # nObs+nZero long
  deCounts <- df |>
    dplyr::mutate(..groupSizes.. = ..groupSizes..
                , ..pDetect.. = ..pDetect..
                , ..nAdjusted.. = ..nAdjusted..
                , ..instrip.. = ..instrip.. ) |> 
    dplyr::group_by(dplyr::across(dplyr::all_of(siteIDs))) |> 
    dplyr::summarise(individualsSeen = sum(..groupSizes..[..instrip..])
                     , avgPdetect = dplyr::if_else(
                             sum(..instrip..) > 0
                           , mean(..pDetect..[..instrip..])
                           , mean(..pDetect..)
                         )
                     , ..nAdjusted.. = sum(..nAdjusted..[..instrip..])
    ) 
  deCounts <- deCounts |> 
    dplyr::ungroup() |> 
    dplyr::mutate(density = ..nAdjusted.. / ..effort..
                , effort = ..effort..
    ) |> 
    dplyr::rename("abundance" = "..nAdjusted..")
  
  # NOTE: the "..XXX.." variables and renaming of abundance and 
  # the strange MUTATE statements above are to get around CRAN checks
  # that don't recognize new variables inside mutate statements. 

  # Internal function to remove units from unitless columns ----  
  drop1Units <- function(x){
    unitless <- units::set_units(1,"1")
    if(inherits(x, "units")){
      if( units(x) == units(unitless) ){
        x <- units::set_units(x, NULL)
      }
    }
    x
  }
  
  # Remove units from unitless columns ----
  deCounts <- deCounts |> 
    dplyr::mutate(dplyr::across(tidyselect::where(is.numeric),
                                .fns = drop1Units))
  
  return( deCounts )  
}
