#' @title predict.dfunc.density - Density on transects
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
#' 
predict.dfunc.denisty <- function(x = x
                                  , propUnitSurveyed = 1.0
                               ){
  
  if( any(0 > propUnitSurveyed) | any(propUnitSurveyed > 1) ){
    stop(paste0("Proportion of unit surveyed must be between 0 and 1. "
              , "Found "
              , sum((0 > propUnitSurveyed) | (propUnitSurveyed > 1))
              , " values outside this range."
        ))
  }
  if( length(propUnitSurveyed) != 1 | length(propUnitSurveyed) != nrow(x$data) ){
    stop(paste0("Length of 'propUnitSurveyed' must either be 1 or "
              , nrow(x$data)
              , ". Found length "
              , length(propUnitSurveyed)
              , "."))
  }
  
  # Get siteId column(s) ----
  siteIDs <- x$data |> dplyr::group_vars()

  # Merge counts ----
  # Note: We would like to use x$mf which has observations ONLY
  # inside the observation window (w.hi - w.lo). But, x$mf
  # does not have the siteID columns nor does it have missing 
  # missing distances.  So, we use unnest(x$data), which has 
  # all observations, and we filter to the right ones. 
  
  mt <- terms(x$mf)
  distVar <- all.vars(mt)[attributes(mt)$response]
  groupSizeVar <- all.vars(mt)[attributes(mt)$offset]
  
  df <- Rdistance::unnest(x$data) 
  
  # Add group size if not specified in formula. ----
  #  If not specified (assumed 1), fake groupsizes are in x$mf
  #  but not in x$data. 
  
  # Note: to use the [missing distance but sighted group] functionality,
  # users must specify groupsize() in the formula. i.e., set some groupsizes
  # to positive, set their distances to missing, then specify group sizes 
  # in the original formula.  Otherwise, all groups are assumed size 1 and 
  # missing distances (from unnest) are assumed to be zero transects. 
  if( length(groupSizeVar) == 0 || !(groupSizeVar %in% names(df)) ){
    df <- df |> 
      dplyr::mutate(..groupSizes.. = dplyr::if_else(
        is.na(.data[[distVar]]),
        0, # assumed zero transects
        1
      )) 
  } else {
    names(df)[names(df) == groupSizeVar] <- "..groupSizes.."
  }
  
  # Compute ESW/EDR for every observation
  # at this point, df contains missing distances.
  # missing distances are zero transects and missing distances 
  # coupled with non-missing group sizes
  effDist <- Rdistance::effectiveDistance(x
                         , newdata = df)
  
  # Now can rename distance column for convienience
  names(df)[names(df) == distVar] <- "..distances.." 
  
  # filter to observations in strip OR those with 
  # missing distance but non-missing groupsizes
  instrip <- df |> 
    dplyr::bind_cols(tibble::tibble(effDist)) |> 
    dplyr::filter( 
      (is.na(..distances..) & !is.na(..groupSizes..)) 
      |
      ((x$w.lo <= ..distances..) & (..distances.. <= x$w.hi))
                 ) 
  # Counts by transect
  effVar <- attr(x$data, "effortColumn")
  w <- x$w.hi - x$w.lo
  if(is.points(x)){
    deCounts <- instrip |>
      dplyr::mutate( 
          pDetect = (effDist / w)^2
        , nAdjusted = ..groupSizes.. / pDetect) |> 
      dplyr::group_by(dplyr::across(dplyr::all_of(siteIDs))) |> 
      dplyr::summarise(individualsSeen = sum(..groupSizes..)
                       , avgPdetect = mean(pDetect)
                       , nAdjusted = sum(nAdjusted)
                       , effort = pi * propUnitSurveyed * w^2 * dplyr::first(.data[[effVar]])
                       , density = nAdjusted / effort
                       , abundance = nAdjusted
      ) |> 
      dplyr::select(-nAdjusted)
  } else {
    deCounts <- instrip |>
      dplyr::mutate( 
          pDetect = effDist / w
        , nAdjusted = ..groupSizes.. / pDetect) |> 
      dplyr::group_by(dplyr::across(dplyr::all_of(siteIDs))) |> 
      dplyr::summarise(individualsSeen = sum(..groupSizes..)
                     , avgPdetect = mean(pDetect)
                     , nAdjusted = sum(nAdjusted)
                     , effort = 2 * propUnitSurveyed * w * dplyr::first(.data[[effVar]])
                     , density = nAdjusted / effort
                     , abundance = nAdjusted
      ) |> 
      dplyr::select(-nAdjusted)
  }

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
    dplyr::mutate(dplyr::across(where(is.numeric),
                                .fns = drop1Units))
  
  return( deCounts )  
}
