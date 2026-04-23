#' @title Construct Rdistance nested data frames
#' 
#' @description Makes an `Rdistance` data frame from 
#' separate transect and detection 
#' data frames. `Rdistance` data frames are nested 
#' data frames with one row per transect. Detection 
#' information for each transect appears in a list-based 
#' column that itself contains a
#' data frame. See **Rdistance Data Frames**. 
#' 
#' `Rdistance` data frames can be constructed using calls to 
#' `dplyr::nest_by` and `dplyr::right_jion`, with subsequent 
#' attribute assignment (see **Examples**). This routine is 
#' a convenient wrapper for those calls.
#' 
#' @param transectDf A data frame with one row per transect and 
#' columns containing information about the entire transect. 
#' At a minimum, this data frame must contain the transect's ID so 
#' it can be merged with `detectionDf`, (see parameter `by`) 
#' and the amount of effort the transect represents 
#' (see parameter `.effortCol`).  
#' All detections are made on a transect, but not all transects 
#' require detections. That is, `transectDf` 
#' should contain rows, and hence
#' ID's and lengths, of all surveyed transects, even those on 
#' which no targets were detected (so-called "zero transects"). 
#' Transect-level covariates, such 
#' as habitat type, elevation, or observer IDs, should appear as variables 
#' in this data frame. 
#' 
#' @param detectionDf A data frame containing   
#' detection information associated with each transect.  
#' At a minimum, each row of this data 
#' frame must contain the following:
#' \itemize{
#'   \item **Transect IDs**: The ID of the transect on 
#'   which a target group was detected so that the 
#'   detection data frame can be merged with `transectDf` 
#'   (see parameter `by`). Multiple detections on the same transect 
#'   are possible and hence multiple rows in `detectonDf` 
#'   can contain the same transect ID. 
#'      
#'   \item **Detection Distances**: The distance at which each 
#'   detection was made. The distance column will eventually be 
#'   specified on the left-hand 
#'   side of `formula` in a call to `dfuncEstim`.  
#'   As of Rdistance version 3.0.0, detection distances must have 
#'   physical measurement units assigned. See 
#'   **Measurement Units**. 
#' }
#' Optional columns in `detectionDf`:
#' \itemize{
#'    \item **Group sizes**:If sighted targets vary in size, or group sizes 
#'          are not all 1, `detectionDf` must 
#'          also contain a column specifying group sizes. Non-unity group sizes 
#'          are specified using `+groupsize(columnName)` on the 
#'          right-hand-side of `formula` in an eventual call to `dfuncEstim`. 
#'          
#'    \item **Detection Level Covariates**: Such as sex, color, habitat, etc. 
#' }
#' 
#' @param pointSurvey If TRUE, observations were
#' made from discrete points (i.e., during a point-transect survey) and distances 
#' are radial from observation point to target.  If FALSE, 
#' observations were made along a continuous transect 
#' (i.e., during a line-transect survey) and distances are from target to nearest 
#' point on the transect (i.e., perpendicular to transect).
#'    
#' @param observer Type of observer system.  Legal values are **"single"** for 
#' single observer systems, **"1given2"** for a double observer system 
#' wherein observations 
#' made by observer 1 are tested against observations made by observer 2,
#' **"2given1"** for a double observer system wherein observations 
#' made by observer 2 are tested against observations made by observer 1, and
#' **"both"** for a double observer system wherein observations 
#' made by both observers are tested against the other and combined.
#' 
#' @param .detectionCol Name of the list column that will
#' contain detection data frames. Default name is "detections".
#' Detection distances (LHS of `dfuncEstim` formula) and 
#' group sizes are normally 
#' columns in the nested detection data frames embedded in 
#' `.detectionCol`. 
#' 
#' @param .effortCol For continuous line transects, 
#' this specifies the name of a column in `transectDf` 
#' containing transect lengths, which must have measurement units.  
#' For point transects, this specifies the name of a column containing 
#' the number of points on each transect.  The effort column for point 
#' transects *cannot* contain 
#' measurement units. Default is "length" for line-transects, "numPoints" for 
#' point-transects. If those names are not found, the first column
#' in the merged data frame whose name contains 'point' 
#' (for point transects) or 'length' (for line transects)
#' is used and a message is printed. Matching is case insensitive, 
#' so for example, 'nPoints' and 
#' 'N_point' and 'numberOfPoints' will all be matched.   If two or more column 
#' names match the effort column search terms, a warning is issued. 
#' See **Transect Lengths** for a description of point and line transects.
#' 
#' @param by A character vector of variables to use in the join. The right-hand
#' side of this join identifies unique transects (unique 
#' rows) in both `transectDf` and the output (see warning in **Details**).
#' If NULL, the join will be 'natural', using all common variables 
#' in `transectDf` and `detectionDf`. To join on 
#' specific variables, specify a character vector of 
#' the variables. For example, by = c("a", "b") 
#' joins `transectDf$a` to `detectionDf$a` and 
#' `transectDf$b` to `detectionDf$b`. If join variable names 
#' differ between `transectDf` and `detectionDf`, 
#' use a named character vector like by = c("a" = "b", 
#' "c" = "d") which joins `transectDf$a` to 
#' `detectionDf$b` and 
#' `transectDf$c` to `detectionDf$d`. 
#' 
#' 
#' @inheritSection dE.single Measurement Units
#' 
#' @return A nested tibble (a generalization of base data frames) 
#' with one row per transect, and detection 
#' information in a list column.  Technically, the return is 
#' a grouped `tibble` from 
#' the `tibble` package with one row per group, and a list 
#' column containing detection information. 
#' Survey type, observer system, and name of the effort column 
#' are recorded 
#' as attributes (`transType`, `obsType`, and `effortColumn`, respectfully). 
#' The return prints nicely using methods 
#' in package `tibble`.  If returned objects print strangely, 
#' attach library `tibble`.  A summary method tailored to distance sampling 
#' is available (i.e., `summary(return)`).
#' 
#' @section Rdistance Data Frames: 
#' 
#' `RdistDf` data frames contain the following information:
#' 
#' \itemize{
#'   \item **Transect Information**: Each row of the 
#'   data frame contains transect id and effort. Effort is 
#'   transect length for line-transects, and number of points for 
#'   point-transects.
#'   Optionally, transect-level covariates (such as habitat or observer
#'   id) appear on each row. 
#'   \item **Detection Information**: Observation distances
#'   (either perpendicular off-transect or radial off-point) appear 
#'   in a data frame stored in a list column.  If detected groups
#'   occasionally included more than one target, a group size column must 
#'   be present in the list-column data frame.
#'   Optionally, detection-level covariates (such as sex or size)
#'   can appear in the data frame of the list column.
#'   \item **Distance Type**: The type of observation distances, either 
#'   perpendicular off-transect (for line-transects studies) or 
#'   radial off-point (for point-transect studies) must appear as an 
#'   attribute of `RdistDf`'s. 
#'   \item **Observer Type**: The type of observation system used, either 
#'   single observer or one of three types of multiple observer systems, must 
#'   appear as an attribute of `RdistDf`'s.
#' }
#'
#' 
#' @details 
#' 
#' For valid bootstrap estimates of confidence intervals (computed in [abundEstim()]), 
#' each row of the nested data frame must represent one transect (more generally, 
#' one sampling unit), and none should
#' be duplicated. The combination of transect columns 
#' in `by` (i.e., the LHS of the merge, or "a" and "b" of 
#' `by = c("a" = "d", "b" = "c")` for example) 
#' should specify *unique* transects and unique rows of 
#' `transectDf`. **Warning:** If `by` 
#' does not specify unique rows of `transectDf`, `dplyr::left_join`, 
#' which is called internally, will perform a many-to-many merge without 
#' warning, and this normally duplicates both 
#' transects and detections.
#'
#' 
#' @section Transect Lengths:
#' Line-transects are continuous paths with targets detectable at 
#' any point.  Point transects
#' consist of one or more discrete points along a path 
#' from which observers search for targets. 
#' The length of a line-transect is it's physical length (e.g., km or miles).
#' The 'length' of a point transect is 
#' the number of points along the transect. Single 
#' points are considered transects of length one. The length of line-transects
#' must have a physical measurement unit (e.g., 'm' or 'ft').  The length of 
#' point-transects must be a unit-less integers (i.e., number of points).
#' 
#'  
#' @seealso [is.RdistDf()]: check validity of RdistDf data frames;
#'  [dfuncEstim()]: estimate distance function.
#'  
#' @examples
#' data(sparrowSiteData)
#' data(sparrowDetectionData)
#' 
#' sparrowDf <- RdistDf( sparrowSiteData, sparrowDetectionData )
#' is.RdistDf(sparrowDf, verbose = T)
#' summary(sparrowDf)
#' summary(sparrowDf
#'       , formula = dist ~ groupsize(groupsize)
#'       , w.hi = 100 %m%.)
#' 
#' # Equivalent to above: 
#' sparrowDf <- sparrowDetectionData |> 
#'   dplyr::nest_by( siteID
#'                , .key = "detections") |> 
#'   dplyr::right_join(sparrowSiteData, by = "siteID") 
#' attr(sparrowDf, "detectionColumn") <- "detections"
#' attr(sparrowDf, "effortColumn") <- "length"
#' attr(sparrowDf, "obsType") <- "single"
#' attr(sparrowDf, "transType") <- "line"
#' is.RdistDf(sparrowDf, verbose = T)
#' summary(sparrowDf, formula = dist ~ groupsize(groupsize))
#'
#' # Condensed view: 1 row per transect (make sure tibble is attached)
#' sparrowDf
#' 
#' # Expansion methods:
#' # (1) use Rdistance::unnest (includes zero transects)
#' df1 <- unnest(sparrowDf)
#' any( df1$siteID == "B2" )  # TRUE
#' 
#' # Use tidyr::unnest(); but, no zero transects
#' df2 <- tidyr::unnest(sparrowDf, cols = "detections")
#' any( df2$siteID == "B2" )  # FALSE
#' 
#' # Use dplyr::reframe for specific transects (e.g., for transect "B3")
#' sparrowDf |> 
#'   dplyr::filter(siteID == "B3") |>
#'   dplyr::reframe(detections)
#'   
#' # Count detections per transect (can't use dplyr::if_else)
#' df3 <- sparrowDf |> 
#'   dplyr::reframe(nDetections = ifelse(is.null(detections), 0, nrow(detections)))
#' sum(df3$nDetections) # Number of detections
#' sum(df3$nDetections == 0) # Number of zero transects
#'     
#' # Point transects
#' data(thrasherDetectionData)
#' data(thrasherSiteData)
#' thrasherDf <- RdistDf( thrasherSiteData
#'                , thrasherDetectionData
#'                , pointSurvey = TRUE
#'                , by = "siteID"
#'                , .detectionCol = "detections")
#' summary(thrasherDf, formula = dist ~ groupsize(groupsize))
#'                
#' @export
#' 
RdistDf <- function( transectDf
                   , detectionDf
                   , by = NULL
                   , pointSurvey = FALSE
                   , observer = "single"
                   , .detectionCol = "detections"
                   , .effortCol = NULL
                   ){
  
  # Check lengths ----
  # Why? Because switch throws error when EXPR > length 1
  if( length(pointSurvey) > 1 ){
    warning(paste0("'pointSurvey' has length", length(pointSurvey), 
                  ". Only its first element was used."))
    pointSurvey <- pointSurvey[1]
  }
  if( length(observer) > 1 ){
    warning(paste0("'observer' has length", length(observer), 
                   ". Only its first element was used."))
    observer <- observer[1]
  }
  
  # translate inputs to class values ----
  obsType <-  factor(observer
                   , levels = c("single", "1given2", "2given1", "both")
                   )
  transType <-  factor(pointSurvey
                       , levels = c(TRUE, FALSE)
                       , labels = c("point", "line")
                       )

  # check inputs ----
  if( is.na(obsType) ){
    stop(paste("Value of 'observer' must be one of 'single', '1given2', '2given1', or 'both'."
               , "Found", paste(observer, collapse = ",")))
  }
  if( is.na(transType) ){
    stop(paste("Value of 'pointSurvey' must be either TRUE or FALSE."
               , "Found", paste(pointSurvey, collapse = ",")))
  }

  # Nest detection data frame first ----
  if( is.null(by) ){
    by <- intersect(names(transectDf), names(detectionDf))
    if( length(by) == 0){
      stop(paste("No variables in common to merge transects and detections."
                 , "Consider using format 'by = c('a' = 'b')' to'
                 , 'join on columns 'a' and 'b'."))
    }
  } 
  
  # 'by' now has a value always, check it is named
  if( is.null(names(by)) ){
    # e.g., by = c("id", "region")
    reverseBy <- by
    names(reverseBy) <- by
  } else {
    # by is specified. b/c we nest first, then right join, we 
    # gotta reverse names and values in 'by'
    reverseBy <- names(by)
    names(reverseBy) <- by
  }
  ans <- detectionDf |> 
    dplyr::nest_by( dplyr::across(dplyr::all_of(unname(by)))
                    , .key = .detectionCol
                    , .keep = FALSE) |> 
    dplyr::right_join(transectDf, by = reverseBy)

  attr(ans, "detectionColumn") <- .detectionCol
  attr(ans, "obsType") <- as.character(obsType)
  attr(ans, "transType") <- as.character(transType)
  
  # Rename ID column: b/c we nested first, then merged, the ID column
  # in the result is names the first element of reverseBy. 
  # Don't need to do this if by == reverseby, but...no harm if so
  ans <- ans |> 
    dplyr::rename_with(
        .fn = function(inNm, nmMap){nmMap}
      , .cols = names(reverseBy), nmMap = reverseBy
    )

  # Figure out effort column ----
  if( is.null(.effortCol) ){
    if( pointSurvey ){
      .effortCol <- "numPoints"
    } else {
      .effortCol <- "length"
    }
  }
  if( !(.effortCol %in% names(ans)) ){
    if( pointSurvey ){
      searchTerm <- "(p|P)(o|O)(i|I)(n|N)(t|T)"
      torp <- "number of points"
    } else {
      searchTerm <- "(l|L)(e|E)(n|N)(g|G)(t|T)(h|H)"
      torp <- "transect length"
    }
    candidates <- grep(searchTerm, names(ans))
    if(length(candidates) == 0){
      # we tried
      stop(paste0("Effort column '", .effortCol, "' not found.", 
                  " Specify effort column using .effortCol input parameter. ",
                  "Potential effort column(s): ", 
                  paste(names(ans)[candidates], collapse = ", ")))
    } else if(length(candidates) > 1){
      warning(paste0("Found two or more potential effort columns. "
                  , "Using "
                  , names(ans)[candidates[1]]
                  , " as "
                  , torp
                  , ". Specify a different effort column using the .effortCol input parameter."
                  ))
    } else {
      cat(paste0(
                 "Using "
                , colorize(names(ans)[candidates[1]])
                , " as "
                , torp
                , ". Specify a different effort column using the .effortCol input parameter."
      ))
    }
    .effortCol <- names(ans)[candidates[1]]
  }
  attr(ans, "effortColumn") <- .effortCol
  
  # Could assign class this way; but, I wish outside ops would preserve
  # the extra classes.  they don't. dplyr::group_by wipes out 
  # obsType and transType class components
  # class(ans) <- c("RdistDf", class(ans))
  # class(ans) <- clsAns
  
  ans
}
