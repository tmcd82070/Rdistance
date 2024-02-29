#' @title RdistDf - Construct Rdistance nested data frames
#' 
#' @description Makes an \code{Rdistance} data frame from 
#' separate transect and detection 
#' data frames. \code{Rdistance} data frames are nested 
#' data frames with one row per transect. Detection 
#' information for each transect appears in a list-based 
#' column that itself contains a
#' data frame. See section `Rdistance Data Frames`. 
#' 
#' \code{Rdistance} data frames can be constructed using calls to 
#' \code{dplyr::nest_by} and \code{dplyr::right_jion}, with subsequent 
#' attribute assignment (see \bold{Examples}). This routine is 
#' a convenience wrapper for those calls.
#' 
#' @param transectDf A data frame with one line per transect and 
#' columns containing information about the entire transect. 
#' At a minimum, this data frame must contain the transect's ID (so 
#' it can be merged with \code{detectionDf}, see parameter \code{by}) 
#' and the transect's length.  
#' All detections are made on a transect, but not all transects 
#' necessarily have detections. That is, \code{transectDf} 
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
#'   \item \bold{Transect IDs}: The ID of the transect on 
#'   which a target group was detected. Transect ID must be present 
#'   so that the detection data frame can be merged with \code{transectDf} 
#'   (see parameter \code{by}). Multiple detections on the same transect 
#'   are possible and hence multiple rows in \code{detectonDf} 
#'   can contain the same transect ID. 
#'      
#'   \item \bold{Detection Distances}: The distance at which each 
#'   detection was made. The distance column will eventually be 
#'   specified on the left-hand 
#'   side of \code{formula} in a call to \code{dfuncEstim}.  
#'   As of Rdistance version 3.0.0, detection distances must have 
#'   physical measurement units assigned. See 
#'   Section \bold{Measurment Units}. 
#'   
#' }
#' If group sizes of sighted targets are not all 1, \code{detectionDf} must 
#' contain a column specifying group sizes. 
#' Optionally, \code{detectionDf} can contain detection level covariates.
#' 
#' @param pointSurvey If TRUE, observations were
#' made from discrete points (i.e., during a point-transect survey) and distances 
#' are radial from observation point to target.  If FALSE, 
#' observations were made along a continuous transect 
#' (i.e., during a line-transect survey) and distances are from target to nearest 
#' point on the transect (i.e., perpendicular to transect).
#'    
#' @param observer Type of observer system.  Legal values are \bold{"single"} for 
#' single observer systems, \bold{"1given2"} for a double observer system 
#' wherein observations 
#' made by observer 1 are tested against observations made by observer 2,
#' \bold{"2given1"} for a double observer system wherein observations 
#' made by observer 2 are tested against observations made by observer 1, and
#' \bold{"both"} for a double observer system wherein observations 
#' made by both observers are tested against the other and combined.
#' 
#' @param .detectionCol Name of the list column that will
#' contain detection data frames. Default name is "detections".
#' 
#' @param by A character vector of variables to use in the join. The right-hand
#' side of this join identifies unique transects and will specify unique 
#' rows in both \code{transectDf} and the output (see warning in \bold{Details}).
#' If NULL, the join will be 'natural', using all variables in common between
#' \code{transectDf} and \code{detectionDf}. To join on 
#' specific variables, specify a character vector of 
#' the variables. For example, by = c("a", "b") 
#' joins \code{transectDf$a} to \code{detectionDf$a} and 
#' \code{transectDf$b} to \code{detectionDf$b}. If join variable names 
#' differ between \code{transectDf} and \code{detectionDf}, 
#' use a named character vector like by = c("a" = "b", 
#' "c" = "d") which joins \code{transectDf$a} to 
#' \code{detectionDf$b} and 
#' \code{transectDf$c} to \code{detectionDf$d}. 
#' 
#' 
#' @inheritSection dE.lt.single Measurement Units
#' 
#' @return A nested data frame with one row per transect, and detection 
#' information in a list column.  Technically, the return is 
#' a grouped \code{tibble} from 
#' the \code{tibble} package with one row per group, and a list 
#' column containing detection information. 
#' Survey type and observer system are recorded 
#' as attributes (\code{transType} and \code{obsType}, respectfully). 
#' 
#' @section Rdistance Data Frames: 
#' 
#' \code{RdistDf} data frames contain the following information:
#' \itemize{
#'   \item \bold{Transect Information}: At a minimum, each row of the 
#'   data frame contains transect id and length.  
#'   Optionally, transect-level covariates(such as habitat or observer
#'   id) appear on each row. 
#'   \item \bold{Detection Information}: At a minimum, observation distances
#'   (either perpendicular off-transect or radial off-point) appear 
#'   in a data frame stored in a list column.  If detected groups
#'   occasionally included more than one target, group sizes must 
#'   also be present in the data frame contained in the list column.
#'   Optionally, detection-level covariates (such as sex or size)
#'   can appear in the data frame of the list column.
#'   \item \bold{Distance Type}: The type of observation distances, either 
#'   perpendicular off-transect (for line-transects studies) 
#'       or radial off-point (for point-transect studies) must appear as an 
#'       attribute of \code{RdistDf}'s. 
#'   \item \bold{Observer Type}: The type of observation system used, either 
#'   single observer or one of three types of multiple observer systems, must 
#'   appear as an attribute of \code{RdistDf}'s.
#'
#' }  
#' 
#' @details 
#' 
#' For valid bootstrap estimates of confidence intervals (computed in \code{\link{abundEstim}}), 
#' each row of the nested data frame must represent one transect (more generally, 
#' one sampling unit), and none should
#' be duplicated. The combination of transect columns 
#' in \code{by} (i.e., the RHS of the merge, or "a" and "b" of 
#' \code{by = c("a" = "d", "b" = "c")} for example) 
#' should specify \emph{unique} transects and unique rows of 
#' \code{transectDf}. \bold{Warning:} If \code{by} 
#' does not specify unique rows of \code{transectDf}, \code{dplyr::left_join}, 
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
#' @seealso \code{\link{checkRdistDf}}: check validity of RdistDf data frames;
#'  \code{\link{dfuncEstim}}: estimate distance function.
#'  
#' @examples
#' 
#' sparrowDf <- RdistDf( sparrowSiteData, sparrowDetectionData )
#' 
#' # Equivalent to above: 
#' sparrowDf <- sparrowDetectionData |> 
#'   dplyr::nest_by( siteID
#'                , .key = "detections") |> 
#'   dplyr::right_join(sparrowSiteData, by = "siteID") 
#' attr(sparrowDf, "detectionColumn") <- "detections"
#' attr(sparrowDf, "obsType") <- "single"
#' attr(sparrowDf, "transType") <- "line"
#' class(sparrowDf) <- c("RdistDf", class(sparrowDf))
#' 
#' # Point transects
#' thrasherDf <- RdistDf( thrasherSiteData
#'                , thrasherDetectionData
#'                , pointSurvey = T
#'                , by = "siteID"
#'                , .detectionCol = "birds")
#'                
#' @export
#' 
RdistDf <- function( transectDf
                   , detectionDf
                   , by = NULL
                   , pointSurvey = FALSE
                   , observer = "single"
                   , .detectionCol = "detections"
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
  ans <- detectionDf |> 
    dplyr::nest_by( dplyr::across(dplyr::all_of(unname(by)))
                    , .key = .detectionCol
                    , .keep = FALSE) |> 
    dplyr::right_join(transectDf, by = by)

  attr(ans, "detectionColumn") <- .detectionCol
  attr(ans, "obsType") <- as.character(obsType)
  attr(ans, "transType") <- as.character(transType)
  
  # Could assign class this way; but, I wish outside ops would preserve
  # the extra classes.  they don't. dplyr::group_by wipes out 
  # obsType and transType class components
  # class(ans) <- c("RdistDf", class(ans))
  # class(ans) <- clsAns
  
  ans
}