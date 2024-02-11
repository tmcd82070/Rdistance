#' @title RdistDf - Construct Rdistance nested data frames
#' 
#' @description Makes an \code{Rdistance} data frame from 
#' separate transect and detection 
#' data frames. \data{Rdistance} data frames are nested 
#' data frames with one row per transect and detection 
#' information in a list-based column that itself contains a
#' data frame. \code{Rdistance} contain the following information:
#' \itemize{
#'   \item \bold{Transect Information}: At a minimum, each row of the 
#'   data frame must contain transect id and length.  
#'   Optionally, transect-level covariate variables (such as habitat or observer
#'   id) can appear on each row. 
#'   \item \bold{Detection Information}: At a minimum, observation distances
#'   (either perpendicular off-transect or radial) must appear in the 
#'   list column on each row.  If detections occasionally included  
#'   more than one target in a group, group sizes must 
#'   also be present in the list column alongside observation distances.  
#'   Optionally, detection-level covariates (such as sex or size)
#'   can appear in the list column.
#'   \item \bold{Distance Type}: The type of observation distances, either 
#'   perpendicular off-transect (for line-transects studies) 
#'       or radial off-point (for point-transect studies) must appear as an 
#'       attribute of \code{Rdistance} data frames. 
#'   \item \bold{Observer Type}: The type of observation system used, either 
#'   single observer or one of three types of multiple observer systems, must 
#'   appear as an attribute of \code{Rdistance} data frames.
#'
#' }
#' \code{Rdistance} data frames can be constructed using calls to 
#' \code{dplyr::nest_by} and \code{dplyr::right_jion}, with subsequent 
#' attribute assignment (see \bold{Examples}). This routine is 
#' a convenience wrapper for those calls.  
#' 
#' 
#' @param transectDf A data frame with one line per transect and 
#' variables containing transect information. 
#' At a minimum, this data frame must contain the transect's ID (so 
#' it can be merged with \code{detectionDf}, see parameter \code{by}) 
#' and the transect's length.  
#' All detections are made on a transect, but not all transects necessarily have 
#' observations. That is, the \code{transectDf} should contain rows, and hence
#' ID's and lengths, of all surveyed transects, even those on which no targets 
#' were detected (so-called "zero transects"). Transect-level covariates, such 
#' as habitat type, elevation, or observer IDs, should appear as variables 
#' in this data frame.
#' 
#' @param detectionDf A data frame containing information on the 
#' detections made on each transect.  At a minimum, each row of this data 
#' frame must contain 
#' the following:
#' \itemize{
#'   \item \bold{Transect IDs}: The ID of the transect on 
#'   which a target group was detected. Transect ID must be present 
#'   so that the detection data frame can be merged with \code{transectDf} 
#'   (see parameter \code{by}). Multiple detections on the same transect 
#'   result in multiple transect ID's across several rows of this data base. 
#'   
#'   here!!!
#'      
#'   \item \bold{Detection Distances}: A single column containing 
#'   detection distances. This column will be specified on the left-hand 
#'   side of \code{formula} in a later call to \code{dfuncEstim}.  
#'   As of Rdistance version 3.0.0, detection distances must have 
#'   physical measurement units attached. See 
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
#' @param .distanceCol Name of the list column that
#' contains distances. Default name is "distances".
#' 
#' @param by A character vector of variables to join on. The right-hand
#' side of this join identifies unique transects and will specify unique 
#' rows in the output (see \bold{Details}).
#' If NULL, \code{RdistDf}⁠ will perform a
#' natural join, using all variables in common between
#' \code{transectDf} and \code{detectionDf}. To join on 
#' specific variables, specify a character vector of 
#' variable names to join by. For example, by = c("a", "b") 
#' joins \code{transectDf$a} to \code{detectionDf$a} and 
#' \code{transectDf$b} to \code{detectionDf$b}. If variable names 
#' differ between \code{transectDf} and \code{detectionDf}, 
#' use a named character vector like by = c("a" = "b", 
#' "c" = "d") which joins \code{transectDf$a} to 
#' \code{detectionDf$b} and 
#' \code{transectDf$c} to \code{detectionDf$d}. 
#' 
#' 
#' @inheritSection dE.lt.single Measurement Units
#' 
#' @return A nested dataframe with one row per transect, and observation 
#' information (detections) in a list column.  Technically, the return is 
#' a \code{tibble} from 
#' the \code{tibble} package with a list column containing 
#' distance, group size, and (potentially) covariate information. 
#' Survey type and observer system are recorded 
#' as attributes (\code{transType} and \code{obsType}, respectfully). Duplicate
#' transects or detections, if produced, are not identified. 
#' 
#' @details 
#' For the bootstrap method in \code{\link{abundEstim}} to yield 
#' accurate confidence intervals, each row of the nested
#' data frame should represent one transect (or sampling unit), and none should
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
#' @section Transect Lengths:
#' Line-transects are continuous paths wherein targets can 
#' be sighted at any point.  Point transects
#' consist of one or more discrete points from which observers search for targets. 
#' The length of a line-transect is it's physical length in 2D space.
#' The 'length' of a point transect is 
#' the number of points along the transect. Single 
#' points are considered transects of length one. The length of line-transects
#' must have a physical measurement unit (e.g., 'm' or 'ft').  The length of 
#' point-transects must be a unit-less integers (i.e., number of points).
#'  
#' @examples
#' 
#' sparrowDf <- RdistDf( sparrowSiteData, sparrowDetectionData )
#' 
#' # Equivalent to above, but with re-ordered rows and columns 
#' sparrowDf <- sparrowDetectionData |> 
#'   dplyr::nest_by( siteID
#'                , .key = "distances") |> 
#'   dplyr::right_join(sparrowSiteData, by = "siteID") |> 
#'   dplyr::ungroup()  
#' attr(sparrowDf, "distColumn") <- "distances"
#' attr(sparrowDf, "obsType") <- "single"
#' attr(sparrowDf, "transType") <- "line"
#' 
#' thrasherDf <- RdistDf( thrasherSiteData
#'                , thrasherDetectionData
#'                , pointSurvey = T
#'                , by = "siteID")
#'                
#' @export
#' 
RdistDf <- function( transectDf
                   , detectionDf
                   , by = NULL
                   , pointSurvey = FALSE
                   , observer = "single"
                   , .distanceCol = "distances"
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
                    , .key = .distanceCol
                    , .keep = FALSE) |> 
    dplyr::right_join(transectDf, by = by)
  
  
  attr(ans, "distColumn") <- .distanceCol
  attr(ans, "obsType") <- as.character(obsType)
  attr(ans, "transType") <- as.character(transType)
  
  # Could assign class this way; but, I wish outside ops would preserve
  # the extra classes.  they don't. dplyr::group_by wipes out 
  # obsType and transType class components
  # class(ans) <- c(as.character(obsType), as.character(transType), clsAns)
  # class(ans) <- clsAns
  
  ans
}