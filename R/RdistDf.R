#' @title RdistDf - Make an Rdistance nested data frame
#' 
#' @description Make an Rdistance data frame from 
#' separate transect and distance 
#' data frames. Rdistance data frames are nested 
#' and contain the following information:
#' \itemize{
#'   \item \bold{transects}: Groups of observations on the same transect, plus
#'       id, length, and potentially covariates. (rows)
#'   \item \bold{distances}: Observation distances and potentially covariates 
#'   are recorded in a list column that contains a data frame.
#'   \item \bold{distance types}: Either perpendicular (line-transects) 
#'       or radial (point-transects). (an attribute)
#'   \item \bold{observer type}: Either single observer or multiple observers.
#'   (an attribute)
#' }
#' Rdistance data frames can be constructed using a relatively straight forward
#' call to \code{dplyr::nest} and attribute 
#' assignment (see \bold{Examples}). This routine is simply a convenience wrapper 
#' for those calls.  
#' 
#' 
#' @param transectDf A data frame containing attributes of transects. 
#' At a minimum, this data frame must contain the transect's ID (so 
#' it can be merged with \code{detectionDf}) and the transect's length.  
#' All observations are made on a transect, but not all transects necessarily have 
#' observations.  Line-transects are continuous paths wherein targets can 
#' be sighted at any point.  Point transects
#' consist of one or more discrete points from which observers search for targets. 
#' The length of a line-transect is it's physical length in 2D space.
#' The 'length' of a point transect is 
#' the number of points along the transect. A single 
#' point is considered a transect of length one.  Transect level covariates,
#' if any, appear in this data frame.
#'  
#' @param detectionDf A data frame containing information about observations
#' made on each transect.  At a minimum, this data frame must contain 
#' the following:
#' \itemize{
#'   \item \bold{Detection Distances}: A single column containing 
#'   detection distances. This column will be specified on the left-hand 
#'   side of \code{formula} in a later call to \code{dfuncEstim}.  
#'   As of Rdistance version 3.0.0, detection distances must have 
#'   physical measurement units attached. See Section \bold{Measurment Units}. 
#'   
#'   \item \bold{Transect IDs}: The ID of the transect on which target groups
#'   were detected. Transect ID must be present so that the detection
#'   data frame can be merged with \code{transectDf}.    
#' }
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
#' @param .distanceCol Name of the list column in returned object 
#' containing distances. 
#' 
#' @param by A character vector of variables to join by.
#' If NULL, the default, \code{RdistDf}⁠ will perform a
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
#' Normally, the merge columns in \code{transectDf} 
#' specify unique rows.  See \bold{Details}.
#' 
#' @inheritSection dE.lt.single Measurement Units
#' 
#' @return A nested dataframe with one row per transect and observation information  
#' in a list column.  Technically, the return is 
#' a \code{tibble} from 
#' the \code{tibble} package with a list column containing 
#' distance information. Survey type and observer system are recorded 
#' as attributes (\code{transType} and \code{obsType}, respectfully). 
#' 
#' @details 
#' Users should ensure that rows of the output nested
#' data frame contains one sampling unit and that none are duplicated because
#' rows will eventually be re-sampled (in \code{abundEstim}) to estimate 
#' variance and confidence intervals. The combination of transect columns 
#' in \code{by} (i.e., the RHS of the merge, or "a" and "b" of 
#' 'c("a" = "d", "b" = "c")' for example) 
#' should normally specify \emph{unique} transects, which should normally 
#' be unique rows of \code{transectDf}. If duplicate transect row ID's are 
#' specified in \code{by}, \code{dplyr::left_join}, which is called internally,
#' may perform a many-to-many merge without warning, and this normally duplicates both 
#' transects and detections.
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
    dplyr::ungroup()
  
  # Merge transect and detection data frames -----
  clsAns <- class(ans)
  ans <- transectDf |> 
    dplyr::left_join(ans, by = by)
  
  
  attr(ans, "distColumn") <- .distanceCol
  attr(ans, "obsType") <- as.character(obsType)
  attr(ans, "transType") <- as.character(transType)
  
  # Could assign class this way; but, I wish outside ops would preserve
  # the extra classes.  they don't. dplyr::group_by wipes out 
  # obsType and transType class components
  # class(ans) <- c(as.character(obsType), as.character(transType), clsAns)
  class(ans) <- clsAns
  
  ans
}