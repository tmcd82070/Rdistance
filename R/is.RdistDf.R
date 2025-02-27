#' @title checkRdistDf - Check RdistDf data frames
#' 
#' @description
#' Checks the validity of \code{Rdistance} nested data frames. 
#' \code{Rdistance} data frames 
#' are a particular implementation of rowwise \code{tibbles} 
#' that contain detections in a list column, and extra attributes 
#' specifying types. 
#' 
#' @param df A data frame to check
#' 
#' @param verbose If TRUE, an explanation of the check that fails is printed. 
#' Otherwise, no information on checks is provided.
#' 
#' @details The following checks are performed (in this order):
#' \itemize{
#'   \item \code{attr(df, "detectionColumn")} exists and points to a valid 
#'   list-based column in the data frame. 
#'   \item \code{attr(df, "obsType")} exists and is one of the valid values.
#'   \item \code{attr(df, "transType")} exists and is one of the valid values.
#'   \item The data frame is either a 'rowwise_df' or 'grouped_df' 
#'   \code{tibble}.
#'   \item The data frame has only one row per group. One row per group 
#'   is implied by 'rowwise_df', but not a 'grouped_df', and both are allowed
#'   in \code{Rdistance}. One row per group ensures rows are uniquely identified 
#'   and hence represents one transect. 
#'   \item No column names in the list-column are duplicated in the non-list 
#'   columns of the data frame. This check ensures that \code{tidyr::unnest}
#'   executes. 
#' }
#' Other data checks, e.g., for measurement units, are performed 
#' later in \code{\link{dfuncEstim}}, after the model is specified. 
#' 
#' @return TRUE or FALSE invisibly. TRUE means all checks passed. FALSE implies 
#' at least one check failed. Use \code{verbose} = TRUE to see which. 
#' 
#' @examples
#' 
#' sparrowDf <- RdistDf( sparrowSiteData, sparrowDetectionData )
#' is.RdistDf(sparrowDf)
#' 
#' # Data frame okay, but no attributes
#' sparrowDf <- sparrowDetectionData |> 
#'   dplyr::nest_by( siteID
#'                , .key = "distances") |> 
#'   dplyr::right_join(sparrowSiteData, by = "siteID")
#' is.RdistDf(sparrowDf, verbose = TRUE)
#' 
#' 
#' @export
#' 
is.RdistDf <- function(df, verbose = FALSE){
  
  dfName <- deparse(substitute(df))
  
  # Check for 'RdistDf' class 
  # if(!inherits(df, "RdistDf")){
  #   if(verbose){
  #     cat(paste(
  #       crayon::red(dfName)
  #       , "does not inherit from class 'RdistDf'."
  #       , "Assign class using", 
  #       crayon::red(paste0("class("
  #                          , dfName
  #                          , ") <- c('RdistDf', class("
  #                          , dfName
  #                          , ")),"
  #                          ))
  #       , "or execute function Rdistance::RdistDf()."
  #       , "\n"
  #       ))
  #   }
  #   return(FALSE)
  # }
  
  # Check for list column name attribute ----
  distColName <- attr(df, "detectionColumn")[1]  # could be NULL
  hasListColName <- !is.null(distColName)
  if( !hasListColName ){
    if(verbose){
      cat(paste(
        crayon::red(dfName)
        , "must have a"
        , crayon::red("'detectionColumn'")
        , "attribute naming a list-based column that contains detection information."
        , "Assign attributes with statements like"
        , crayon::red(paste0("attr("
                             , dfName
                             , ",'detectionColumn') <- <list column>"))
        , "See help('RdistDf')."
        , "\n"
      ))
    }
    return(FALSE)
  } 
  
  # Check for list-based distance column. ----
  # The && are critical here. Must stop evaluating if distColName 
  # is not in the data frame. 
  hasDistCol <-  
    (distColName %in% names(df)) &&
    (is.list(df[,distColName][[1]]))
  if( !hasDistCol ){
    if(verbose){
      cat(paste0(
        crayon::red(dfName)
        , " must have a list column containing detection information." 
        , " Expected to find a list-based column named "
        , crayon::red(distColName)
        , ". "
        , "See help('RdistDf')."
        , "\n"
      ))
    }
    return(FALSE)
  }
  
  # Check for presence and validity of transType ----
  transType <- Rdistance::transectType(df)
  hasTransType <- !is.null(transType) &&
    (transType %in% c("line", "point"))
  if( !hasTransType ){
    if(verbose){
      cat(paste(
        crayon::red(dfName)
        , "must have a declared transect type, either 'line' or 'point'."
        # , "Transect type must be named in attribute 'transType'."
        , "Assign transect type attribute with statement like"
        , crayon::red(paste0("attr("
                             , dfName
                             , ",'transType') <- '<type>'"))
        , "See help('RdistDf').\n"
      ))
    }
    return(FALSE)
  }

  # Check for presence and validity of obsType ----
  obsType <- Rdistance::observationType(df)
  hasObsType <- !is.null(obsType) &&
    (obsType %in% c("single", "1given2", "2given1", "both"))
  if( !hasObsType ){
    if(verbose){
      cat(paste(
        crayon::red(dfName)
        , "must have a declared observation system, one of 'single', '1given2', '2given1', or 'both'."
        # , " Observation type must be named in attribute 'obsType'."
        , "Assign observation system attribute with statement like"
        , crayon::red(paste0("attr("
                             , dfName
                             , ",'obsType') <- '<type>'"))
        , "See help('RdistDf').\n"
      ))
    }
    return(FALSE)
  }

  # Check for presence and validity of Effort column ----
  effCol <- attr(df, "effortColumn")
  hasEffCol <- !is.null(effCol) &&
    (effCol %in% names(df))
  if( !hasEffCol ){
    if(verbose){
      cat(paste(
        crayon::red(dfName)
        , "must have a valid effort column."
        , "Assign effort with a statement like"
        , crayon::red(paste0("attr("
                             , dfName
                             , ",'effortColumn') <- '<column name>'"))
        , "See help('RdistDf').\n"
      ))
    }
    return(FALSE)
  }
  
  # Check line length has units, nPoints does not ----
  effVec <- df |> 
    dplyr::pull(dplyr::all_of(effCol))
  if( transType == "line" ){
    if( !inherits(effVec, "units") ){
      if(verbose){
        cat(paste(
          "Transect type is 'line' but effort column"
          , crayon::red(effCol)
          , "does not have units. "
          , "Set units with a statement like"
          , crayon::red(paste0(
            dfName
            , "$"
            , effCol
            , " <- units::set_units( "
            , dfName
            , "$"
            , effCol
            , ", 'm')\n"
          ))
        ))
      }
      return(FALSE)
    }
  } else {
    # Transect type is 'point'
    if( inherits(effVec, "units") || 
        !is.numeric(effVec)){
      if(verbose){
        cat(paste(
          "Transect type is 'point', but effort column"
          , crayon::red(effCol)
          , "either has units or is not numeric. "
          , "Set effort column to number of points on transects.\n"
          ))
      }
      return(FALSE)
    }
  }
  
  
  # Check for rowwise or grouped data frame ----
  if( !inherits(df, "rowwise_df") && !inherits(df, "grouped_df")){
    if(verbose){
      cat(paste(
        crayon::red(dfName)
        , "must be a 'rowwise_df' or 'grouped_df'."
        , "Use dplyr::nest() or run Rdistance::RdistDf()."
        , "See help('RdistDf').\n"
      ))
    }
    return(FALSE)
  }
  
  # Check that groups uniquely identify rows ----
  # If we are here, the data frame has groups.
  # Note: I am not sure it is possible to have a "rowwise_df" containing groups
  # that have >1 row.  I.e., "rowwise_df" may imply all groups have one row.
  # I don't think so.  Anyway, we check group sizes here. 
  grps <- dplyr::group_size(df)
  if( any(grps > 1) ){
    if(verbose){
      cat(paste(
        "Groups in" 
        , crayon::red(dfName)
        , "must identify unique rows, corresponding"
        , "to unique transects."
        , "Found"
        , sum(grps > 1)
        , "groups containing more than one row, i.e., duplicate row IDs."
        , "Identify duplicate rows using"
        , crayon::red(paste0(dfName, " |> dplyr::summarise(n = dplyr::n())"
        , " |> dplyr::filter(n > 1)."))
        , "See help('RdistDf').\n"
      ))
    }
    return(FALSE)
  }
  
  # Check that tidy::unnest will work ----
  # no duplicate names in list vs out list
  firstList <- df |> 
    dplyr::ungroup() |>  
    dplyr::pull(dplyr::all_of(distColName))
  firstList <- do.call(dplyr::bind_rows, firstList)
  namesInList <- names(firstList)
  namesOutList <- setdiff(names(df), distColName)
  if( any( namesInList %in% namesOutList) ){
    if(verbose){
      cat(paste0(
        "Duplicate names found inside ",
        crayon::red(distColName), 
        " column and in the remainder of " 
        , crayon::red(dfName)
        , ". Make names in detection data frame unique."
        , " Test: "
        , crayon::red(paste0("tidyr::unnest(", dfName, ")"))
        , " should execute."))
      cat(" Duplicate names found: ")
      cat(crayon::red(intersect(namesOutList, namesInList)))
      cat("\n")
    }
    return(FALSE)
  }
  
  # could pull and save the site ID's that identify rows. Columns that 
  # produce unique rows (i.e., the Site ID columns) are: 
  # grps <- dplyr::group_data(df)
  # names(grps)[ names(grps) != ".rows"]
  # But, I don't think we need the actual name of the columns provided 
  # groups of one row each are defined. 
  
  return(TRUE)
  
}