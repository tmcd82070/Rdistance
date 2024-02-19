#' @title checkRdistDf - Check RdistDf data frames
#' 
#' @description
#' Checks the valicity of \code{Rdistance} nested data frames, 
#' which have class 'RdistDf'.  \code{Rdistance} data frames 
#' are a particular implementation of rowwise \code{tibbles} 
#' that contain detections in a list column, and extra attributes 
#' specifying types. 
#' 
#' @param df A data frame to check
#' 
#' @param verbose If true, an explanation of which check the data frame
#' fails is printed. Otherwise, no information on checks is provided.
#' 
#' @details The data frame is checked for the following:
#' \itemize{
#'   \item That the data frame inherits from the 'RdistDf' class. 
#'   \item That the data frame is a 'rowwise_df' \code{tibble} with one 
#'   row per group.  This ensures that each 
#'   row is uniquely identified and hence represents one transect. 
#'   \item \code{attr(df, "detectionColumn")} exists and points to a valid 
#'   list-based column in the data frame. 
#'   \item \code{attr(df, "obsType")} exists and is one of the valid values.
#'   \item \code{attr(df, "transType")} exists and is one of the valid values.
#' }
#' Other data checks, e.g., for measurement units, are performed 
#' later in \code{\link{dfuncEstim}}, after the model is specified. 
#' 
#' @return 0 invisibly. 0 means all checks passed. If a check fails, 
#' an error is thrown.  So, if this returns, all is good.
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
  
  # Check for 'RdistDf' class ----
  if(!inherits(df, "RdistDf")){
    if(verbose){
      cat(paste(
        crayon::red(dfName)
        , "does not inherit from class 'RdistDf'."
        , "Assign class using", 
        crayon::red(paste0("class("
                           , dfName
                           , ") <- c('RdistDf', class("
                           , dfName
                           , ")),"
                           ))
        , "or execute function Rdistance::RdistDf()."
        , "\n"
        ))
    }
    return(FALSE)
  }
  
  # Check for list-based distance column. ----
  # The && are critical here. Must stop evaluating hasDistCol if distColName 
  # is NULL or not in data. 
  distColName <- attr(df, "detectionColumn")[1]  # could be NULL
  hasDistCol <- !is.null(distColName) && 
    (distColName %in% names(df)) &&
    (is.list(df[,distColName][[1]]))
  if( !hasDistCol ){
    if(verbose){
      cat(paste(
        crayon::red(dfName)
        , "must have a list column containing detection information," 
        , "such as distance and group size. The list column must be named"
        , "in attribute 'detectionColumn'. Assign attribute with statement like"
        , crayon::red(paste0("attr("
                           , dfName
                           , ",'detectionColumn') <- <list column>"))
        , "See help('RdistDf')."
        , "\n"
      ))
    }
    return(FALSE)
  }
  
  # Check for presence and validity of transType ----
  transType <- attr(df, "transType")[1]
  hasTransType <- !is.null(transType) &&
    (transType %in% c("line", "point"))
  if( !hasTransType ){
    if(verbose){
      cat(paste(
        crayon::red(dfName)
        , "must have a declared transect type, either 'line' or"
        , "'point'. Transect type must be named in attribute 'transType'."
        , "Assign transect type attribute with statement like"
        , crayon::red(paste0("attr("
                             , dfName
                             , ",'transType') <- '<type>'"))
        , "See help('RdistDf')."
      ))
    }
    return(FALSE)
  }

  # Check for presence and validity of transType ----
  obsType <- attr(df, "obsType")[1]
  hasObsType <- !is.null(obsType) &&
    (obsType %in% c("single", "1given2", "2given1", "both"))
  if( !hasObsType ){
    if(verbose){
      cat(paste(
        crayon::red(dfName)
        , "must have a declared observation system, one of 'single',"
        , "'1given2', '2given1', or 'both'. Observation type must be named"
        , "in attribute 'obsType'."
        , "Assign observation system attribute with statement like"
        , crayon::red(paste0("attr("
                             , dfName
                             , ",'obsType') <- '<type>'"))
        , "See help('RdistDf')."
      ))
    }
    return(FALSE)
  }
  
  # Check for rowwise or grouped data frame ----
  if( !inherits(df, "rowwise_df") && !inherits(df, "grouped_df")){
    if(verbose){
      cat(paste(
        crayon::red(dfName)
        , "must be a 'rowwise_df' or 'grouped_df'."
        , "Use dplyr::nest() or run Rdistance::RdistDf()."
        , "See help('RdistDf')."
      ))
    }
    return(FALSE)
  }
  
  # Check that groups uniquely identify rows ----
  # If we are here, the data frame has groups.
  # Note: I am not sure it is possible to have a "rowwise_df" containing groups
  # that have >1 row.  I.e., "rowwise_df" may imply all groups have one row.
  # Anyway, we check group sizes here. 
  grps <- attr(df, "groups")
  lenGrps <- lengths(grps$.rows)
  if( any(lenGrps > 1) ){
    if(verbose){
      cat(paste(
        "Groups in" 
        , crayon::red(dfName)
        , "must identify unique rows, corresponding"
        , "to unique transects."
        , "Found"
        , sum(lenGrps > 1)
        , "groups containing more than one row, i.e., duplicate row IDs."
        , "Identify duplicate rows using"
        , crayon::red(paste0(dfName, " |> dplyr::summarise(n = dplyr::n())"
        , " |> dplyr::filter(n > 1)."))
        , "See help('RdistDf')."
      ))
    }
    return(FALSE)
  }
  
  # could pull and save the site ID's that identify rows. Columns that 
  # produce unique rows (i.e., the Site ID columns) are: 
  # names(grps)[ names(grps) != ".rows"]
  # But, I don't think we need the actual name of the columns provided 
  # groups of one row each are defined. 
  
  return(TRUE)
  
}