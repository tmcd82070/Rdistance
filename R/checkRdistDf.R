#' @title checkRdistDf - Check validity of data frame
#' 
#' @description
#' Checks that the input data frame is a proper \code{Rdistance}
#' data frame. 
#' 
#' @param df A data frame to check
#' 
#' @details The data frame is checked for the following:
#' \itemize{
#'   \item That the data frame is a 'rowwise_df' with one 
#'   row per group.  This ensures that each 
#'   row is uniquely identified and hence represents one transect. 
#'   \item \code{attr(df, "distColumn")} exists and points to a valid 
#'   list-based column in the data frame. 
#'   \item \code{attr(df, "obsType")} exists and is one of the valid values.
#'   \item \code{attr(df, "transType")} exists and is one of the valid values.
#' }
#' Checks for measurement units and other things are carried out 
#' later, after the model is specified. 
#' 
#' @return 0 invisibly. 0 means all checks passed. If a check fails, 
#' an error is thrown.  So, if this returns, all is good.
#' 
#' @examples
#' 
#' sparrowDf <- RdistDf( sparrowSiteData, sparrowDetectionData )
#' checkRdistDf(sparrowDf)
#' 
#' # Data frame okay, no attributes
#' sparrowDf <- sparrowDetectionData |> 
#'   dplyr::nest_by( siteID
#'                , .key = "distances") |> 
#'   dplyr::right_join(sparrowSiteData, by = "siteID")
#' checkRdistDf(sparrowDf)
#' 
#' 
#' @export
#' 
checkRdistDf <- function(df){
  
  
  # Check for list-based distance column. ----
  # The && are critical here. Must stop evaluating hasDistCol if distColName 
  # is NULL or not in data. 
  distColName <- attr(df, "distColumn")[1]  # could be NULL
  hasDistCol <- !is.null(distColName) && 
    (distColName %in% names(df)) &&
    (is.list(df[,distColName][[1]]))
  if( !hasDistCol ){
    stop(paste(
      "Input data must have a list column containing detection information," 
      , "such as distance and group size. The list column must be named"
      , "in attribute 'distColumn'."
      , "See help('RdistDf') for the Rdistance data frame constructor."
    ))
  }
  
  # Check for presence and validity of transType ----
  transType <- attr(df, "transType")[1]
  hasTransType <- !is.null(transType) &&
    (transType %in% c("line", "point"))
  if( !hasTransType ){
    stop(paste(
      "Input data must have a declared type of transect, either 'line' or"
      , "point. Transect type must be named in attribute 'transType'."
      , "See help('RdistDf') for the Rdistance data frame constructor."
    ))
  }

  # Check for presence and validity of transType ----
  obsType <- attr(df, "obsType")[1]
  hasObsType <- !is.null(obsType) &&
    (obsType %in% c("single", "1given2", "2given1", "both"))
  if( !hasObsType ){
    stop(paste(
      "Input data must have a declared observation system, one of 'single',"
      , "'1given2', '2given1', or 'both'. Observation type must be named"
      , "in attribute 'obsType'."
      , "See help('RdistDf') for the Rdistance data frame constructor."
    ))
  }
  
  # Check for rowwise or grouped data frame ----
  if( !inherits(df, "rowwise_df") && !inherits(df, "grouped_df")){
    stop(paste(
      "Input data frame must be a 'rowwise_df' or 'grouped_df'."
      , "See help('RdistDf') for the Rdistance data frame constructor."
    ))
  }
  
  # Check that groups uniquely identify rows ----
  # If we are here, the data frame has groups.
  # Note: I am not sure it is possible to have a "rowwise_df" containing groups
  # that have >1 row.  I.e., "rowwise_df" may imply all groups have one row.
  # Anyway, we check group sizes here. 
  grps <- attr(df, "groups")
  lenGrps <- lengths(grps$.rows)
  if( any(lenGrps > 1) ){
    stop(paste(
      "Groups of input data frame must identify unique rows, corresponding"
      , "to unique transects."
      , "Found"
      , sum(lenGrps > 1)
      , "groups containing more than one row, i.e., duplicate row IDs."
      , "Identify duplicate rows using df |> dplyr::summarise(n = dplyr::n())"
      , "|> dplyr::filter(n > 1)."
      , "See help('RdistDf') for the Rdistance data frame constructor."
    ))
  }
  
  # could pull and save the site ID's that identify rows. Columns that 
  # produce unique rows (i.e., the Site ID columns) are: 
  # names(grps)[ names(grps) != ".rows"]
  # But, I don't think we need the actual name of the columns provided 
  # groups of one row each are defined. 
  
  invisible(0)
}