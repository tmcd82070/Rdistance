#' @title makeDataframe - Make Rdistance nested data frame
#' 
#' @description Make an Rdistance data frame from transect and distance 
#' information.  For example, this will combine 'site' information, such 
#' as transect length and location, with 'observation' information, such
#' as distance and type. 
#' 
#' @param siteDf A data frame containing site information
#' 
#' @param detectionDf A data frame containing information about the observeration, 
#' such as distance to each and covariates. 
#' 
#' @inheritParams dplyr::right_join
#' 
#' @return A nested dataframe with one row per site and observations from 
#' each site in a list column.  Technically, the return is a 'rowwise_df' from 
#' the \code{dplyr} package and has class \code{c("rowwise_df", "tbl_df", "tbl", "data.frame")}.
#' 
#' @examples
#' 
#' sparrowDf <- makeRdistanceDataframe( sparrowSiteData, sparrowDetectionData )
#' 
#' @export
#' 
makeRdistanceDataframe <- function(siteDf
                                 , detectionDf
                                 , by = NULL
                                 , .distanceCol = "distances"
                                 ){
  
  nm <- names(siteDf)
  tmp <- siteDf |> dplyr::right_join(detectionDf, by = by)
  tmp3 <- tmp |> dplyr::nest_by( dplyr::across(dplyr::all_of(nm)), .key = .distanceCol)
  attr(tmp3, "distColumn") <- .distanceCol
  
  tmp3
}