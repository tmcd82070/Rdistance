#' @title errDataUnk - Unknown error message
#' 
#' @description
#' Constructs a string stating what is "unknown" that is 
#' suitable for use in warning and error functions. 
#' 
#' @param txt Text.  The "unknown" we are looking for. 
#' 
#' @param attri Attribute description we are looking for.
#'
#' @return A descriptive string, suitable for warning or error.
#' 
errDataUnk <- function(txt, attri){
  paste0( "Unknown "
          , txt
          , ". Set "
          , txt
          , " using 'RdistDf',"
          , " or with "
          , 'attr(data, "'
          , attri
          , '") <- "value".'
          , " See help(RdistDf) for list of values."
  )
} 
