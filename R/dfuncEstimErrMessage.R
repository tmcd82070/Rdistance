#' @title dfuncEstimErrMessage - dfuncEstim error messages
#' 
#' @description
#' Utility function to produce error messages suitable for \code{stop}
#' 
#' @param txt A text string describing the error. 
#' 
#' @param attri An attribute to report.
#' 
#' @return A string
#' 

# Non-exported
dfuncEstimErrMessage <- function(txt, attri){
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
