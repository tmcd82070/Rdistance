#' @title Brewer's Sparrow detection function
#' 
#' @name sparrowDfuncObserver 
#' 
#' @description 
#' Pre-estimated Brewer's sparrow detection function that 
#' includes an 'observer' effect.  Included to speed up 
#' example execution times. See 'Examples'.
#' 
#' @docType data
#' 
#' @format An estimated distance function object with 
#' class 'dfunc'. See 'Value' section of 
#' \code{\link{dfuncEstim}} for 
#' description of components. 
#' 
#' @seealso \code{\link{sparrowSiteData}} and 
#' \code{\link{sparrowDetectionData}} for description of the data
#' 
#' 
#' @examples
#' \dontrun{
#' # the following code generated 'sparrowDfuncObserver'
#' data(sparrowDf)
#' sparrowDfuncObserver <- sparrowDf |> 
#'             dfuncEstim(formula = dist ~ observer
#'                      , likelihood = "hazrate")
#' }
#' 
#' 
NULL