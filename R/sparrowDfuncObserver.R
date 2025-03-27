#' @name sparrowDfuncObserver 
#' 
#' @title Brewer's Sparrow detection function
#' 
#' @description 
#' Pre-estimated Brewer's sparrow detection function that 
#' included and 'observer' effect.  Included to speed up 
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
#' # the following code was used to generate 'sparrowDfuncObserver'
#' data(sparrowDf)
#' sparrowDfuncObserver <- sparrowDf |> 
#'             dfuncEstim(formula = dist ~ observer
#'                      , likelihood = "hazrate")
#' }
#' 
#' 
NULL