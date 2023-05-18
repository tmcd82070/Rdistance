#' @title uniform.start.limits - Start and limit values for uniform distance function
#' 
#' @description DEPRECATED.: Starting values and limits for parameters of the uniform 
#' distance function. 
#' 
#' @inheritParams logistic.like
#' 
#' @return A list containing the following components:
#' \itemize{
#'    \item \code{start} : a vector of starting values
#'    \item \code{lowlimit} : a vector of lower limits (can be -Inf)
#'    \item \code{highlimit} : a vector of upper limits (can be Inf)
#'    \item \code{nms} : a vector containing names of the parameters
#' }
#' 
#' @details This function is usually called within 
#' \code{F.start.limits}. 
#' 
#' @export
#' 
uniform.start.limits <- function(dist
                                , covars
                                , expansions
                                , w.lo
                                , w.hi
                                ){

  x <- logistic.start.limits(dist = dist
                             , covars = covars
                             , expansions = expansions
                             , w.lo = w.lo
                             , w.hi = w.hi)
  x

}