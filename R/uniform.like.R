#' @title uniform.like - Uniform distance likelihood
#' @aliases "uniform.like-deprecated"
#' 
#' @description Compute uniform-like distribution for 
#' distance functions.  This function was present in \code{Rdistance}
#' version < 2.2.0.  It has been replaced by the more appropriately named
#' \code{\link{logistic.like}}. 
#' 
#' @inheritParams logistic.like
#' 
#' @inheritSection logistic.like Expansion Terms
#' 
#' @inherit logistic.like return
#' 
#' @export
#' 
uniform.like <- function(a
                         , dist
                         , covars = NULL
                         , w.lo = 0
                         , w.hi = max(dist)
                         , series = "cosine"
                         , expansions = 0
                         , scale = TRUE
                         , pointSurvey = FALSE
                        ){
  
  .Deprecated(new = "logistic.like"
            , package = "Rdistance"
            , old = "uniform.like")
  
  L <- logistic.like(a = a
                     , dist = dist
                     , covars = covars
                     , w.lo = w.lo
                     , w.hi = w.hi
                     , series = series
                     , expansions = expansions
                     , scale = scale
                     , pointSurvey = pointSurvey
  )
    
  L
  
}  
  
  
  