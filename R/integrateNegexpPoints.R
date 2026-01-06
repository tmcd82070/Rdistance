#' @title Integrate Negative exponential point surveys
#' 
#' @description
#' Compute integral of the negative exponential distance function
#' for point surveys
#' 
#' @inheritParams integrateOneStepPoints
#' 
#' @inheritSection integrateOneStepPoints Note
#'  
#' @inherit integrateOneStepPoints return
#' 
#' @details 
#' 
#' Returned integrals are
#' \deqn{\int_0^{w} xe^{-a_i x} dx = \frac{1 - e^{-a_i w} (a_i w + 1)}{a_i^2},}{
#' Integral( x exp(-a*x) ) = (1 - exp(-a*w)(a*w + 1)) / a^2,}
#' where \eqn{w = w.hi - w.lo} and \eqn{a_i}{a} is the estimated 
#' negative exponential distance 
#' function parameter for the 
#' i-th observed distance. 
#' 
#' @seealso \code{\link{integrateNumeric}}; \code{\link{integrateNegexpLines}} 
#' 
#' @examples
#' 
#' # Fake distance function object w/ minimum inputs for integration
#' d <- rep(1,4) %#% "m" # Only units needed, not values
#' obs <- factor(rep(c("obs1", "obs2"), 2))
#' beta <- c(-5, -0.5)
#' w.hi <- 125
#' w.lo <- 20
#' ml <- list(
#'     mf = model.frame(d ~ obs)
#'   , par = beta 
#'   , likelihood = "negexp"
#'   , w.lo = w.lo %#% "m"
#'   , w.hi = w.hi %#% "m"
#'   , expansions = 0
#' )
#' class(ml) <- "dfunc"
#' integrateNegexpPoints(ml)
#' 
#' # Check: Integral of x*exp(-bx) from 0 to w.hi-w.lo
#' b <- c(exp(beta[1]), exp(beta[1] + beta[2]))
#' intgral <- (1 - exp(-b*(w.hi - w.lo)) * (b*(w.hi - w.lo) + 1)) / (b^2)
#' intgral
#' 
#' 
#' @export
#' 
integrateNegexpPoints <- function(object
                            , newdata = NULL
                            , w.lo = NULL
                            , w.hi = NULL
                            , Units = NULL
                              ){

  if( inherits(object, "dfunc") ){
    Units <- object$outputUnits
    w.lo <- object$w.lo
    w.hi <- object$w.hi
    object <- stats::predict(object = object
                             , newdata = newdata
                             , type = "parameters"
    )
  } 
  
  # Remove units b/c cannot exp units object.
  w <- dropUnits(w.hi - w.lo)
  aw <- object * w

  outArea <- (1 - exp(-aw) * (aw + 1)) / (object * object)
  
  outArea <- setUnits(outArea, Units)
  
  outArea 
  
}