#' @title Integrate Negative exponential 
#' 
#' @description
#' Compute integral of the negative exponential distance function. 
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
#' \deqn{\int_0^{w} e^{-a_i x} dx = \frac{1}{a_i} (1 - e^{-a_i w}),}{
#' Integral( exp(-a*x) ) = (1 - exp(-a*w)) / a,}
#' where \eqn{w = w.hi - w.lo} and \eqn{a_i}{a} is the estimated 
#' negative exponential distance 
#' function parameter for the 
#' i-th observed distance. 
#'  
#' @seealso \code{\link{integrateNumeric}}; \code{\link{integrateNegexpPoints}}; 
#' \code{\link{integrateOneStepLines}} 
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
#' integrateNegexpLines(ml)
#' 
#' # Check: Integral of exp(-bx) from 0 to w.hi-w.lo
#' b <- c(exp(beta[1]), exp(beta[1] + beta[2]))
#' intgral <- (1 - exp(-b*(w.hi - w.lo))) / b
#' intgral
#' 
#' 
#' @export
#' 
integrateNegexpLines <- function(object
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

  outArea <- (1 - exp(-object*w)) / object
  
  outArea <- setUnits(outArea, Units)
  
  outArea 
  
}