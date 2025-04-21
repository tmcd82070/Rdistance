#' @title Numeric Integration
#' 
#' @description
#' Numerically integrate under a distance function. 
#' 
#' @inheritParams effectiveDistance
#' 
#' @section Numeric Integration: 
#' Rdistance uses Simpson's composite 1/3 rule to numerically 
#' integrate distance functions from \code{object$w.lo} to 
#' \code{object$w.hi}. The number of points evaluated 
#' during numerical integration is controlled by 
#' \code{options(Rdistance_intEvalPts)} (default 101).
#' Option 'Rdistance_intEvalPts' must be odd because Simpson's rule
#' requires an even number of intervals. 
#' Lower values of 'Rdistance_intEvalPts' increase calculation speeds; 
#' but, decrease accuracy.
#' 'Rdistance_intEvalPts' must be >= 5.  A warning is thrown if 
#' 'Rdistance_intEvalPts' < 29. Empirical tests by the author 
#' suggest 'Rdistance_intEvalPts' values >= 30 are accurate 
#' to several decimal points for smooth distance functions
#' (e.g., hazrate, halfnorm, negexp)
#' and that all 'Rdistance_intEvalPts' >= 101 produce 
#' identical results if the distance function is smooth. 
#'   
#' \emph{Details}: Let \code{n} = \code{options(Rdistance_intEvalPts)}
#' and evaluate the distance function at \code{n} equal-spaced 
#' locations \{f(x0), f(x1), ..., f(xn)\}. 
#' Simpson's composite approximation to the area under the curve is
#' \deqn{\frac{1}{3}h(f(x_0) + 4f(x_1) + 2f(x_2) + 
#'      4f(x_3) + 2f(x_4) + ... + 2f(x_{n-2}) + 
#'      4f(x_{n-1}) + f(x_{n}))}{(1/3)h(f(x0) + 4f(x1) + 2f(x2) + 
#'      4f(x3) + 2f(x4) + ... + 2f(x(n-2)) + 4f(x(n-1)) + f(xn))}
#' where \eqn{h} is the interval size (w.hi - w.lo) / n.
#' 
#' @return A vector of areas under distance functions. 
#' If \code{newdata} is specified, return length is 
#' \code{nrow(newdata)}.  If \code{newdata} is NULL, 
#' return length is \code{length(distances(object))}. 
#' 
#' @examples
#' 
#' # Fake a distance function object
#' d <- units::set_units(rep(1,4),"m") # Only units needed, not values
#' obs <- factor(rep(c("obs1", "obs2"), 2))
#' ml <- list(
#'     mf = model.frame(d ~ obs) 
#'   , likelihood = "halfnorm"
#'   , expansions = 0
#'   , w.lo = units::set_units(0, "m")
#'   , w.hi = units::set_units(125, "m")
#'   , outputUnits = units(units::set_units(1,"m"))
#'   , transType = "line"
#' )
#' class(ml) <- "dfunc"
#' integrateNumeric(ml)
#' 
#' # Check:
#' w.hi <- 125
#' w.lo <- 0
#' s1 <- 40
#' s2 <- exp(log(s1) + log(0.5))
#' obs1Scaler <- (pnorm(w.hi, mean=w.lo, sd = s1) - 0.5) * sqrt(2*pi)*s1
#' obs2Scaler <- (pnorm(w.hi, mean=w.lo, sd = s2) - 0.5) * sqrt(2*pi)*s2
#' c(obs1Scaler, obs2Scaler)
#' 
#' @export
#' 
integrateNumeric <- function(object
                           , newdata = NULL
                             ){
  
  nInts <- checkNEvalPts(getOption("Rdistance_intEvalPts")) - 1 # nInts MUST BE even!!!
  seqx = seq(object$w.lo, object$w.hi, length=nInts + 1)
  
  y <- stats::predict(object = object
                      , newdata = newdata
                      , distances = seqx
                      , type = "dfuncs"
  )
  
  x <- attr(y, "distances") # these are 0 to w, not w.lo to w.hi, which is what we want
  
  # we want units on dx
  dx <- x[2] - x[1]  # or (w.hi - w.lo) / (nInts); could do diff(dx) if unequal intervals
  
  if(is.points(object)){
    x <- units::set_units(x, NULL)
    x <- matrix(x, nrow(y), ncol(y))
    y <- x * y  # element-wise
  }
  
  # Numerical integration ----
  # Apply composite Simpson's 1/3 rule 
  #
  # Simpson's rule coefficients on f(x0), f(x1), ..., f(x(nEvalPts))
  # i.e., 1, 4, 2, 4, 2, ..., 2, 4, 1
  intCoefs <- c(rep( c(2,4), (nInts/2) ), 1) # here is where we need nInts to be even
  intCoefs[1] <- 1
  intCoefs <- matrix(intCoefs, ncol = 1)
  
  outArea <- (t(y) %*% intCoefs) * dx / 3
  outArea <- drop(outArea) # convert from matrix to vector
  
  if( is.points(object) ){
    outArea <- units::set_units(outArea, NULL)
    outArea <- sqrt( 2 * outArea )  # cannot sqrt units (unless like m^2 are assigned)
    outArea <- units::set_units(outArea, object$outputUnits, mode = "standard") # add back units
  }
  
  # Trapazoid rule: Computation used in Rdistance version < 0.2.2
  # y1 <- y[,-1,drop=FALSE]
  # y  <- y[,-ncol(y),drop=FALSE]
  # esw <- dx*rowSums(y + y1)/2
  
  # Trapezoid rule. (dx/2)*(f(x1) + 2f(x_2) + ... + 2f(x_n-1) + f(n)) 
  # Faster than above, maybe.
  # ends <- c(1,nrow(y))
  # esw <- (dx/2) * (colSums( y[ends, ,drop=FALSE] ) + 
  #                  2*colSums(y[-ends, ,drop=FALSE] ))
 
  outArea 
  
}