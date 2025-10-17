#' @title Numeric Integration
#' 
#' @description
#' Numerically integrate under a distance function. 
#' 
#' @inheritParams effectiveDistance
#' 
#' @section Numeric Integration: 
#' Rdistance uses Simpson's composite 1/3 rule to numerically 
#' integrate distance functions from 0 to 
#' \code{object$w.hi - object$w.lo}. The number of points evaluated 
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
#' \emph{Details}: Let \code{n} = \code{options(Rdistance_intEvalPts)}.
#' Evaluate the distance function at \code{n} equal-spaced 
#' locations \{f(x0), f(x1), ..., f(xn)\} between 0 and (w.hi - w.lo). 
#' Simpson's composite approximation to the area under the curve is
#' \deqn{\frac{1}{3}h(f(x_0) + 4f(x_1) + 2f(x_2) + 
#'      4f(x_3) + 2f(x_4) + ... + 2f(x_{n-2}) + 
#'      4f(x_{n-1}) + f(x_{n}))}{(1/3)h(f(x0) + 4f(x1) + 2f(x2) + 
#'      4f(x3) + 2f(x4) + ... + 2f(x(n-2)) + 4f(x(n-1)) + f(xn))}
#' where \eqn{h} is the interval size (w.hi - w.lo) / n.
#' 
#' Physical units on the return values
#' are the original (linear) units if \code{object} contains line-transect data
#' (e.g., [m]), or square of the original units if \code{object} contains
#' point-transect data (e.g., [m^2]). Point-transect units are squared because
#' the likelihood consists of the detection function (which is unitless) 
#' multiplied by distances (which have units).  
#' 
#' @return A vector of areas under distance functions. 
#' If \code{newdata} is specified, return length is 
#' \code{nrow(newdata)}.  If \code{newdata} is NULL, 
#' return length is \code{length(distances(object))}. 
#' 
#' 
#' @examples
#' 
#' # Fake distance function object w/ minimum inputs for integration
#' d <- units::set_units(rep(1,4),"m") # Only units needed, not values
#' df <- data.frame(1) # Need attributes only
#' attr(df, "transType") <- "line"
#' obs <- factor(rep(c("obs1", "obs2"), 2))
#' beta <- c(3.5, -0.5)
#' w.hi <- 125
#' w.lo <- 20
#' ml <- list(
#'     mf = model.frame(d ~ obs)
#'   , data = df
#'   , par = beta 
#'   , likelihood = "halfnorm"
#'   , w.lo = units::set_units(w.lo, "m")
#'   , w.hi = units::set_units(w.hi, "m")
#'   , outputUnits = units(d)
#'   , expansions = 0
#'   , x.scl = units::set_units(w.lo, "m")
#'   , g.x.scl = 1
#' )
#' class(ml) <- "dfunc"
#' exact <- integrateHalfnorm(ml) # exact area
#' apprx <- integrateNumeric(ml)  # Numeric approx
#' pd <- options(digits = 20)
#' cbind(exact, apprx)
#' absDiff <- abs(apprx - exact) 
#' options(pd)
#' 
#' # Approximation to halfnorm is good to this number of digits
#' equalDigits <- round(log10(absDiff),1)  
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
  
  # we want units on x
  x <- attr(y, "distances") # these are 0 to w, not w.lo to w.hi, which is what we want
  
  # we want units on dx
  dx <- x[2] - x[1]  # or (w.hi - w.lo) / (nInts); could do diff(dx) if unequal intervals
  
  if(is.points(object)){
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