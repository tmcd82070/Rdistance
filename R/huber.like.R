#' @title huber.like - Huber distance function
#' 
#' @description 
#' Computes the Huber likelihood for use as a distance function. 
#' The Huber likelihood is based on Huber loss.  
#' It is quadratic from the lowest distance value to 
#' its first parameter, then linear from the first parameter
#' to its second. The function is zero after the second parameter. 
#' Values are scaled such that the integral from lowest distance 
#' value to the second parameter (\code{range}) is 1.0.
#' 
#' @param a Distance at which the likelihood 
#' transitions from quadratic to linear.  First derivative
#' is constant just before and after this point. 
#' 
#' @param range Distance beyond which the likelihood is zero.
#' 
#' @return Vector of huber likelihood values.
#' 
#' @examples
#' 
#' d <- seq(0, 100, length=101)
#' covs <- matrix(1,length(d),1)
#' y <- huber.like(c(log(40), 40), d, covs)
#' plot(d,y,type="l")
#' points(d[41],y[41])
#' points(d[81], 0)
#' 
#' 
#' @export
#' 

huber.like <- function(a, dist, covars){
  
  # Restrictions : 0 < a <= range <= w.hi
  # dist must be >= 0, or this does not work right
  
  # What's in a? : 
  #     a = [(Intercept), b1, ..., bp, range, <expansion coef>]
  
  q <- Rdistance:::nCovars(covars)
  
  beta <- matrix(a[1:q], ncol = 1) 
  s <- drop( covars %*% beta )      
  beta <- exp(s)  # link function here
  gam <- a[q+1]
  range <- beta + gam
  
  # beta is location of transition between 
  # squared trend and linear. 
  # gam is distance between beta and range, 
  # i.e., range = loc w/ like of 0 = beta + gam
  
  d <- units::set_units(dist, NULL)
  h <- ifelse( d <= beta
              , 0.5 * d^2
              , beta*(d - 0.5*beta)
  )
  
  h <- beta*(range - 0.5*beta) - h
  h <- ifelse( h <= 0.0
              , 0.0
              , h
              )
  
  # Integrate under the function and scale
  integral0a <- beta^2*range - (2/3)*beta^3
  integralar <-  0.5*beta*(range^2 + beta^2 - 2*range*beta)
  areaUnder <- integral0a + integralar
  
  h <- h / areaUnder
  
  return(
  list(L.unscaled = h, 
       params = data.frame(par1 = beta
                         , par2 = gam)
  )
  )
  
}
