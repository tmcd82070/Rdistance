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
#' @details
#' The `huber` likelihood is an inverted version of the 
#' Huber loss function scaled and constrained to the interval [0,1]. 
#' The `huber` likelihood is, 
#' \deqn{
#' L(x|\beta,\gamma) = 1 - \frac{h/m}
#' }{
#' L(x|b,g) = 1 - h(x)/m
#' }
#' where
#' $$
#'  m = \beta^2 + \beta\gamma - 0.5\beta^2,
#' $$
#' and `h(x)` is Huber loss with a plateau,
#' $$
#'  h(x) = 0.5x^2 \text{if } x <  \beta,
#' $$
#' $$
#'  h(x) = \beta(x - 0.5\beta^2) \text{if } \beta < x < \beta + \gamma,
#' $$ and
#' $$
#'  h(x) = m \text{if } x > \beta + \gamma. 
#' $$ 
#' The `huber` distance function is quadratic between `w.lo` and $\beta$, 
#' linear between $\beta$ and $\beta + \gamma$, and 0 after $\beta + \gamma$.
#' 
#' @examples
#' 
#' d <- seq(0, 100, length=101)
#' covs <- cbind(
#'    matrix(1,2*length(d),1)
#'  , matrix(c(rep(0,length(d)), rep(1,length(d))), 2*length(d), 1)
#'  )
#' y <- huber.like(c(log(40), -log(2), 40), d, covs)$L.unscaled
#' plot(d,y[1:101],type="l")
#' lines(d,y[102:202], col = "blue" )
#' abline(v = exp(log(40)), lty = 2)  # transition to linear, group 1
#' abline(v = exp(log(40) - log(2)), lty = 2) # transition to linear, group 2
#' 
#' # transitions to zero
#' exp(log(40)) + 40
#' exp(log(40) - log(2)) + 40
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
  
  # Huber loss function
  h <- ifelse( d <= beta
              , 0.5 * d^2
              , beta*(d - 0.5*beta)
  )
  
  # Flip it over
  mx <- beta*(range - 0.5*beta)
  h <- ifelse( d > range
             , mx
             , h)
  h <-  1 - (h / mx)


  # Integrate under the function and scale
  # integral0a <- beta^2*range - (2/3)*beta^3
  # integralar <-  0.5*beta*(range^2 + beta^2 - 2*range*beta)
  # areaUnder <- integral0a + integralar
  # 
  # h <- h / areaUnder
  
  return(
  list(L.unscaled = h, 
       params = data.frame(par1 = beta
                         , par2 = gam)
  )
  )
  
}
