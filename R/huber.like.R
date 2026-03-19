#' @title Huber distance function
#' 
#' @description 
#' Computes the Huber likelihood for use as a distance function. 
#' The Huber likelihood is a mixture of an inverted Huber loss 
#' and a uniform distribution.  
#' The distance function is quadratic from the lowest distance to 
#' its first parameter, linear from the first parameter
#' to the sum of its first and second parameter, and constant 
#' after that. 
#' 
#' @inheritParams halfnorm.like
#' 
#' @inherit halfnorm.like return seealso
#' 
#' @details
#' The 'huber' likelihood is an inverted version of the 
#' Huber loss function mixed with a uniform distribution. 
#' The 'huber' likelihood is, 
#' \deqn{
#'    f(x|\theta_1,\theta_2, p) = \left(1 - \frac{h(x)}{m}\right) I(0 < x \leq \theta_.)  + p*I(\theta_.< x \leq w.hi)
#' }
#' where
#' \eqn{\theta_. = \theta_1 + \theta_2}{T = T1 + T2}
#' 
#' \deqn{
#'  m = \theta_1(0.5\theta_1 + \theta_2)
#' }
#' and \emph{h(x)} is Huber loss with a plateau, i.e.,
#' \deqn{
#'  h(x) = 0.5x^2 ,
#' }
#' for \eqn{x < \theta_1}
#' \deqn{
#'  h(x) = \theta_1(x - 0.5\theta_1^2),
#' }
#' for \eqn{\theta_1 < x < (\theta_1 + \theta_2)}.
#' The 'huber' distance function is quadratic between \code{0} and \eqn{\theta_1}{T1}, 
#' linear between \eqn{\theta_2}{T2} and \eqn{\theta_1 + \theta_2}{T1 + T2}, and 
#' constant  after \eqn{\theta_1 + \theta_2}{T1 + T2}.
#' 
#' The first parameter, \eqn{\theta_1}{T1}, is related to covariates if present. 
#' i.e., \eqn{log(\theta_1) = \beta_0 + \beta_1x_1 + \ldots + \beta_qx_q}{
#' log(T_1) = B_0 + B_1(x_1) + ... + B_q(x_q)}.
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
huber.like <- function(a
                       , dist
                       , covars 
                       , w.hi = NULL){
  
  # What's in a? : 
  #     a = [(Intercept), b1, ..., bp, range, <expansion coef>]
  
  q <- Rdistance:::nCovars(covars)
  
  if(is.matrix(a)){
    beta <- a[,1:q, drop = FALSE]  # k X q
    p <- a[1, (q+1):(q+2), drop = TRUE]     # 1 X 2
  } else {
    beta <- matrix(a[1:q], nrow = 1) # 1 X q
    p <- a[(q+1):(q+2), drop = TRUE]     # 1 X 2
  }
  s <- covars %*% t(beta) # (nXq) %*% (qXk) = nXk
  theta1 <- exp(s)  # link function here
  
  dist <- dropUnits(dist)
  
  theta2 <- p[1]
  range <- theta1 + theta2
  
  # theta1 is location of transition between 
  # squared trend and linear. 
  # theta2 is distance between theta1 and range, 

  # Huber loss function
  h <- ifelse( dist <= theta1
              , 0.5 * dist^2
              , theta1*(dist - 0.5*theta1)
  )
  
  # Flip it over
  mx <- theta1*(range - 0.5*theta1)
  h <-  1 - (h / mx)*(1-p[2])
  h <- ifelse( dist > range
             , p[2]
             , h)

  # Integrate under the function and scale
  # integral0a <- beta^2*range - (2/3)*beta^3
  # integralar <-  0.5*beta*(range^2 + beta^2 - 2*range*beta)
  # areaUnder <- integral0a + integralar
  # 
  # h <- h / areaUnder
  
  return(
    list(L.unscaled = h, 
         params = data.frame(par1 = theta1
                           , par2 = p[1]
                           , par3 = p[2])
    )
  )
  
}
