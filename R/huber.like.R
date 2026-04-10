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
#' Huber loss function, mixed with a uniform at the upper end, and contains
#' three canonical parameters. 
#' The 'huber' distance function is a negative quadratic 
#' between \code{0} and its first parameter \eqn{\theta_1}{T1},
#' linear between \eqn{\theta_1}{T1} and \eqn{\theta_1 + \theta_2}{T1 + T2}, and 
#' constant  after \eqn{\theta_1 + \theta_2}{T1 + T2}.
#' Specifically, the 'huber' likelihood is, 
#' \deqn{
#'    f(d|\theta_1,\theta_2, p) = \left(1 - \frac{(1-p)h(d)}{m}\right) \,
#'      I(0 \leq d \leq \Theta) \  + \  p \, I(\Theta < d \leq w)
#' }
#' where
#' \eqn{\Theta = \theta_1 + \theta_2}{T = T1 + T2}, \eqn{w} = \code{w.hi - w.lo},
#' \eqn{
#'  m = \theta_1(\Theta - 0.5\theta_1)
#' }{
#'  m = T1(T - 0.5(T1))
#' }
#' and \emph{h(d)} is Huber loss between 0 and \eqn{\Theta}{T}, i.e.,
#' \deqn{
#'  h(d)  = 0.5d^2\, I(0 \leq d \leq \theta_1) \ + \ 
#'          \theta_1(d - 0.5\theta_1)\, I(\theta_1 < d \leq \Theta).
#' }
#' 
#' The first parameter, \eqn{\theta_1}{T1}, is related to covariates, 
#' i.e., \eqn{log(\theta_1) = \beta_0 + \beta_1x_1 + \ldots + \beta_qx_q}{
#' log(T_1) = B_0 + B_1(x_1) + ... + B_q(x_q)}.  \eqn{\theta_2}{T2} and \eqn{p} 
#' are constant across covariate values.
#' 
#' @seealso See \href{https://mcdonalddatasciences.com/Rdistance.html}{Rdistance tutorials}
#' for a method to generate random observations from the Huber likelihood.
#' 
#' @examples
#' 
#' t1 <- c(65,80,120)
#' t2 <- 60
#' p  <- 0.05
#' a <- matrix(c(log(t1)
#'        , rep(t2,3)
#'        , rep(p,3))
#'        , 3,3)
#' d <- seq(0, 200, length=201)
#' X <- matrix(1,length(d),1)
#' y <- huber.like(a, d, X)
#' 
#' # Plot showing covariate effects 
#' plot(range(d), range(y$L.unscaled)
#'   , type = "n", xlab = "d", ylab = "Huber(d)")
#' for(i in 1:3){
#'   # Distance functions
#'   lines(d
#'     , y$L.unscaled[,i]
#'     , col = i
#'     , lwd= 2)
#'   # Quadradic to linear transitions
#'   points(exp(a[i,1])
#'     , y$L.unscaled[(t1[i]-0.1) < d & d < (t1[i]+0.01),i]
#'     , pch = 16
#'     , col = i )
#'   # Linear to constant transition
#'   Theta <- exp(a[i,1]) + a[i,2]
#'   points(Theta
#'     , y$L.unscaled[(Theta-0.1) < d & d < (Theta+0.1),i]
#'     , pch = 15
#'     , col = i )
#' }
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
  
  theta2 <- p[1]
  p <- p[2]
  
  # theta1 is location of transition between 
  # squared trend and linear. 
  # theta2 is distance between theta1 and range, 

  if(p < 0 | p > 1 | theta2 < 0){
    h <- matrix(NA, length(dist), 1)
  } else {
    theta1 <- exp(s)  # link function here
    dist <- dropUnits(dist)
    range <- theta1 + theta2
    
    # Huber loss function
    h <- ifelse( dist <= theta1
                , 0.5 * dist^2
                , theta1*(dist - 0.5*theta1)
    )
    
    # Flip it over
    mx <- theta1*(range - 0.5*theta1)
    h <-  1 - (h / mx)*(1-p)
    h <- ifelse( dist > range
               , p
               , h)
  }

  # Integrate under the function and scale
  # integral0a <- beta^2*range - (2/3)*beta^3
  # integralar <-  0.5*beta*(range^2 + beta^2 - 2*range*beta)
  # areaUnder <- integral0a + integralar
  # 
  # h <- h / areaUnder
  
  return(
    list(L.unscaled = h, 
         params = data.frame(par1 = s
                           , par2 = theta2
                           , par3 = p
                           , row.names = NULL)
    )
  )
  
}
