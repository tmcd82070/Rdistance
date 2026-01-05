#' @title Gamma distance function 
#' 
#' @description Evaluate the gamma distance function 
#' for sighting distances, potentially including covariates
#' and expansion terms
#' 
#' @inheritParams halfnorm.like 
#' 
#' @details 
#' 
#' The Rdistance implementation of a Gamma distance function follows 
#' Becker and Quang (2009). Rdistance's Gamma distance function is 
#' \deqn{f(d|\alpha, \sigma) = \left(\frac{d}{m}\right)^{\alpha - 1}e^{-(d-m)/\sigma},}{
#' f(d|a,s) =  (d/m)^{a-1} exp(-(d-m)/s),
#' }
#' where \eqn{\alpha}{a} is the \bold{shape} parameter, \eqn{\sigma}{s} is 
#' the \bold{scale} parameter, and \eqn{m = (\alpha-1)\sigma}{m = (a-1)s}.
#' \eqn{m} is the mode of the Gamma function, and in Rdistance it's 
#' scaled to have a maximum of 1.0 at \eqn{m}.  
#' The \bold{scale} parameter is a function of the shape parameter 
#' \emph{and} sighting covariates, i.e., 
#' \deqn{\sigma = k [exp(x'\beta)],}{s = k*exp(x'b),}
#' where \eqn{x} is a vector of covariate values associated with distance \eqn{d} 
#' (i.e., a row of \code{covars}), \eqn{\beta}{b} is a vector of the 
#' first \eqn{q} (=\code{ncol(covars)}) values of the first argument 
#' of the function (\code{a}), and 
#' \eqn{k} is a function of the shape parameter, i.e., 
#' \deqn{k = \frac{1}{\Gamma(\alpha)}  \left(\frac{a - 1}{e^1} \right)^{a - 1}.}{
#' k = (1/Gamma(a)) * (((a - 1)/exp(1))^(a - 1)).}  
#' The shape parameter \eqn{\alpha}{a} is the 
#' \eqn{q+1}-st value in the function's first argument and is constrained to 
#' be strictly greater than 1.0.
#'  
#' See Examples for use of \code{\link{GammaReparam}} to compute \eqn{\alpha}{a}
#' and \eqn{\sigma}{s} from fitted object coefficients.
#'   
#' @inherit halfnorm.like return seealso
#'    
#' @references Becker, E. F., and P. X. Quang, 2009. \emph{A Gamma-Shaped Detection Function for Line-Transect Surveys with Mark-Recapture and Covariate Data.}
#'   Journal of Agricultural, Biological, and Environmental Statistics 14(2):207-223.
#'   
#' @examples 
#' x <- seq(0, 100, length=100)
#' covars <- matrix(1,100,1)
#' 
#' # Plots showing changes in scale
#' plot(x, Gamma.like(c(log(20),2.5), x, covars)$L.unscaled, type="l", col="red")
#' lines(x, Gamma.like(c(log(40),2.5), x, covars)$L.unscaled, col="blue")
#' 
#' # Plots showing changes in shape
#' plot(x, Gamma.like(c(log(20),1.5), x, covars)$L.unscaled, type="l", col="red")
#' lines(x, Gamma.like(c(log(20),2.5), x, covars)$L.unscaled, col="blue")
#' lines(x, Gamma.like(c(log(20),4.5), x, covars)$L.unscaled, col="green")
#' 
#' # Roll-your-own plot, showing "re-parameterization":
#' # Assume fitted object coefficients are c(log(20), 4.5)
#' fit <- list(par = c(log(20), 4.5))
#' 
#' # The distance function is then,
#' gammaPar <- GammaReparam( scl = exp(fit$par[1])
#'                         , shp = fit$par[2] ) # returns scl=k*exp(x'B)
#' scl <- gammaPar$scl
#' shp <- gammaPar$shp
#' m <- (shp - 1) * scl
#' g <- (x / m)^(shp - 1) * exp(-(x - m) / scl) # distance function
#' lines(x, g, lwd = 3, lty = 2, col="green3")
#' 
#' @export
Gamma.like <- function(a
                       , dist
                       , covars
                       , w.hi = NULL
                       ){

  # What's in a? : 
  #   If no covariates: a = [Shape, Scale, <expansion coef>]
  #   If covariates:    a = [(Intercept), b1, ..., bp, Shape, <expansion coef>]
  
  # w.hi is ignored, but needed for compatability in other likelihoods
  # cat(paste("In", crayon::red("gamma.like"), "\n"))
  
  if(length(dim(dist)) >= 2 && dim(dist)[2] != 1 ){ 
    stop(paste("Argument 'dist' must be a vector or single-column matrix.",
               "Found array with", length(dim(dist)), "dimensions."))
  }
  
  q <- nCovars(covars)
  if(is.matrix(a)){
    beta <- a[,1:q, drop = FALSE]  # k X q
    shp <- a[1, q+1, drop = TRUE]     # 1 X 1
  } else {
    beta <- matrix(a[1:q], nrow = 1) # 1 X q
    shp <- a[q+1]     # 1 X 1
  }
  s <- covars %*% t(beta) # (nXq) %*% (qXk) = nXk
  scl <- exp(s)  # link function here

  # An oddity:  dgamma preserved units if scl is 1D vector; but, 
  #  strips units if scl has >= 2 cols. 
  
  dgamPars <- GammaReparam(shp, scl)

  m <- (dgamPars$shp - 1)*dgamPars$scl  # mode of Gamma
  d <- dropUnits(dist)
  
  key <- (d/m)^(dgamPars$shp - 1)*exp(-(d-m)/dgamPars$scl)

  # key <- stats::dgamma( dist, shape=dgamPars$shp, scale=dgamPars$scl )
  # 
  # # stats::dgamma returns a matrix if dgamPars$scl is a matrix with ncol(key) > 1
  # # stats::dgamma returns vector if $scl is vector or matrix with ncol(key) == 1
  # # fix this to return matrix always
  # if( ncol(scl) == 1 ){
  #   key <- matrix(key, ncol = 1)  # drops units too
  # }
  # 
  # # Scale like to have max 1
  # m <- (dgamPars$shp - 1)*dgamPars$scl
  # keyAtM <- stats::dgamma( m, shape=dgamPars$shp, scale=dgamPars$scl )
  # 
  # key <- key / keyAtM
  
  # Note: Mode of Gamma distribution is (dgamPars$shp - 1)*dgamPars$scl,
  # or scl * b * (shp - 1) where b = (1/gamma(shp)) * (((shp - 1)/exp(1))^(shp - 1)) 

  return( list(L.unscaled = key, 
               params = cbind(s, shp)))  # return params on log scale

}
