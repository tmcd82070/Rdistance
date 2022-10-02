#' @title Gamma.like - Gamma distance function 
#' 
#' @description Computes the gamma likelihood, 
#' scaled appropriately, for use as a likelihood 
#' in estimating a distance function.
#' 
#' @param a A vector of likelihood parameter values. Length and meaning depend on \code{series} and \code{expansions}. If no expansion terms were called for
#'   (i.e., \code{expansions = 0}), the distance likelihoods contain one or two canonical parameters (see Details). If one or more expansions are called for,
#'   coefficients for the expansion terms follow coefficients for the canonical parameters.  If \code{p} is the number of canonical parameters, coefficients
#'   for the expansion terms are \code{a[(p+1):length(a)]}.
#'   
#' @param dist A numeric vector containing the observed distances.
#' 
#' @param covars Data frame containing values of covariates at each observation in \code{dist}.
#' 
#' @param w.lo Scalar value of the lowest observable distance.  This is the \emph{left truncation} of sighting distances in \code{dist}. Same units as \code{dist}.
#'   Values less than \code{w.lo} are allowed in \code{dist}, but are ignored and their contribution to the likelihood is set to \code{NA} in the output.
#'   
#' @param w.hi Scalar value of the largest observable distance.  
#' This is the \emph{right truncation} of sighting distances in 
#' \code{dist}.  Same units as \code{dist}.
#' Values greater than \code{w.hi} are allowed in \code{dist}, but are ignored and their contribution to the likelihood is set to \code{NA} in the output.
#'   
#' @param series A string specifying the type of expansion to use.  Currently, valid values are 'simple', 'hermite', and 'cosine'; but, see 
#'   \code{\link{dfuncEstim}} about defining other series.
#' @param expansions A scalar specifying the number of terms in \code{series}. Depending on the series, this could be 0 through 5.
#'   The default of 0 equates to no expansion terms of any type.
#' @param scale Logical scalar indicating whether or not to scale the likelihood so it integrates to 1. This parameter is used to stop recursion in other functions.
#'   If \code{scale} equals TRUE, a numerical integration routine (\code{\link{integration.constant}}) is called, which in turn calls this likelihood function again
#'   with \code{scale} = FALSE. Thus, this routine knows when its values are being used to compute the likelihood and when its value is being used to compute the 
#'   constant of integration.  All user defined likelihoods must have and use this parameter.
#' @param pointSurvey Boolean. TRUE if \code{dist} is point transect data, FALSE if line transect data.
#' @details This function utilizes the built-in R function \code{dgamma} to evaluate the gamma density function.  Using the parameterization of \code{dgamma}, 
#'   the gamma shape parameter is \code{a[1]} while the gamma scale parameter is \code{(a[2]/gamma(r)) * (((r - 1)/exp(1))^(r - 1))}. Currently, this function 
#'   implements a non-covariate version of the gamma detection function used by Becker and Quang (2009).  In future, linear equations will relate covariate values 
#'   to values of the gamma parameters.  This future implementation will fully replicate the distance functions of Becker and Quang (2009).
#' @return A numeric vector the same length and order as \code{dist} containing the likelihood contribution for distances in \code{dist}.  Assuming 
#'   \code{L=gamma.like(c(r,lam),dist)}, the full log likelihood of all the data is \code{-sum(log(L), na.rm=T)}. Note that the returned likelihood value for 
#'   distances less than \code{w.lo} or greater than \code{w.hi} is \code{NA}, and thus it is prudent to use \code{na.rm=TRUE} in the sum. If \code{scale} = TRUE, 
#'   the integral of the likelihood from \code{w.lo} to \code{w.hi} is 1.0. If \code{scale} = FALSE, the integral of the likelihood is an arbitrary constant.
#'   
#' @references Becker, E. F., and P. X. Quang, 2009. \emph{A Gamma-Shaped Detection Function for Line-Transect Surveys with Mark-Recapture and Covariate Data.}
#'   Journal of Agricultural, Biological, and Environmental Statistics 14(2):207-223.
#'   
#' @author Trent McDonald, WEST, Inc. \email{tmcdonald@west-inc.com}
#'         Aidan McDonald, WEST, Inc. \email{aidan@mcdcentral.org}
#'         
#' @seealso \code{\link{dfuncEstim}}, \code{\link{halfnorm.like}}, \code{\link{hazrate.like}}, \code{\link{uniform.like}}, \code{\link{negexp.like}}
#' @examples \dontrun{
#' set.seed(238642)
#'x <- seq(0, 100, length=100)
#' 
#' # Plots showing effects of changes in shape
#' plot(x, Gamma.like(c(20,20), x), type="l", col="red")
#' plot(x, Gamma.like(c(40,20), x), type="l", col="blue")
#' 
#' # Plots showing effects of changes in scale
#' plot(x, Gamma.like(c(20,20), x), type="l", col="red")
#' plot(x, Gamma.like(c(20,40), x), type="l", col="blue")
#' 
#' # Estimate 'Gamma' distance function
#' r <- 5
#' lam <- 10
#' b <- (1/gamma(r)) * (((r - 1)/exp(1))^(r - 1))
#' x <- rgamma(1000, shape=r, scale=b*lam)
#' dfunc <- dfuncEstim(x~1, likelihood="Gamma", x.scl="max")
#' plot(dfunc)
#' }
#' @keywords models
#' @export
#' @importFrom stats dgamma
Gamma.like <- function(a, 
                       dist, 
                       covars = NULL, 
                       w.lo = units::set_units(0,"m"), 
                       w.hi = max(dist),
                       series = "cosine", 
                       expansions = 0, 
                       scale = TRUE,
                       pointSurvey = FALSE){

  # What's in a? : 
  #   If no covariates: a = [Shape, Scale, <expansion coef>]
  #   If covariates:    a = [(Intercept), b1, ..., bp, Shape, <expansion coef>]
  
  if(!is.null(covars)){
    q <- ncol(covars)
    beta <- a[1:q] 
    s <- drop( covars %*% matrix(beta,ncol=1) )      
    lam <- exp(s)  # link function here
  } else {
    lam <- a[1]
  }

  r <- a[length(a) - expansions]
  
  if( any(lam < 0) ) warning("Scale parameter of gamma likelihood invalid (< 0).")
  if( r <= 1 ) warning("Shape parameter of gamma likelihood invalid (<= 1).")

  dist[ (dist < w.lo) | (dist > w.hi) ] <- NA

  zero <- .Machine$double.xmin
    # This was all part of Becker and Quan's code
    #bb <- (1/gamma(r)) * (((r - 1)/exp(1))^(r - 1))
    #J <- dim(X.)[2]
    #eta <- X. %*% matrix(b2[1:(length(b2)-1)],ncol=1)
    #eta <- 0   # use if no covariates ?
    #lam <- exp(eta)
    #w1b <- wb/(lam * bb)
    #v1 <- dist/(lam * bb)
    #w1 <- w/(lam * bb)
    #loglik <- (r - 1) * log(v1) - v1 - lgamma(r) - log(bb) - eta - log(pgamma(w1, r))
    #if (USE.PGAMMA.WB){
    #    loglik <- (r - 1) * log(v1) - v1 - lgamma(r) - log(bb) - eta - log(pgamma(w1, r)-pgamma(w1b,r))
    #} else {
    #    #computationaly more efficient form of likelihood than eq. 8 in Becker and Quang 2008
    #    #note that when wb is zero pgamma(w1b,r) will also be zero so
    #    #     no blind strip in intergration
    #    loglik <- (r - 1) * log(v1) - v1 - lgamma(r) - log(bb) - eta - log(pgamma(w1, r))
    #}


    #   In the following, I am trying to avoid taking gamma(r), which could be huge   ## DON'T KNOW WHETER THIS WORKS
    #log.b <- -lgamma(r) + (r - 1)*(log(r - 1) - 1)
    #log.v1 <- log(dist) - log(lam) - log.b
    #v1 <- exp( log.v1 )
    #loglik <- (r - 1) * log.v1 - v1 - lgamma(r) - log.b - eta
    #like <- exp(loglik)

    #   In the following, I assume I can evaluate gamma(r)  ## THIS WORKS
    #b <- (1/gamma(r)) * (((r - 1)/exp(1))^(r - 1))
    #v1 <- dist/(lam * b)
    #w1 <- w1/(lam * b)
    #loglik <- (r - 1) * log(v1) - v1 - lgamma(r) - log(b) - eta # - log(pgamma(w1, r))  # this last term is meant to be integration constant, but it's off, and we use numerical integration below
    #like <- exp(loglik)

  # Note: Mode of Gamma distribution is lam * b * (r - 1) 

  #   In the following, I use the built in R function  ## THIS ALSO WORKS
  #   Note: I think Quan is missing an extra 1/lam in front of his density equation.
  b <- (1/gamma(r)) * (((r - 1)/exp(1))^(r - 1))
  key <- dgamma( units::drop_units(dist), shape=r, scale=lam*b )
    
    # print(b)
    # # By default we want g(x.scl) = 1
    # x.scl <- lam * b * (r - 1) # Mode of gamma distribution
    # g.at.x0 <- 1
    # f.at.x0 <- dgamma( x.scl, shape=r, scale=lam*b )
    # scaler <- g.at.x0 / f.at.x0
    # like <- like * scaler
    
  if(expansions > 0){
    
    w <- w.hi - w.lo
    
    if (series=="cosine"){
      dscl = units::drop_units(dist/w)  # unit conversion here; drop units is safe
      exp.term <- cosine.expansion( dscl, expansions )
    } else if (series=="hermite"){
      dscl = units::drop_units(dist/w)
      exp.term <- hermite.expansion( dscl, expansions )
    } else if (series == "simple") {
      dscl = units::drop_units(dist/w)
      exp.term <- simple.expansion( dscl, expansions )
    } else {
      stop( paste( "Unknown expansion series", series ))
    }
    
    expCoefs <- a[(length(a)-(expansions-1)):(length(a))]
    key <- key * (1 + c(exp.term %*% expCoefs))
    
    # without monotonicity restraints, function can go negative, 
    # especially in a gap between datapoints. This makes no sense in distance
    # sampling and screws up the convergence. 
    key[ which(key < 0) ] <- 0
  }
  
  if( scale ){
    # for fitting, likelihood must integrate to 1.0 
    # in the future, when we know the integral, we should compute it 
    # here instead of calling a separate numberical integration routine. 
    # For example, in the Gamma case without covars or expansions, we know
    #   gammaIntegral <- diff(pgamma(q = c(w.lo, w.hi)
    #                      , shape = r
    #                      , scale = lam * b))
    # But, you have to remember all cases (Points, Lines) X expansions X covars
    key = key / integration.constant(dist=dist,
                                          density=Gamma.like,
                                          a=a, 
                                          covars = covars,
                                          w.lo=w.lo, 
                                          w.hi=w.hi, 
                                          expansions = expansions,
                                          pointSurvey = pointSurvey,
                                          series = series)

  } 

  key

}
