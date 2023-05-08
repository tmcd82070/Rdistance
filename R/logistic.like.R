#' @title logistic.like - Logistic distance function likelihood
#' 
#' @description Computes a two parameter logistic distance function.
#' 
#' @param a A vector of likelihood parameter values. Length and meaning 
#' depend on whether covariates and \code{expansions} are present as follows:
#' \itemize{
#'    \item If no covariates and no expansions: \code{a} = [a, b] (see Details)
#'    \item If no covariates and \emph{k} expansions: \code{a} = [a, b, e1, ..., e\emph{k}] 
#'    \item If \emph{p} covariates and no expansions: \code{a} = [a, b, b1, ..., b\emph{p}] 
#'    \item If \emph{p} covariates and \emph{k} expansions: \code{a} = [a, b, b1, ..., b\emph{p}, e1, ..., e\emph{k}] 
#' }
#'  
#' @param dist A numeric vector containing observed distances with measurement
#' units.
#' 
#' @param covars Data frame containing values of covariates at 
#' each observation in \code{dist}.
#' 
#' @param w.lo Scalar value of the lowest observable distance, with measurement
#' units.  
#' This is the \emph{left truncation} sighting distance.  Values less than 
#' \code{w.lo} are allowed in \code{dist}, but are ignored and 
#' their likelihood value is set to
#'  \code{NA} in the output.
#'  
#' @param w.hi Scalar value of the largest observable distance, with measurement
#' units.  
#' This is the \emph{right truncation} sighting distance.
#' Values greater than \code{w.hi} are allowed in \code{dist}, 
#' but are ignored and their likelihood value is 
#' set to \code{NA} in the output.
#' 
#' @param series A string specifying the type of expansion to 
#' use.  Currently, valid values are 'simple', 'hermite', and 
#' 'cosine'; but, see \code{\link{dfuncEstim}} about 
#' defining other series.
#' 
#' @param expansions A scalar specifying the number of terms 
#' in \code{series}. Depending on the series, this could be 0 through 5.
#' The default of 0 equates to no expansion terms of any type.
#'   
#' @param scale Logical scalar indicating whether or not to scale 
#' the likelihood into a density function, i.e., so that it integrates 
#' to 1. This parameter is used 
#' to stop recursion in other functions.
#' If \code{scale} equals TRUE, a numerical integration 
#' routine (\code{\link{integration.constant}}) is called, which 
#' in turn calls this likelihood function again
#' with \code{scale} = FALSE. Thus, this routine knows when its 
#' values are being used to compute the likelihood and when its 
#' values are being used to compute the constant of integration.  
#' All user defined likelihoods must have and use this parameter.
#' 
#' @param pointSurvey Boolean. TRUE if \code{dist} is point 
#' transect data, FALSE if line transect data.
#' 
#' @details 
#' 
#' The 'logistic' likelihood contains two 
#' parameters.  Parameter \eqn{a} determines the scale and is 
#' labeled 'threshold' in Rdistance.  Parameter \eqn{b} determines 
#' sharpness (slope) of the likelihood's decrease at \eqn{a} and is labeled
#' 'knee' in Rdistance.  
#' This function is sometimes called the 
#' \emph{heavy side} function (e.g., engineering).  The technical form 
#' of the function is, 
#' \deqn{f(x|a,b) = 1 - \frac{1}{1 + \exp(-b(x-a))} = 
#' \frac{\exp( -b(x-a) )}{1 + exp( -b(x-a) )},}{%
#' f(x|a,b) = 1 - 1 / (1 + exp(-b*(x-a))) = exp(-b*(x-a)) / (1 + exp(-b*(x-a))).} 
#' 
#' Parameter \eqn{a} is the location (distance) of 
#' 0.5. That is, the inverse likelihood of 0.5 
#' is \eqn{a} before scaling 
#' (i.e., \code{logistic.like( c(a,b), a, scale=FALSE)} equals 
#' \code{0.5}). 
#' 
#' Parameter \eqn{b} is slope of function 
#' at \eqn{a}.  
#' Prior to scaling, 
#' slope of the likelihood at \eqn{a} is \eqn{-b/4}. 
#' If \eqn{b}
#' is large, the "knee" is sharp and the likelihood can look 
#' uniform with support from 
#' \code{w.lo} to \eqn{a/f(0)}.  If \eqn{b} is small, the 
#' "knee" is shallow and the density of observations declines 
#' in an elongated "S" shape pivoting at \eqn{a/f(0)}.  
#' As \eqn{b} grows large and assuming f(0) = 1, the effective 
#' strip width approaches \eqn{a}.  
#' 
#' See plots in Examples. 
#' 
#' @section Expansion Terms:
#' 
#' If \code{expansions} = k (k > 0), the 
#' expansion function specified by \code{series} is called (see for example
#' \code{\link{cosine.expansion}}). Assuming 
#' \eqn{h_{ij}(x)}{h_ij(x)} is the \eqn{j^{th}}{j-th} expansion term 
#' for the \eqn{i^{th}}{i-th} distance and that 
#' \eqn{c_1, c_2, \dots, c_k}{c(1), c(2), ..., c(k)} are (estimated) 
#' coefficients, the likelihood contribution 
#' for the \eqn{i^{th}}{i-th} distance is, 
#' \deqn{f(x|a,b,c_1,c_2,\dots,c_k) = f(x|a,b)(1 + 
#' \sum_{j=1}^{k} c_j h_{ij}(x)).}{%
#' f(x|a,b,c_1,c_2,...,c_k) = f(x|a,b)(1 + c(1) h_i1(x) + 
#' c(2) h_i2(x) + ... + c(k) h_ik(x)). }
#'   
#' @return A numeric vector the same length and order as \code{dist} 
#' containing the likelihood contribution for corresponding distances 
#' in \code{dist}. 
#' Assuming \code{L} is the returned vector, 
#' the log likelihood of all data is \code{-sum(log(L), na.rm=T)}. 
#' Note that the returned likelihood value for distances less than 
#' \code{w.lo} or greater than \code{w.hi} is \code{NA}, and thus it is 
#' essential to use \code{na.rm=TRUE} in the sum. If \code{scale} = TRUE, 
#' the integral of the likelihood from \code{w.lo} to \code{w.hi} is 1.0. 
#' If \code{scale} = FALSE, the integral of the likelihood is
#' arbitrary.
#'   
#' @seealso \code{\link{dfuncEstim}},
#'          \code{\link{halfnorm.like}},
#'          \code{\link{hazrate.like}},
#'          \code{\link{negexp.like}},
#'          \code{\link{Gamma.like}}
#'          
#' @examples 
#' x <- units::set_units(seq(0, 100, length=100), "m")
#' 
#' # Plots showing effects of changes in Threshold
#' plot(x, logistic.like(c(20, 20), x), type="l", col="red")
#' lines(x, logistic.like(c(40, 20), x), type="l", col="blue")
#' 
#' # Plots showing effects of changes in Knee
#' plot(x, logistic.like(c(50, 100), x), type="l", col="red")
#' lines(x, logistic.like(c(50, 1), x), type="l", col="blue")
#' lines(x, logistic.like(c(50, .1), x), type="l", col="orange")
#' 
#'          
#' @keywords models
#' @export

logistic.like <- function(a
                        , dist
                        , covars = NULL
                        , w.lo = units::set_units(0,"m")
                        , w.hi = max(dist)
                        , series = "cosine"
                        , expansions = 0
                        , scale = TRUE
                        , pointSurvey = FALSE
                        ){

  # rule is: parameter 'a' never has units.
  # upon entry: 'dist', 'w.lo', and 'w.hi' all have units 
  
  #   A couple internal functions first.
  #   This is the heavy-side function.  Basically, a steep logistic. f is just heavi flipped over
  heavi <- function(x,k){ 1 / (1 + exp( -k * x ))}
  f <- function(beta1, beta2, x){ 1 - heavi(x-beta1,beta2) }

  # What's in a? : 
  #   If no covariates: a = [a, b, <expansion coef>]
  #   If covariates:    a = [(Intercept), b1, ..., bp, b, <expansion coef>]
  
  # Comparison respects units. ie., if dist and w.lo have different units 
  # the "<" operation converts to a common units, then compares. The following 
  # is safe provided dist and w.lo/w.hi have units, which they do.
  dist[ (dist < w.lo) | (dist > w.hi) ] = NA
  
  if(!is.null(covars)){
    q <- ncol(covars)
    beta <- a[1:q]
    s <- drop( covars %*% beta )
    A.param <- exp(s)  # link function here
  } else {
    A.param <- a[1]  # no link function without covars
  }
  
  B.param <- a[length(a)-expansions]
  key <- f(A.param, B.param, units::drop_units(dist))

  # If there are expansion terms
  if(expansions > 0){

      w <- w.hi - w.lo  # units taken care of here
      
  		if (series=="cosine"){
              dscl = units::drop_units(dist/w)
              exp.term <- cosine.expansion( dscl, expansions )
  		} else if (series=="hermite"){
              dscl = units::drop_units(dist/ (a[1]/sqrt(12)))  # denom is approx std of U[0,a[1]]
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
          key = key / integration.constant(dist = dist
                                         , density = logistic.like
                                         , a = a
                                         , covars = covars
                                         , w.lo = w.lo
                                         , w.hi = w.hi
                                         , series = series
                                         , expansions = expansions
                                         , pointSurvey = pointSurvey
                                         )
  }

  c(key)
}
