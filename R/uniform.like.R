#' @title uniform.like - Uniform distance function likelihood
#' 
#' @description Computes the uniform form of a distance function.
#' 
#' @param a A vector of likelihood parameter values. Length and meaning 
#' depend on \code{series} and \code{expansions}. If no expansion terms 
#' were called for (i.e., \code{expansions = 0}), the distance likelihoods 
#' contain one or two canonical parameters (see Details). If one or more 
#' expansions are called for, coefficients for the expansion terms 
#' follow coefficients for the canonical parameters in elements
#' \code{a[(p+1):length(a)]}, where \code{p} is 
#' the number of canonical parameters (2) plus number of covariates.
#' 
#' @param dist A numeric vector containing the observed distances.
#' 
#' @param covars Data frame containing values of covariates at 
#' each observation in \code{dist}.
#' 
#' @param w.lo Scalar value of the lowest observable distance.  
#' This is the \emph{left truncation} of sighting distances in 
#' \code{dist}. Same units as \code{dist}. Values less than 
#' \code{w.lo} are allowed in \code{dist}, but are ignored and 
#' their contribution to the likelihood is set to
#'  \code{NA} in the output.
#'  
#' @param w.hi Scalar value of the largest observable distance.  
#' This is the \emph{right truncation} of sighting distances 
#' in \code{dist}.  Same units as \code{dist}.
#' Values greater than \code{w.hi} are allowed in \code{dist}, 
#' but are ignored and their contribution to the likelihood is 
#' set to \code{NA} in the output.
#' 
#' @param series A string specifying the type of expansion to 
#' use.  Currently, valid values are 'simple', 'hermite', and 
#' 'cosine'; but, see \code{\link{dfuncEstim}} about 
#' defining other series.
#' 
#' @param expansions A scalar specifying the number of terms 
#' in \code{series}. Depending on the series, this could be 0 through 5.
#'   The default of 0 equates to no expansion terms of any type.
#'   
#' @param scale Logical scalar indicating whether or not to scale 
#' the likelihood so it integrates to 1. This parameter is used 
#' to stop recursion in other functions.
#' If \code{scale} equals TRUE, a numerical integration 
#' routine (\code{\link{integration.constant}}) is called, which 
#' in turn calls this likelihood function again
#' with \code{scale} = FALSE. Thus, this routine knows when its 
#' values are being used to compute the likelihood and when its 
#' value is being used to compute the constant of integration.  
#' All user defined likelihoods must have and use this parameter.
#' 
#' @param pointSurvey Boolean. TRUE if \code{dist} is point 
#' transect data, FALSE if line transect data.
#' 
#' @details 
#' 
#' The 'uniform' likelihood used here is 
#' technically not uniform. 
#' The function is continuous at its upper end where a true uniform 
#' is discontinuous at its upper end.  The function has two 
#' parameters (the upper end called the 'threshold' and 
#' the slope at the upper end called the 'knee').  In engineering, 
#' this function is sometimes called the 
#' \emph{heavy side} function.  In other areas of statistics, 
#' this function is called \emph{logistic}.  The technical form 
#' of the function is, 
#' \deqn{f(x|a,b) = 1 - \frac{1}{1 + \exp(-b(x-a))} = 
#' \frac{\exp( -b(x-a) )}{1 + exp( -b(x-a) )},}{%
#' f(x|a,b) = 1 - 1 / (1 + exp(-b*(x-a))) = exp(-b*(x-a)) / (1 + exp(-b*(x-a))),} 
#' where \eqn{a} is the "threshold" and \eqn{b} is the "knee". 
#' 
#' Parameter \eqn{a} = "threshold" is the location of the 
#' approximate upper limit of a uniform distribution's 
#' support.  The inverse likelihood of 0.5 
#' is \code{a} before scaling 
#' (i.e., \code{uniform.like(c(a,b),a,scale=FALSE)} equals 
#' \code{0.5}). 
#' 
#' Parameter \code{b} = "knee" is slope of function 
#' at \code{a}.  
#' Note that, prior to scaling for \code{g.x.scl}, 
#' the slope of the likelihood at \eqn{a} is \eqn{-b/4}. 
#' After scaling for \code{g.x.scl}, the inverse of 
#' \code{g.x.scl/2} is close to \code{a/f(0)}. If \eqn{b}
#' is large, the "knee" is sharp and the likelihood looks 
#' uniform with support from 
#' \code{w.lo} to \eqn{a/f(0)}.  If \eqn{b} is small, the 
#' "knee" is shallow and the density of observations declines 
#' in an elongated "S" shape pivoting at \code{a/f(0)}.  
#' As  \code{b} grows large and assuming f(0) = 1, the effective 
#' strip width approaches \code{a} from above.  
#' 
#' See Examples for plots using large and small values of \eqn{b}. 
#' 
#' \bold{Expansion Terms}: If \code{expansions} = k (k > 0), the 
#' expansion function specified by \code{series} is called (see for example
#' \code{\link{cosine.expansion}}). Assuming 
#' \eqn{h_{ij}(x)}{h_ij(x)} is the \eqn{j^{th}}{j-th} expansion term 
#' for the \eqn{i^{th}}{i-th} distance and that 
#' \eqn{c_1, c_2, \dots, c_k}{c(1), c(2), ..., c(k)} are (estimated) 
#' coefficients for the expansion terms, the likelihood contribution 
#' for the \eqn{i^{th}}{i-th} distance is, 
#' \deqn{f(x|a,b,c_1,c_2,\dots,c_k) = f(x|a,b)(1 + 
#' \sum_{j=1}^{k} c_j h_{ij}(x)).}{%
#' f(x|a,b,c_1,c_2,...,c_k) = f(x|a,b)(1 + c(1) h_i1(x) + 
#' c(2) h_i2(x) + ... + c(k) h_ik(x)). }
#'   
#' @return A numeric vector the same length and order as \code{dist} 
#' containing the likelihood contribution for corresponding distances 
#' in \code{dist}. 
#' Assuming \code{L} is the returned vector from one of these functions, 
#' the full log likelihood of all the data is \code{-sum(log(L), na.rm=T)}. 
#' Note that the returned likelihood value for distances less than 
#' \code{w.lo} or greater than \code{w.hi} is \code{NA}, and thus it is 
#' prudent to use \code{na.rm=TRUE} in the sum. If \code{scale} = TRUE, 
#' the integral of the likelihood from \code{w.lo} to \code{w.hi} is 1.0. 
#' If \code{scale} = FALSE, the integral of the likelihood is
#'   arbitrary.
#'   
#' @seealso \code{\link{dfuncEstim}},
#'          \code{\link{halfnorm.like}},
#'          \code{\link{hazrate.like}},
#'          \code{\link{negexp.like}},
#'          \code{\link{Gamma.like}}
#'          
#' @examples 
#' x <- seq(0, 100, length=100)
#' 
#' # Plots showing effects of changes in Threshold
#' plot(x, uniform.like(c(20, 20), x), type="l", col="red")
#' plot(x, uniform.like(c(40, 20), x), type="l", col="blue")
#' 
#' # Plots showing effects of changes in Knee
#' plot(x, uniform.like(c(50, 100), x), type="l", col="red")
#' plot(x, uniform.like(c(50, 1), x), type="l", col="blue")
#' 
#'          
#' @keywords models
#' @export

uniform.like <- function(a, 
                         dist, 
                         covars = NULL, 
                         w.lo = 0, 
                         w.hi = max(dist), 
                         series = "cosine", 
                         expansions = 0, 
                         scale = TRUE, 
                         pointSurvey = FALSE){

    #   A couple internal functions first.
    #   This is the heavy-side function.  Basically, a steep logistic. f is just heavi flipped over
    heavi <- function(x,k){ 1 / (1 + exp( -units::drop_units(k*x) ))}
    f <- function(beta1, beta2, x){ 1 - heavi(x-beta1,beta2) }


    dist[ (dist < w.lo) | (dist > w.hi) ] = NA
    beta <- c(0,0)
    if(!is.null(covars)){
      q <- ncol(covars)
      beta <- a[1:q]
      s <- drop( covars %*% beta )
      # s <- 0
      # for (i in 1:(ncol(covars)))
      #   s <- s + a[i]*covars[,i]
      beta1 <- exp(s)
    } else {
      beta1 <- a[1]
    }
    
    beta1 <-  units::set_units(beta1, units(dist), mode = "standard")

    beta2 <- a[length(a)-expansions]
	  key <- f(beta1, beta2, dist)
    dfunc <- key
    w <- w.hi - w.lo

    #    cat(paste( "w.lo=", w.lo, "w.hi=", w.hi, "\n"))

    # If there are expansion terms
    if(expansions > 0){

        nexp <- expansions #min(expansions,length(a)-2)  # should be equal. If not, fire warning next

    		if (series=="cosine"){
                dscl = units::drop_units(dist/w)
                exp.term <- cosine.expansion( dscl, nexp )
    		} else if (series=="hermite"){
                dscl = units::drop_units(dist/ (a[1]/sqrt(12)))  # denom is approx std of U[0,a[1]]
                exp.term <- hermite.expansion( dscl, nexp )
    		} else if (series == "simple") {
                dscl = units::drop_units(dist/w)
                exp.term <- simple.expansion( dscl, nexp )
        } else {
                stop( paste( "Unknown expansion series", series ))
        }
    
        dfunc <- key * (1 + c(exp.term %*% a[(length(a)-(nexp-1)):(length(a))]))
    }

    if( scale ){
            dfunc = dfunc / integration.constant(dist=dist, 
                                                 density=uniform.like, 
                                                 a=a,
                                                 covars = covars, 
                                                 w.lo=w.lo, 
                                                 w.hi=w.hi, 
                                                 series=series, 
                                                 expansions=expansions, 
                                                 pointSurvey = pointSurvey)
    }

    c(dfunc)
}
