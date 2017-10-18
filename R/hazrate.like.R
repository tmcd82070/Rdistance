#' @title Hazard rate likelihood function for distance analyses
#' 
#' @description This function computes likelihood contributions for 
#' off-transect sighting distances, scaled appropriately, for use as 
#' a distance likelihood.
#' 
#' @param a A vector of likelihood parameter values. Length and meaning 
#' depend on \code{series} and \code{expansions}. If no expansion terms 
#' were called for
#'   (i.e., \code{expansions = 0}), the distance likelihoods contain 
#'   one or two canonical parameters (see Details). If one or more 
#'   expansions are called for,
#'   coefficients for the expansion terms follow coefficients for the 
#'   canonical parameters.  If \code{p} is the number of canonical 
#'   parameters, coefficients
#'   for the expansion terms are \code{a[(p+1):length(a)]}.
#'   
#' @param dist A numeric vector containing the observed distances.
#' 
#' @param covars Data frame containing values of covariates at 
#' each observation in \code{dist}.
#' 
#' @param w.lo Scalar value of the lowest observable distance.  
#' This is the \emph{left truncation} of sighting distances in 
#' \code{dist}. Same units as \code{dist}.
#' Values less than \code{w.lo} are allowed in \code{dist}, 
#' but are ignored and their contribution to the likelihood is 
#' set to \code{NA} in the output.
#' 
#' @param w.hi Scalar value of the largest observable distance.  
#' This is the \emph{right truncation} of sighting distances in 
#' \code{dist}.  Same units as \code{dist}.
#' Values greater than \code{w.hi} are allowed in \code{dist}, 
#' but are ignored and their contribution to the likelihood is 
#' set to \code{NA} in the output.
#' 
#' @param series A string specifying the type of expansion to use.  
#' Currently, valid values are 'simple', 'hermite', and 'cosine'; but, see 
#'   \code{\link{dfuncEstim}} about defining other series.
#'   
#' @param expansions A scalar specifying the number of terms in 
#' \code{series}. Depending on the series, this could be 0 through 5.
#'   The default of 0 equates to no expansion terms of any type.
#'   
#' @param scale Logical scaler indicating whether or not to scale 
#' the likelihood so it integrates to 1. This parameter is used to 
#' stop recursion in other functions.
#' If \code{scale} equals TRUE, a numerical integration 
#' routine (\code{\link{integration.constant}}) is called, which 
#' in turn calls this likelihood function again
#' with \code{scale} = FALSE. Thus, this routine knows when its 
#' values are being used to compute the likelihood and when its 
#' value is being used to compute the 
#' constant of integration.  All user defined likelihoods must have 
#' and use this parameter.
#' 
#' @param pointSurvey Boolean. TRUE if \code{dist} is point 
#' transect data, FALSE if line transect data.
#' 
#' @details The hazard rate likelihood is 
#' \deqn{f(x|a,b) = 1 - \exp(-(x/\sigma)^{-\beta})}{%
#' f(x|a,b) = 1 - exp(-(x/Sigma)^(-Beta))} 
#' where \eqn{\sigma}{Sigma} is a variance parameter, 
#' and \eqn{\beta}{Beta}
#'   is a slope parameter to be estimated. 
#'   
#'   \bold{Expansion Terms}: If \code{expansions} = k 
#'   (k > 0), the expansion function specified by 
#'   \code{series} is called (see for example
#'   \code{\link{cosine.expansion}}). Assuming 
#'   \eqn{h_{ij}(x)}{h_ij(x)} is the \eqn{j^{th}}{j-th} 
#'   expansion term for the \eqn{i^{th}}{i-th} distance and that 
#'   \eqn{c_1, c_2, \dots, c_k}{c(1), c(2), ..., c(k)} are 
#'   (estimated) coefficients for the expansion terms, the 
#'   likelihood contribution for the \eqn{i^{th}}{i-th} 
#'   distance is, \deqn{f(x|a,b,c_1,c_2,\dots,c_k) = f(x|a,b)(1 + 
#'   \sum_{j=1}^{k} c_j h_{ij}(x)).}{%
#'   f(x|a,b,c_1,c_2,...,c_k) = f(x|a,b)(1 + c(1) h_i1(x) + 
#'   c(2) h_i2(x) + ... + c(k) h_ik(x)). }
#'   
#' @return A numeric vector the same length and order as 
#' \code{dist} containing the likelihood contribution for 
#' corresponding distances in \code{dist}. 
#' Assuming \code{L} is the returned vector from one of these 
#' functions, the full log likelihood of all the data is 
#' \code{-sum(log(L), na.rm=T)}. Note that the
#'   returned likelihood value for distances less than 
#'   \code{w.lo} or greater than \code{w.hi} is \code{NA}, 
#'   and thus it is prudent to use \code{na.rm=TRUE} in the
#'   sum. If \code{scale} = TRUE, the integral of the likelihood 
#'   from \code{w.lo} to \code{w.hi} is 1.0. If \code{scale} = 
#'   FALSE, the integral of the likelihood is
#'   arbitrary.
#'   
#' @author Trent McDonald, WEST, Inc. \email{tmcdonald@west-inc.com}\cr
#'         Aidan McDonald, WEST, Inc. \email{aidan@mcdcentral.org}
#'         
#' @seealso \code{\link{dfuncEstim}},
#'          \code{\link{halfnorm.like}},
#'          \code{\link{uniform.like}},
#'          \code{\link{negexp.like}},
#'          \code{\link{Gamma.like}}
#'          
#' @examples \dontrun{
#' x <- seq(0, 100, length=100)
#' 
#' # Plots showing effects of changes in sigma
#' plot(x, hazrate.like(c(20, 5), x), type="l", col="red")
#' plot(x, hazrate.like(c(40, 5), x), type="l", col="blue")
#' 
#' # Plots showing effects of changes in beta
#' plot(x, hazrate.like(c(50, 20), x), type="l", col="red")
#' plot(x, hazrate.like(c(50, 2), x), type="l", col="blue")
#' }
#'          
#' @keywords models
#' @export

hazrate.like <- function(a, dist, covars = NULL, w.lo = 0, 
                         w.hi = max(dist), series = "cosine", 
                         expansions = 0, scale = TRUE, 
                         pointSurvey = FALSE, ...){
	

    dist[ (dist < w.lo) | (dist > w.hi) ] <- NA
  
    if(!is.null(covars)){
      q <- ncol(covars)
      beta <- a[1:q]
      s <- drop( covars %*% beta )
      # s <- 0
      # for (i in 1:(ncol(covars)))
      #   s <- s + a[i]*covars[,i]
      s <- exp(s)
    } else {
      s <- a[1]
    }
	
	beta = a[length(a) - expansions]
	key = 1 - exp(-(dist/s)^(-beta))
    dfunc <- key
    w <- w.hi - w.lo


    if(expansions > 0){

        nexp <- expansions #nexp <- min(expansions,length(a)-2)  # should be equal. If not, fire warning next
        
        #if( length(a) != (expansions+2) ) {
        #    warning("Wrong number of parameters in expansion. Should be (expansions+2). Higher terms ignored.")
        #}

		if (series=="cosine"){
            dscl = dist/w
            exp.term <- cosine.expansion( dscl, nexp )
		} else if (series=="hermite"){
            dscl = dist/s
            exp.term <- hermite.expansion( dscl, nexp )
		} else if (series == "simple") {
            dscl = dist/w
            exp.term <- simple.expansion( dscl, nexp )
        } else {
            stop( paste( "Unknown expansion series", series ))
        }

        dfunc <- key * (1 + c(exp.term %*% a[(length(a)-(nexp-1)):(length(a))]))


    }# else if(length(a) > 2){
    #    warning("Wrong number of parameters in hazrate. Only 2 needed if no expansions. Higher terms ignored.")
    #}

    if( scale ){
        dfunc = dfunc / integration.constant(dist, hazrate.like, w.lo=w.lo, w.hi=w.hi, a=a, covars = covars, series=series,expansions=expansions, pointSurvey = pointSurvey, ...)
    }
    
    c(dfunc)
}
