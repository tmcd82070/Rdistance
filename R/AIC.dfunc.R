#' @title AIC.dfunc - AIC-related fit statistics for detection functions
#' 
#' @description Computes AICc, AIC, or BIC for estimated distance functions.
#' 
#' @inheritParams predict.dfunc
#' 
#' @param criterion String specifying the criterion to compute. Either 
#'   "AICc", "AIC", or "BIC".
#'   
#' @details Regular Akaike's information criterion 
#'   (\url{https://en.wikipedia.org/wiki/Akaike_information_criterion}) (\eqn{AIC}) is 
#'   \deqn{AIC = LL + 2p,}{AIC = (LL) + 2p,}
#'   where \eqn{LL} is the maximized value of the log likelihood 
#'   (the minimized value of the negative log 
#'   likelihood) and \eqn{p} is the 
#'   number of coefficients estimated in the detection function.  For 
#'   \code{dfunc} objects, \eqn{AIC} = \code{obj$loglik + 2*length(coef(obj))}.  
#'   
#'   A correction 
#'   for small sample size, \eqn{AIC_c}{AICc}, is 
#'   \deqn{AIC_c = LL + 2p + \frac{2p(p+1)}{n-p-1},}{AIC_c = LL + 2p + (2p(p+1))/(n-p-1),} 
#'   where \eqn{n} is sample 
#'   size or number of detected groups for distance analyses.  By default, this function 
#'   computes \eqn{AIC_c}{AICc}.   \eqn{AIC_c}{AICc} converges quickly to \eqn{AIC} 
#'   as \eqn{n} increases.
#'   
#'   The Bayesian Information Criterion (BIC) is
#'   \deqn{BIC = LL + log(n)p,}{BIC = (LL) + log(n)p}. 
#'   
#' @return A scalar, the requested fit statistic for \code{object}.
#' 
#' @references Burnham, K. P., and D. R. Anderson, 2002. 
#' \emph{Model selection and multi-model inference: 
#'    A practical information-theoretic approach, Second ed.} 
#'    Springer-Verlag. ISBN 0-387-95364-7.
#'   
#'   McQuarrie, A. D. R., and Tsai, C.-L., 1998. \emph{Regression and 
#'   time series model selection.} 
#'   World Scientific. ISBN 981023242X
#'   
#' @seealso \code{\link{coef}}, \code{\link{dfuncEstim}}
#' 
#' @examples 
#' data(sparrowDf)
#' dfunc <- sparrowDf |> dfuncEstim(dist~1)
#'   
#' # Fit statistics
#' AIC(dfunc)  # AICc
#' AIC(dfunc, criterion="AIC")  # AIC
#' AIC(dfunc, criterion="BIC")  # BIC
#' 
#' @keywords model
#' @export

AIC.dfunc=function (object, ..., criterion="AICc") 
{
  if( criterion == "AIC"){
    k <- 2
    n <- Inf
  } else if( criterion == "BIC"){
    k <- log(nrow(object$mf))
    n <- Inf
  } else {
    k <- 2
    n <- nrow(object$mf)
    criterion <- "AICc"
  }
    
  p <- length(coef(object))
  ans <- -2*object$loglik + (k*p) + (k * p * (p + 1))/(n - p - 1)
  attr(ans, "criterion") <- criterion
  ans
}