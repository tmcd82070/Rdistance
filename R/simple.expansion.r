#' @name simple.expansion
#' @aliases simple.expansion
#' @title Calculate simply polynomial expansion for detection function likelihoods.
#' @description Computes simple polynomial expansion terms used in the likelihood of a distance analysis. More generally, will compute
#'   polynomial expansions of any numeric vector.
#' @usage simple.expansion(x, expansions)
#' @param x In a distance analysis, \code{x} is a numeric vector of the proportion of a strip transect's half-width 
#'   at which a group of individuals were sighted.  If \eqn{w} is the strip transect half-width or maximum sighting 
#'   distance, and \eqn{d} is the perpendicular off-transect distance to a sighted group (\eqn{d\leq w}{d <= w}), 
#'   \code{x} is usually \eqn{d/w}.  More generally, \code{x} is a vector of numeric values
#' @param expansions A scaler specifying the number of expansion terms to compute. Must be one of the integers 1, 2, 3, or 4.
#' @details The polynomials computed here are:
#'   \itemize{
#'     \item \bold{First term}: \deqn{h_1(x)=x^4,}{h1(x) = x^4,}
#'     \item \bold{Second term}: \deqn{h_2(x)=x^6,}{h2(x) = x^6,}
#'     \item \bold{Third term}: \deqn{h_3(x)=x^8,}{h3(x) = x^8,}
#'     \item \bold{Fourth term}: \deqn{h_4(x)=x^10,}{h4(x) = x^10,}
#'   }
#'   The maximum number of expansion terms computed is 4.
#' @value A matrix of size \code{length(x)} X \code{expansions}.  The columns of this matrix are the Hermite polynomial expansions of \code{x}. 
#'   Column 1 is the first expansion term of \code{x}, column 2 is the second expansion term of \code{x}, and so on up to \code{expansions}.
#' @author Trent McDonald, WEST, Inc. \email{tmcdonald@west-inc.com}
#'         Aidan McDonald, WEST, Inc. \email{aidan@mcdcentral.org}
#' @seealso \code{\link{F.dfunc.estim}}, \code{\link{cosine.expansion}}, \code{\link{hermite.expansion}}, and the discussion 
#'   of user defined likelihoods in \code{\link{F.dfunc.estim}}.
#' @examples set.seed(883839)
#'   x <- rnorm(1000) * 100
#'   x <- x[ 0 < x & x < 100 ]
#'   simp.expn <- simple.expansion(x, 4)
#' @keywords models

simple.expansion <- function(x, expansions){
# Calculates simple polynomial for detection function.
# Input:
#       x = distances / w
#       expansions = number of expansion terms (1 - 4)
#
# Output:
#       expansion = a list of vectors, with
#                   expansion[,1] = expansion for the 1st term,
#                   expansion[,2] = expansion for the 2nd, and so on.
#

    if (expansions > 4){
        warning("Too many Simple polynomial expansion terms. Only 4 used.")
        expansions = 4
    }

    if( expansions < 1 ) stop( "Number of expansions must be >= 1" )

    expansion = matrix(nrow=length(x), ncol=expansions)

    expansion[,1] = (x)^4

    if(expansions >= 2){
        expansion[,2] = (x)^6
    }
    if(expansions >= 3){
        expansion[,3] = (x)^8
    }
    if(expansions >= 4){
        expansion[,4] =  (x)^10
    }

    return(expansion)

}
