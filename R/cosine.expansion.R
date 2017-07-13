#' @name cosine.expansion
#' @aliases cosine.expansions
#' @title Calcululation of cosine expansion for detection function likelihoods.
#' @description Computes the cosine expansion terms used in the likelihood of a distance analysis. 
#'   More generally, will compute a cosine expansion of any numeric vector.
#' @usage cosine.expansion(x, expansions)
#' @param x In a distance analysis, \code{x} is a numeric vector of the proportion of a strip transect's half-width 
#'   at which a group of individuals were sighted.  If \eqn{w} is the strip transect half-width or maximum sighting 
#'   distance, and \eqn{d} is the perpendicular off-transect distance to a sighted group (\eqn{d\leq w}{d <= w}), 
#'   \code{x} is usually \eqn{d/w}.  More generally, \code{x} is a vector of numeric values
#' @param expansions A scaler specifying the number of expansion terms to compute. Must be one of the integers 1, 2, 3, 4, or 5.
#' @details There are, in general, several expansions that can be called cosine. The cosine expansion used here is:
#'   \itemize{
#'     \item \bold{First term}: \deqn{h_1(x)=\cos(2\pi x),}{h1(x) = cos(2*Pi*x),}
#'     \item \bold{Second term}: \deqn{h_2(x)=\cos(3\pi x),}{h2(x) = cos(3*Pi*x),}
#'     \item \bold{Third term}: \deqn{h_3(x)=\cos(4\pi x),}{h3(x) = cos(4*Pi*x),}
#'     \item \bold{Fourth term}: \deqn{h_4(x)=\cos(5\pi x),}{h4(x) = cos(5*Pi*x),}
#'     \item \bold{Fifth term}: \deqn{h_5(x)=\cos(6\pi x),}{h5(x) = cos(6*Pi*x),}
#'   }
#'   The maximum number of expansion terms computed is 5.
#' @value A matrix of size \code{length(x)} X \code{expansions}.  The columns of this matrix are the cosine expansions of 
#'   \code{x}. Column 1 is the first expansion term of \code{x}, column 2 is the second expansion term of \code{x}, and so on 
#'   up to \code{expansions}.
#' @author Trent McDonald, WEST, Inc. \email{tmcdonald@west-inc.com}
#'         Aidan McDonald, WEST, Inc. \email{aidan@mcdcentral.org}
#' @seealso \code{\link{F.dfunc.estim}}, \code{\link{hermite.expansion}}, \code{\link{simple.expansion}}, and the discussion 
#'   of user defined likelihoods in \code{\link{F.dfunc.estim}}.
#' @examples set.seed(33328)
#'   x <- rnorm(1000) * 100
#'   x <- x[ 0 < x & x < 100 ]
#'   cos.expn <- cosine.expansion(x, 5)
#' @keywords models

cosine.expansion <- function(x, expansions){
# Calculates cosine expansion for detection function.
# Input:
#       x = distances / w
#       expansions = number of expansion terms (1 - 5)
#
# Output:
#       expansion = a matrix with columns
#                   expansion[,1] = expansion for the 1st term,
#                   expansion[,2] = expansion for the 2nd, and so on.  
# changed 1st coeff to 2 - even function - jg
   
    if (expansions > 5){
        warning("Too many Cosine polynomial expansion terms. Only 5 used.")
        expansions = 5
    }
    
    if( expansions < 1 ) stop( "Number of expansions must be >= 1" )
    
    expansion = matrix(nrow=length(x), ncol=expansions)

    expansion[,1] = cos(2*pi*x)
    
    # I realize I could do this in a for loop, but I think this is faster.    
    if(expansions >= 2){
        expansion[,2] = cos(3*pi*x)
    }
    if(expansions >= 3){
        expansion[,3] = cos(4*pi*x)
    }
    if(expansions >= 4){
        expansion[,4]=  cos(5*pi*x)
    }
    if(expansions >= 5){
        expansion[,5] = cos(6*pi*x)
    }      
   
    return(expansion)
            
}
