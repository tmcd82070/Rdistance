#' @title cosine.expansion - Cosine expansion terms
#' 
#' @description 
#' Computes the cosine expansion terms used to modify the 
#' shape of distance likelihood functions. 
#'   
#' @param x A numeric vector of distances at which to evaluate 
#' the expansion series. For distance analysis, \code{x} is
#' of the proportion of a strip transect's half-width 
#' at which a group of individuals were sighted, i.e., \eqn{d/w}.  
#'   
#' @param expansions A scalar specifying the number of expansion terms to compute. Must be one of the 
#' integers 1, 2, 3, 4, or 5.
#' 
#' @details There are, in general, several expansions that can be called cosine. The cosine expansion used here is:
#'   \itemize{
#'     \item \bold{First term}: \deqn{h_1(x)=(1+\cos(2\pi x))/(2n),}{h1(x) = (1 + cos(2*Pi*x))/(2n),}
#'     \item \bold{Second term}: \deqn{h_2(x)=(1 + \cos(4\pi x))/(2n),}{h2(x) = (1+cos(4*Pi*x))/(2n),}
#'     \item \bold{Third term}: \deqn{h_3(x)=\cos(4\pi x),}{h3(x) = cos(4*Pi*x),}
#'     \item \bold{Fourth term}: \deqn{h_4(x)=\cos(5\pi x),}{h4(x) = cos(5*Pi*x),}
#'     \item \bold{Fifth term}: \deqn{h_5(x)=\cos(6\pi x),}{h5(x) = cos(6*Pi*x),}
#'   }
#'   where n is number of expansions (i.e., \code{expansions}).
#'   The maximum number of expansion terms computed is 5.
#' @return A matrix of size \code{length(x)} X \code{expansions}.  The columns of this matrix are the cosine expansions of 
#'   \code{x}. Column 1 is the first expansion term of \code{x}, column 2 is the second expansion term of \code{x}, and so on 
#'   up to \code{expansions}.
#'   
#' @seealso \code{\link{dfuncEstim}}, \code{\link{hermite.expansion}}, \code{\link{simple.expansion}}, and the discussion 
#'   of user defined likelihoods in \code{\link{dfuncEstim}}.
#'   
#' @examples 
#' x <- seq(0, 1, length = 200)
#' cos.expn <- cosine.expansion(x, 5)
#' plot(range(x), range(cos.expn), type="n")
#' matlines(x, cos.expn, col=rainbow(5), lty = 1)
#' @keywords models
#' @export

cosine.expansion <- function(x, expansions){

    if (expansions > 5){
        warning("Too many Cosine polynomial expansion terms. Only 5 used.")
        expansions = 5
    }
    
    if( expansions < 1 ) stop( "Number of expansions must be >= 1" )
    
    expansion = array(dim = c(nrow(x), ncol(x), expansions))

    expansion[,,1] = (1 + cos(2*pi*x)) / 2 / expansions
    
    # I realize I could do this in a for loop, but I think this is faster.    
    if(expansions >= 2){
        expansion[,,2] = (1 + cos(4*pi*x)) / 2 / expansions
    }
    if(expansions >= 3){
        expansion[,,3] = (1 + cos(6*pi*x)) / 2 / expansions
    }
    if(expansions >= 4){
        expansion[,,4]=  (1 + cos(8*pi*x)) / 2 / expansions
    }
    if(expansions >= 5){
        expansion[,,5] = (1 + cos(10*pi*x)) / 2 / expansions
    }      
   
    return(expansion)
            
}
