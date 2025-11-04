#' @title Simple polynomial expansion factors
#' 
#' @description Computes simple polynomial expansion terms 
#' for use in distance analysis. The Simple (and other expansions) 
#' allow "wiggle" in estimated distance functions.
#'   
#' @inheritParams cosine.expansion
#' 
#' @details The polynomials computed here are:
#'   \itemize{
#'     \item \bold{First term}: \deqn{h_1(x)=x^4,}{h1(x) = x^4,}
#'     \item \bold{Second term}: \deqn{h_2(x)=x^6,}{h2(x) = x^6,}
#'     \item \bold{Third term}: \deqn{h_3(x)=x^8,}{h3(x) = x^8,}
#'     \item \bold{Fourth term}: \deqn{h_4(x)=x^{10},}{h4(x) = x^10,}
#'   }
#'   The maximum number of expansion terms computed is 4.
#'
#' @inherit cosine.expansion return
#'   
#' @seealso \code{\link{dfuncEstim}}
#' , \code{\link{cosine.expansion}}
#' , \code{\link{sine.expansion}}
#' , \code{\link{hermite.expansion}}.
#'   
#' @examples 
#' x <- matrix(seq(0, 1, length = 200), ncol = 1)
#' simp.expn <- simple.expansion(x, 4)
#' plot(range(x), range(simp.expn), type="n")
#' matlines(x, simp.expn[,1,1:4], col=rainbow(4), lty = 1)
#' 
#' @export

simple.expansion <- function(x, expansions){

    expansions <- as.integer(expansions)  
  
    if (expansions > 4){
        warning("Too many Simple polynomial expansion terms. Only 4 used.")
        expansions = 4
    }

    if( expansions < 1 ) stop( "Number of expansions must be >= 1" )

    expansion = array(dim = c(nrow(x), ncol(x), expansions))
  
    expansion[,,1] = x^4

    if(expansions >= 2){
        expansion[,,2] = x^6
    }
    if(expansions >= 3){
        expansion[,,3] = (x)^8
    }
    if(expansions >= 4){
        expansion[,,4] =  (x)^10
    }

    return(expansion)

}
