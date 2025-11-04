#' @title Hermite expansion factors
#' 
#' @description Computes Hermite expansion terms for use in 
#' distance analysis. The Hermite (and other expansions) allow "wiggle" in
#' estimated distance functions. 
#'  
#' @inheritParams cosine.expansion
#'    
#' @details There are, in general, several expansions that can be called Hermite. Let \eqn{w = 4x - 2}{w = 4*x - 2}. 
#' Rdistance's Hermite expansions are:
#'   \itemize{
#'     \item \bold{First term}: \deqn{h_1(w) = w + 2,}{h1(w) = w + 2,}
#'     \item \bold{Second term}: \deqn{h_2(w) = w^2 - 4,}{h2(w) = w^2 - 4,}
#'     \item \bold{Third term}: \deqn{h_3(w) = w^3 - 3w + 2,}{h3(w) = w^3 - 3*w + 2,}
#'     \item \bold{Fourth term}: \deqn{h_4(w) = w^4 - 6w^2 + 8,}{h4(w) = w^4 - 6*w^2 + 8,}
#'   }
#'   The maximum number of expansion terms computed is 4.
#'   
#' @inherit cosine.expansion return
#'   
#' @seealso \code{\link{dfuncEstim}}
#' , \code{\link{cosine.expansion}}
#' , \code{\link{sine.expansion}}
#' , \code{\link{simple.expansion}}.
#'   
#' @examples 
#' x <- matrix(seq(0, 1, length = 200), ncol = 1)
#' herm.expn <- hermite.expansion(x, 4)
#' plot(range(x), range(herm.expn), type="n")
#' matlines(x, herm.expn[,1,1:4], col=rainbow(4), lty = 1)
#' 
#' @export

hermite.expansion <- function(x, expansions){

    expansions <- as.integer(expansions)  
  
    if (expansions > 4){
        warning("Too many Hermite polynomial expansion terms. Only 4 used.")
        expansions = 4
    }
    
    if( expansions < 1 ) stop( "Number of expansions must be >= 1" )

    expansion = array(dim = c(nrow(x), ncol(x), expansions))
  
    x <- 4*x - 2

    expansion[,,1] = x + 2

    if(expansions >= 2){
        expansion[,,2] = x^2 - 4
    } 
    if(expansions >= 3){
        expansion[,,3] = x^3 - 3*x + 2
    } 
    if(expansions >= 4){
        expansion[,,4] =  x^4 - 6*x^2 + 8
    }         
    
    return(expansion)
            
}
