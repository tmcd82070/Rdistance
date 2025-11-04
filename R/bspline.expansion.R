#' @title B-spline expansion terms
#' 
#' @description 
#' Computes spline, specifically b-spline, expansion terms that modify the 
#' shape of distance likelihood functions. 
#'   
#' @inheritParams cosine.expansion
#' 
#' @inherit cosine.expansion return
#'   
#' @seealso \code{\link{dfuncEstim}}, 
#'          \code{\link{cosine.expansion}},
#'          \code{\link{hermite.expansion}}, 
#'          \code{\link{simple.expansion}}.
#'   
#' @examples 
#' x <- matrix(seq(0, 1, length = 200), ncol = 1)
#' spl.expn <- bspline.expansion(x, 5)
#' plot(range(x), range(spl.expn), type="n")
#' matlines(x, spl.expn[,1,1:5], col=rainbow(5), lty = 1)
#' 
#' @export
bspline.expansion <- function(x, expansions){

  if( expansions < 1 ) stop( "Number of expansions must be >= 1" )
  
  expansions <- as.integer(expansions)  
  
  if( expansions == 1){
    bs = x
  } else if(expansions == 2){
    bs = splines::bs(x, df = 2, degree = 2)
  } else if(expansions >= 3){
    bs = splines::bs(x, df = expansions, degree = 3)
  }

  bs <- array(bs, dim = c(nrow(x), ncol(x), expansions))
  return(bs)
            
}
