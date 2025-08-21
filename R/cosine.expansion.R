#' @title Cosine expansion terms
#' 
#' @description 
#' Computes the cosine expansion terms that modify the 
#' shape of distance likelihood functions. 
#'   
#' @param x A numeric matrix of distances at which to evaluate 
#' the expansion series. For distance analysis, \code{x} should
#' be the proportion of the maximum sighting distance
#' at which a group was sighted, i.e., \eqn{x = d/w}, where \eqn{d}
#' is sighting distance and \eqn{w} is maximum sighting distance.
#'   
#' @param expansions A scalar specifying the number of 
#' expansion terms to compute. Must be one of the 
#' integers 1, 2, 3, 4, or 5.
#' 
#' @details The cosine expansion used here is:
#'   \itemize{
#'     \item \bold{First term}: \deqn{h_1(x)=\cos(2\pi x),}{h1(x) = cos(2*Pi*x),}
#'     \item \bold{Second term}: \deqn{h_2(x)=\cos(3\pi x),}{h2(x) = cos(3*Pi*x),}
#'     \item \bold{Third term}: \deqn{h_3(x)=\cos(4\pi x),}{h3(x) = cos(4*Pi*x),}
#'     \item \bold{Fourth term}: \deqn{h_4(x)=\cos(5\pi x),}{h4(x) = cos(5*Pi*x),}
#'     \item \bold{Fifth term}: \deqn{h_5(x)=\cos(6\pi x),}{h5(x) = cos(6*Pi*x),}
#'   }
#'   The maximum number of expansion terms is 5.
#'   
#' @return A 3D array of size \code{nrow(x)} X \code{ncol(x)} X \code{expansions}.
#'   The 'pages' (3rd dimension) of this array are the cosine expansions of 
#'   \code{x}. i.e., page 1 is the first expansion term of \code{x}, 
#'   page 2 is the second expansion term of \code{x}, etc.
#'   
#' @seealso \code{\link{dfuncEstim}}, 
#'          \code{\link{hermite.expansion}}, 
#'          \code{\link{simple.expansion}}
#'   
#' @examples 
#' x <- matrix(seq(0, 1, length = 200), ncol = 1)
#' cos.expn <- cosine.expansion(x, 5)
#' plot(range(x), range(cos.expn), type="n")
#' matlines(x, cos.expn[,1,1:5], col=rainbow(5), lty = 1)
#' @keywords models
#' @export

cosine.expansion <- function(x, expansions){

  if (expansions > 5){
      warning("Too many Cosine polynomial expansion terms. Only 5 used.")
      expansions = 5
  }
  
  if( expansions < 1 ) stop( "Number of expansions must be >= 1" )
    
  expansion = array(dim = c(nrow(x), ncol(x), expansions))
  
  expansion[,,1] = cos(2*pi*x)
  
  # I realize I could do this in a for loop, but I think this is faster.    
  if(expansions >= 2){
    expansion[,,2] = cos(3*pi*x)
  }
  if(expansions >= 3){
    expansion[,,3] = cos(4*pi*x)
  }
  if(expansions >= 4){
    expansion[,,4]= cos(5*pi*x)
  }
  if(expansions >= 5){
    expansion[,,5] = cos(6*pi*x)
  }      

  return(expansion)
            
}
