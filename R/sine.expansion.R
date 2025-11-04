#' @title Sine expansion terms
#' 
#' @description 
#' Computes the sine expansion terms that modify the 
#' shape of distance likelihood functions. 
#'   
#' @inheritParams cosine.expansion
#' 
#' @details The sine expansion used here is:
#'   \itemize{
#'     \item \bold{First term}: \deqn{h_1(x)=\sin(2\pi x)/2,}{h1(x) = sin(2*Pi*x)/2,}
#'     \item \bold{Second term}: \deqn{h_2(x)=\sin(3\pi x)/2,}{h2(x) = sin(3*Pi*x)/2,}
#'     \item \bold{Third term}: \deqn{h_3(x)=\sin(4\pi x)/2,}{h3(x) = sin(4*Pi*x)/2,}
#'     \item \bold{Fourth term}: \deqn{h_4(x)=\sin(5\pi x)/2,}{h4(x) = sin(5*Pi*x)/2,}
#'     \item \bold{Fifth term}: \deqn{h_5(x)=\sin(6\pi x)/2,}{h5(x) = sin(6*Pi*x)/2,}
#'   }
#'   The maximum number of expansion terms is 5.
#'   
#' The sine expansion frequency in Rdistance is pi. Each term is one pi more than 
#' the previous. The cosine expansion frequency in Rdistance is 2*pi. Consequently, 
#' the sine and cosine expansions fit different models. 
#'   
#' @inherit cosine.expansion return
#'    
#' @seealso \code{\link{dfuncEstim}}, 
#'          \code{\link{cosine.expansion}}
#'   
#' @examples 
#' x <- matrix(seq(0, 1, length = 200), ncol = 1)
#' sin.expn <- sine.expansion(x, 5)
#' plot(range(x), range(sin.expn), type="n")
#' matlines(x, sin.expn[,1,1:5], col=rainbow(5), lty = 1)
#' 
#' @export

sine.expansion <- function(x, expansions){

  expansions <- as.integer(expansions)  
  
  if (expansions > 5){
      warning("Too many Sine polynomial expansion terms. Only 5 used.")
      expansions = 5
  }
  
  if( expansions < 1 ) stop( "Number of expansions must be >= 1" )
    
  expansion = array(dim = c(nrow(x), ncol(x), expansions))
  
  expansion[,,1] = sin(2*pi*x)
  
  # I realize I could do this in a for loop, but I think this is faster.    
  if(expansions >= 2){
    expansion[,,2] = sin(4*pi*x)
  }
  if(expansions >= 3){
    expansion[,,3] = sin(6*pi*x)
  }
  if(expansions >= 4){
    expansion[,,4]= sin(8*pi*x)
  }
  if(expansions >= 5){
    expansion[,,5] = sin(10*pi*x)
  }      

  # Sine expansion range = -.5 to .5
  expansion <- expansion / 2
  
  return(expansion)
            
}
