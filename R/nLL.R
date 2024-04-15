#' @title nLL - Negative log likelihood of distances
#' 
#' @description Return the negative log likelihood of 
#' a vector of observed distances given a specified likelihood, 
#'   number of expansion terms, and estimated parameters.
#'
#' @param a A vector of likelihood parameter values. Length and 
#' meaning depend on \code{ml$series} and \code{ml$expansions}. If no expansion 
#' terms were called for (i.e., \code{ml$expansions = 0}), the distance 
#' likelihoods contain one or two canonical parameters (see Details). 
#' If one or more expansions are called for, coefficients for the 
#' expansion terms follow coefficients for the canonical parameters.  
#' i.e., length of this vector must be 
#'   \code{(num Covars incl. intercept) + expansions + 1*(like \%in\% c("hazrate", "logistic", "huber"))}.
#'   
#' @inheritParams startLimits
#'  
#' @details
#' \bold{Expansion Terms}: If \code{ml$expansions} = k (k > 0), 
#' the expansion function specified by \code{ml$series} is 
#' called (see for example \code{\link{cosine.expansion}}). 
#' Assuming \eqn{h_{ij}(x)}{h_ij(x)} is the 
#' \eqn{j^{th}}{j-th} expansion term for the 
#' \eqn{i^{th}}{i-th} distance and that 
#' \eqn{c_1, c_2, \dots, c_k}{c(1), c(2), ..., c(k)} 
#' are (estimated) coefficients for the expansion terms, 
#' the likelihood contribution for the \eqn{i^{th}}{i-th} 
#' distance is, \deqn{f(x|a,b,c_1,c_2,\dots,c_k) = 
#' f(x|a,b)(1 + \sum_{j=1}^{k} c_j h_{ij}(x)).}{f(x|a,b,c_1,c_2,...,c_k) 
#' = f(x|a,b)(1 + c(1) h_i1(x) + c(2) h_i2(x) + ... + c(k) h_ik(x)). }
#'   
#' @return A scalar, the negative of the log likelihood evaluated at 
#' parameters \code{a}, including expansion terms.
#' 
#' @seealso See \code{\link{halfnorm.like}} and links there; 
#'  \code{\link{dfuncEstim}}
#' 
#' @examples
#' set.seed(238642)
#' 
#' d <- rnorm(1000, mean = 0, sd = 40)
#' d <- units::set_units(d[0 <= d], "m")
#' 
#' # Close to what happens in Rdistance
#' ml <- list(
#'     mf = model.frame(d ~ 1) 
#'   , likelihood = "halfnorm"
#'   , expansions = 0
#'   , w.lo = units::set_units(0, "m")
#'   , w.hi = units::set_units(125, "m")
#' )
#' nLL(log(40), ml)
#' 
#' # Another way
#' ones <- matrix(1, nrow = length(d), ncol = 1)
#' l <- halfnorm.like(log(40), d, ones)
#' scaler <-(pnorm(units::drop_units(ml$w.hi)
#'   , units::drop_units(ml$w.lo)
#'   , sd = l$params) - 0.5) * sqrt(2*pi) * l$params
#' -sum(log(l$key/scaler))
#'
#' # A third way, b/c we have pnorm() and dnorm(). 
#' l2 <- dnorm(units::drop_units(d), mean = 0, sd = 40)
#' scaler2 <- pnorm(125, mean = 0, sd = 40) - 0.5 
#' -sum(log(l2/scaler2))
#'  
#' @keywords models
#' @export

nLL <- function(a
                , ml
                ){
  
  # Pull data from input list ----
  # rule is: parameters in 'a' never have units,  
  # even though they could (e.g., sigma of halfnorm)
  #
  # upon entry: 'dist', 'w.lo', and 'w.hi' all have units 
  #
  # could move these retrievals outside nLL, which might 
  # speed things a little, but it's more params to pass in.
  f.like <- match.fun(paste( ml$likelihood, ".like", sep=""))
  X <- model.matrix(ml)  # this calls model.matrix.dfunc()
  dist <- Rdistance::distances(ml)
  
  # Because of na.exclude when building model frame, and 
  # other code in parseModel(), there are no missing distances 
  # in the response, nor are any outside the strip.  

  # cat(crayon::blue("===================="))
  # cat("\n")
  # cat(paste("In", crayon::blue("nLL"), "\n"))
  # print(a)
  # cat(crayon::blue("---- Calling "))
  # cat(ml$likelihood)
  # cat("\n")
      
  # Evaluate the "key" function ----
  # I call Buckland's "key" function just "likelihood".
  # Returns one value per observation. Expansions, if any, 
  # are applied below, after this.
  L <- f.like( a = a
             , dist = dist
             , covars = X
             )
  key <- L$key
  
  # Evaluate and apply the expansions ----
  exp.terms <- Rdistance::expansionTerms(a, ml)
  key <- key * exp.terms 
    
  # without monotonicity restraints, function can go negative, 
  # especially in a gap between datapoints. Don't want this in distance
  # sampling and screws up the convergence. In future, could
  # apply monotonicity constraint here.
  if( ml$expansions > 0 ){
    key[ which(key < 0) ] <- 0
  }

  # Scale the likelihood ----
  # Scalers should be unique to each observation and equal
  # to integral under distance function for that observation. 
  # Integrals are by defn unit-less.
  if( ml$expansions <= 0 && 
      (ml$likelihood %in% c("halfnorm", "negexp", "triangle"))){
    # We know the integral in these cases.  
    # Supposedly, this will speed things up
    theta <- L$params # n X (1) matrix of likelihood parameters in these cases
    if( ml$likelihood == "halfnorm"){
      # We evaluate normal with mean set to w.lo, sd = sigma, from -Inf to w.hi, then
      # subtract 0.5 from result for the area to left of mean (w.lo) 
      # theta = sigma in this case.
      scaler <- (pnorm(units::drop_units(ml$w.hi)
                       , units::drop_units(ml$w.lo)
                       , theta) - 0.5) * 
        sqrt(2*pi) * theta
    } else if( ml$likelihood == "negexp" ){
      # theta = beta (n X 1 vector) in this case
      scaler <- unname(
        (exp(-theta * units::drop_units(ml$w.lo)) -
           exp(-theta * units::drop_units(ml$w.hi))) / theta)
    } else if( ml$likelihood == "triangle" ){
      scaler <- theta / 2
    }
    
    key = key / scaler
    
  } else {
    # We numerically integrate.  These are integrals we 
    # do not know and any that have expansions
    key = key / integrationConstant(a, ml)
    
  }

  if( !is.null(getOption("Rdistance_optimizer")) &&
      (getOption("Rdistance_optimizer") == "optim") ){
    key <- key*10^9 # optim likes big numbers
  }
  
  #print(L)
  key[ !is.na(key) & (key <= 0) ] <- getOption("Rdistance_zero")   # happens at very bad values of parameters

  nLL <- -sum(log(key), na.rm=TRUE)  # Note that distances > w in L are set to NA

  # cat(paste("Log Likelihood:", crayon::red(nLL), "\n"))
  
  # Rules: 
  #   RULE 1 FOR LIKELIHOODS: No matter how bad the guess at a, you cannot return Inf, -Inf, NA, or NaN
  #   This means f.like can return NA, but not NaN (div by 0), Inf or -Inf for any row of data
  #   Must program the likelihoods to trap these values and return the appropriate .Machine constants
  #
  #   RULE 2 FOR NLL: It is possible for all rows of returned L to be non-Inf and non-NaN, 
  #   but there be enough of them that the sum overflows to Inf. I.e., values in L are 
  #   hyper-close to 0 and log(L) is close to -Inf, then sum overflows.  Trap this here.
  #
  if( is.infinite(nLL) ){
    nLL <- getOption("Rdistance_posInf") # positive b/c already flipped over by -1
  }
  
  nLL
}
