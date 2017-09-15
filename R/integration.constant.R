#' @name integration.constant
#' @aliases integration.constant
#' 
#' @title Compute the integration constant for distance density functions.
#' 
#' @description Using numerical integration, this function computes 
#' the area under a distance function between two limits (\code{w.lo} 
#' and \code{w.hi}).
#'   
#' @param dist Vector of detection distance values.
#' 
#' @param density A likelihood function for which the 
#' integration constant is sought. This function 
#' must be capable of evaluating values between \code{w.lo} 
#' and \code{w.hi} and have the following parameters: 
#'     \itemize{
#'       \item \samp{a} = Parameter vector.
#'       \item \samp{dist} = Vector of distances.
#'       \item \samp{w.lo} = Lower limit or left truncation value.
#'       \item \samp{w.hi} = Upper limit or right truncation value.
#'       \item \samp{series} = Form of the series expansions, if any.
#'       \item \samp{expansions} = Number of expansion terms.
#'       \item \samp{scale} = Whether to scale function to integrate to 1.
#'     }
#'     
#' @param w.lo The lower limit of integration, or the left truncation 
#' value for perpendicular distances.
#' 
#' @param w.hi The upper limit of integration, or the right truncation 
#' value for perpendicular distances.
#' 
#' @param covars Matrix of covariate values.
#' 
#' @param a Vector of parameters to pass to \code{density}.
#' 
#' @param expansions Number of expansions in \code{density}.
#' 
#' @param pointSurvey Boolean. TRUE if point transect data, 
#' FALSE if line transect data.
#' 
#' @param \dots Additional parameters to the likelihood 
#' function \code{density}.
#' 
#' @details The trapazoid rule is used to numerically integrate
#' \code{density} from \code{w.lo} to \code{w.hi}. Two-hundred 
#' (200) equal-sized trapazoids are used in the integration.  The number 
#' of trapazoids to use is fixed and cannot be changed without 
#' re-writing this routine.
#' 
#' @return A scalar (or vector of scalars if covariates are present) 
#' that is the area under \code{density} between \code{w.lo} and \code{w.hi}. 
#' This scalar can be used as a divisor to scale density such that 
#' it integrates to 1.0. If x = density(\ldots), then 
#' x / \code{integration.constant(density, \ldots)} will integrate to 1.0.
#' 
#' @author Trent McDonald, WEST Inc.,  \email{tmcdonald@west-inc.com}
#'         Aidan McDonald, WEST Inc.,  \email{aidan@mcdcentral.org}
#'         
#' @seealso \code{\link{dfuncEstim}}, \code{\link{halfnorm.like}}
#' 
#' @examples 
#' #   The following result should be approximately 75
#' scl <- integration.constant(uniform.like, w.lo=0, w.hi=100, a=c(75,25))
#' print(scl)
#'   
#' #   Check that likelihood / scl integrates to 1.0
#' x <- seq(0,100,length=200)
#' y <- uniform.like( c(75,25), x, scale=FALSE ) / scl
#' int.y <- (x[2]-x[1]) * sum(y[-length(y)]+y[-1]) / 2  # the trapazoid rule, should be 1.0
#' print(int.y)
#' 
#' @keywords models
#' @importFrom pracma erf
#' @export

integration.constant <- function(dist, density, w.lo, w.hi, covars, a, 
                                 expansions, pointSurvey, ...){

  density = match.fun(density)
  seqx = seq(w.lo, w.hi, length=200)
  
  if(!is.null(covars)){
    unique.covars <- unique(covars)
    temp.covars <- matrix(nrow = length(seqx), ncol = ncol(unique.covars))
    seqy <- list()
    temp.scaler <- vector(length = nrow(unique.covars))
    scaler <- vector(length = nrow(covars), "numeric")
    
    if(pointSurvey){
      for(i in 1:nrow(unique.covars)){
        for(j in 1:length(seqx)){
          temp.covars[j,] <- unique.covars[i,]
        }
        seqy[[i]] <- seqx * density(dist = seqx, covars = temp.covars, 
                    scale = FALSE, w.lo = w.lo, w.hi = w.hi, a = a, 
                    expansions = expansions, ...)
        temp.scaler[i] <- (seqx[2] - seqx[1]) * sum(seqy[[i]][-length(seqy[[i]])] + seqy[[i]][-1]) / 2
      }
    }
    else if(identical(density, halfnorm.like) & expansions == 0){
      s <- 0
      for (j in 1:(ncol(covars)))
        s <- s + a[j]*unique.covars[,j]
      sigma <- exp(s)
      
      for(i in 1:nrow(unique.covars)){
        temp.scaler[i] <- sqrt(pi/2) * sigma[i] * (erf(w.hi/(sqrt(2)*sigma[i])) - erf(w.lo/(sqrt(2)*sigma[i])))
      }
    }
    else if(identical(density, hazrate.like) & expansions == 0){
      s <- 0
      for (i in 1:(ncol(covars)))
        s <- s + a[i]*unique.covars[,i]
      sigma <- exp(s)
      beta = a[length(a) - expansions]
      
      for(i in 1:nrow(unique.covars)){
        temp.scaler[i] <- integrate(f = function(x){1 - exp(-(x/sigma[i])^(-beta))},lower =  w.lo, 
                          upper = w.hi, stop.on.error = F)$value
      }
    }
    else if(identical(density, negexp.like) & expansions == 0){
      s <- 0
      for (i in 1:(ncol(covars)))
        s <- s + a[i]*unique.covars[,i]
      beta <- exp(s)
      
      for(i in 1:nrow(unique.covars)){
        temp.scaler[i] <- unname((exp(-beta[i]*w.lo) - exp(-beta[i]*w.hi))/beta[i])
      }
    }
    else{
      # not sure about this case.  When does it happen?  Is this needed?
      for(i in 1:nrow(unique.covars)){
        for(j in 1:length(seqx)){
          temp.covars[j,] <- unique.covars[i,]
        }
        seqy[[i]] <- density(dist = seqx, covars = temp.covars, scale = FALSE, w.lo = w.lo, w.hi = w.hi, a = a, expansions = expansions, ...)
        temp.scaler[i] <- (seqx[2] - seqx[1]) * sum(seqy[[i]][-length(seqy[[i]])] + seqy[[i]][-1]) / 2
      }
    }
    
    df <- data.frame(unique.covars,temp.scaler)
    z <- merge(covars, df, by.x = names(as.data.frame(covars)), by.y = names(df[, names(df) != "temp.scaler"]), sort = F)
    scaler <- z$temp.scaler
    if(pointSurvey){
      scaler <- scaler/dist
    }
  }
  else if(pointSurvey){
    seqy <- seqx * density( dist = seqx, scale = FALSE, w.lo = w.lo, w.hi = w.hi, a = a, expansions = expansions, ...)
    
    #   Trapazoid rule
    scaler <- (seqx[2]-seqx[1]) * sum(seqy[-length(seqy)]+seqy[-1]) / (2*dist)
  }
  else{
    seqy <- density( dist = seqx, scale = FALSE, w.lo = w.lo, w.hi = w.hi, a = a, expansions = expansions, ...)
    
    #   Trapazoid rule
    scaler <- (seqx[2]-seqx[1]) * sum(seqy[-length(seqy)]+seqy[-1]) / 2
  }
  #print(scaler)
  scaler
}