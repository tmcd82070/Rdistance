#' @title Compute the integration constant for distance density functions
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
#'       \item \samp{covars} = If the density allows covariates, 
#'       the covariate matrix.
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
#' @param series The series to use for expansions. 
#' If \code{expansions} > 0, this string 
#' specifies the type of expansion. Valid values at 
#' present are 'simple', 'hermite', and 'cosine'.
#' 
#' @param expansions Number of expansions in \code{density}.
#'
#' @param pointSurvey Boolean. TRUE if point transect data,
#' FALSE if line transect data.
#'
#'
#' @details The trapezoid rule is used to numerically integrate
#' \code{density} from \code{w.lo} to \code{w.hi}. Two-hundred
#' (200) equal-sized trapezoids are used in the integration.  The number
#' of trapezoids to use is fixed and cannot be changed without
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
#'         Michael Kleinsasser, WEST Inc.,  \email{mkleinsa@uwyo.edu}
#'
#' @seealso \code{\link{dfuncEstim}}, \code{\link{halfnorm.like}}
#'
#' @examples
#' # Can put any number for first argument (1 used here)
#' scl <- integration.constant(dist=1, density=uniform.like, covars = NULL,
#'                             pointSurvey = FALSE, w.lo=0, w.hi = 100,
#'                             expansions = 0, a=c(75,25))
#' print(scl) # Should be 75.1
#'
#' x <- seq(0,100,length=200)
#' y <- uniform.like( c(75,25), x, scale=FALSE ) / scl
#' int.y <- (x[2]-x[1]) * sum(y[-length(y)]+y[-1]) / 2  # the trapezoid rule, should be 1.0
#' print(int.y) # Should be 1
#'
#' @keywords models
#' @importFrom stats integrate
#' @export

integration.constant <- function(dist, 
                                 density, 
                                 a,
                                 covars, 
                                 w.lo, 
                                 w.hi, 
                                 series,
                                 expansions, 
                                 pointSurvey){

  density = match.fun(density)
  seqx = seq(w.lo, w.hi, length=200) # for trapazoid rule when needed

  if(!is.null(covars)){
    # Not sure following is best to do. 
    # It is much faster to de-dup and compute values on just 
    # unique combinations of covariates IF there are large number
    # of duplicated covariates.  This is common when using 
    # factors.  But, if using continuous covariates, this is 
    # inefficient.  Regardless, this code only computes valuse 
    # of the scaling constant for unique combinations of X, 
    # then merges them into the covar array.  The non-de-dupped
    # way to do these calculations is something like 
    # x <- covars %*% matrix(a,ncol=1), which might be just as fast.
    
    Pkey <- tapply(1:nrow(covars), as.data.frame(covars)) # groups of duplicates
    covars <- data.frame(covars, zzzPkey=Pkey)
    dupCovars <- duplicated(covars$zzzPkey)
    unique.covars <- covars[!dupCovars,] 
    PkeyCol <- ncol(unique.covars)
    
    # Remember that unique.covars now has extra column, zzzPkey hanging off the end
    # don't include this colum in calculations below (or set a[last]=0)
    # covars and unique.covars now have Pkey, which we will use to 
    # merge later

    #unique.covars <- unique(covars)
    seqy <- list()
    temp.scaler <- vector(length = nrow(unique.covars))
    scaler <- vector(length = nrow(covars), "numeric")

    if(pointSurvey){
      for(i in 1:nrow(unique.covars)){
        temp.covars <- matrix(as.numeric(unique.covars[i,-PkeyCol]),nrow=length(seqx),ncol=ncol(unique.covars)-1, byrow=TRUE)
        seqy[[i]] <- seqx * density(a = a, dist = seqx, covars = temp.covars,
                    scale = FALSE, w.lo = w.lo, w.hi = w.hi, 
                    expansions = expansions, series=series)
        temp.scaler[i] <- (seqx[2] - seqx[1]) * sum(seqy[[i]][-length(seqy[[i]])] + seqy[[i]][-1]) / 2
      }
    }
    else if(identical(density, halfnorm.like) & expansions == 0){
      s <- as.matrix(unique.covars) %*% matrix(c(a,0),ncol=1)
      sigma <- exp(s)

      # Point is: temp.scaler should be itegral under distance function
      # We happen to know it for halfnorm (and some others below)
      temp.scaler <- 2*(pnorm(w.hi,w.lo,sigma)-0.5) * sqrt(pi/2) * sigma
      
      # We had these statements when Aidan was requiring the pracma package. 
      # for(i in 1:nrow(unique.covars)){
      #   temp.scaler[i] <- sqrt(pi/2) * sigma[i] * (erf(w.hi/(sqrt(2)*sigma[i])) - erf(w.lo/(sqrt(2)*sigma[i])))
      # }
    }
    else if(identical(density, hazrate.like) & expansions == 0){
      s <- as.matrix(unique.covars[,-PkeyCol]) %*% matrix(a[-length(a)],ncol=1)
      sigma <- exp(s)
      beta = a[length(a)]

      for(i in 1:nrow(unique.covars)){
        seqy <- 1 - exp(-(seqx/sigma[i])^(-beta))
        temp.scaler[i] <- (seqx[2] - seqx[1])*sum(seqy[-length(seqy)] + seqy[-1]) / 2
          # integrate(f = function(x){1 - exp(-(x/sigma[i])^(-beta))},lower =  w.lo,
          #                 upper = w.hi, stop.on.error = F)$value
      }
    }
    else if(identical(density, negexp.like) & expansions == 0){
      s <- as.matrix(unique.covars) %*% matrix(c(a,0),ncol=1)
      beta <- exp(s)

      temp.scaler <- unname((exp(-beta*w.lo) - exp(-beta*w.hi))/beta)
      
      # for(i in 1:nrow(unique.covars)){
      #   temp.scaler[i] <- unname((exp(-beta[i]*w.lo) - exp(-beta[i]*w.hi))/beta[i])
      # }

    }
    else {
      # User defined likelihood case.  
      for(i in 1:nrow(unique.covars)){
        # temp.covars <- matrix(unique.covars[i,-PkeyCol],nrow=length(seqx),
        temp.covars <- matrix(unlist(unique.covars[i,-PkeyCol]),nrow=length(seqx),
                              ncol=ncol(unique.covars)-1, byrow=TRUE)
        seqy[[i]] <- density(dist = seqx, covars = temp.covars, 
                             scale = FALSE, w.lo = w.lo, w.hi = w.hi, 
                             a = a, expansions = expansions, 
                             series=series)
        temp.scaler[i] <- (seqx[2] - seqx[1]) * sum(seqy[[i]][-length(seqy[[i]])] + seqy[[i]][-1]) / 2
      }
    }

    df <- data.frame(unique.covars,temp.scaler)

    z <- merge(covars, df, by.x="zzzPkey", by.y="zzzPkey", sort=F)
    scaler <- z$temp.scaler
    if(pointSurvey){
      scaler <- scaler/dist
    }
  }
  else if(pointSurvey){
    seqy <- seqx * density( dist = seqx, scale = FALSE, 
                            w.lo = w.lo, w.hi = w.hi, a = a, 
                            expansions = expansions, series=series)

    #   trapezoid rule
    scaler <- (seqx[2]-seqx[1]) * sum(seqy[-length(seqy)]+seqy[-1]) / (2*dist)
  }
  else{
    seqy <- density( dist = seqx, scale = FALSE, w.lo = w.lo, 
                     w.hi = w.hi, a = a, expansions = expansions, 
                     series=series)

    #   trapezoid rule
    scaler <- (seqx[2]-seqx[1]) * sum(seqy[-length(seqy)]+seqy[-1]) / 2
  }
  #print(scaler)
  scaler
}