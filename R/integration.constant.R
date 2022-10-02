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
  
  # We need w.lo, w.hi, and dist to have same units. 
  # This is important because we occasionally drop units in integral calculations below. 
  # I cannot think of a case where units(w.lo) != units(dist), 
  # but just in case...
  if( units(w.lo) != units(dist)){
    w.lo <- units::set_units(w.lo, units(dist), mode = "standard")
  }
  if( units(w.hi) != units(dist)){
    w.hi <- units::set_units(w.hi, units(dist), mode = "standard")
  }
  
  # Now, we can safely compute sequence of x values for numerical integration.
  # This is done below, in each case where its needed. 
  
  nTrapazoids <- 200 # number of evaluation points in numerical integration
  
  if(!is.null(covars)){
    # Not sure following is best to do. 
    # It is much faster to de-dup and compute values on just 
    # unique combinations of covariates IF there are large number
    # of duplicated covariates.  This is common when using 
    # factors.  But, if using continuous covariates, this is 
    # inefficient.  Regardless, this code only computes values 
    # of the scaling constant for unique combinations of X, 
    # then merges them into the covar array.  The non-de-dupped
    # way to do these calculations is something like 
    # x <- covars %*% matrix(a,ncol=1), which might be just as fast.
    
    Pkey <- tapply(1:nrow(covars), as.data.frame(covars)) # groups of duplicates
    covars <- data.frame(covars, zzzPkey=Pkey)
    dupCovars <- duplicated(covars$zzzPkey)
    unique.covars <- covars[!dupCovars,] 
    PkeyCol <- ncol(unique.covars)
    
    # Remember that unique.covars now has extra column, zzzPkey 
    # don't include this column in calculations below (or set a[last]=0)
    # covars and unique.covars now have Pkey, which we will use to 
    # merge later

    seqy <- list()
    temp.scaler <- vector(length = nrow(unique.covars))
    scaler <- vector(length = nrow(covars), "numeric")

    if(pointSurvey){
      # This case is POINTS, COVARS, all Likelihoods
      seqx = seq(w.lo, w.hi, length=nTrapazoids) 
      for(i in 1:nrow(unique.covars)){
        temp.covars <- matrix(as.numeric(unique.covars[i,-PkeyCol])
                            , nrow = length(seqx)
                            , ncol = ncol(unique.covars)-1
                            , byrow=TRUE)
        seqy[[i]] <- units::drop_units(seqx) * density(a = a
                                  , dist = seqx
                                  , covars = temp.covars
                                  , scale = FALSE
                                  , w.lo = w.lo
                                  , w.hi = w.hi
                                  , expansions = expansions
                                  , series=series
                                  )
        temp.scaler[i] <- units::drop_units(seqx[2] - seqx[1]) * sum(seqy[[i]][-length(seqy[[i]])] + seqy[[i]][-1]) / 2
      }

    } else if(identical(density, halfnorm.like) & expansions == 0){
      # this case is LINES, COVARS, HALFNORM, NO EXPANSIONS
      # Made this a case because I think it's faster, because we know integral
      s <- as.matrix(unique.covars) %*% matrix(c(a,0),ncol=1)
      sigma <- exp(s)  # link function here

      # temp.scaler should be integral under distance function
      # We happen to know it for halfnorm (and some others below)
      # Integrals are by defn unit-less; but, pnorm returns units. Drop apriori.
      # We evaluate normal with mean w.lo, sd = sigma, from -Inf to w.hi, then
      # subtract 0.5 from result for the area to left of mean (w.lo) 
      temp.scaler <- (pnorm(units::drop_units(w.hi)
                          , units::drop_units(w.lo)
                          , sigma) - 0.5) * 
                      sqrt(2*pi) * sigma
      
    } else if(identical(density, hazrate.like) & expansions == 0){
      # This case is LINES, HAZRATE, COVARS, NO EXPANSIONS
      #
      # Integral of hazrate involves incomplete gamma functions. 
      # See wolfram.  Incomplete gammas are implemented in some packages, e.g., 
      # expint.  You could convert to an exact integral using one of these 
      # packages.  But, for now, numerically integrate.
      seqx = seq(w.lo, w.hi, length=nTrapazoids) 
      beta <- a[-length(a)]
      K <- a[length(a)]
      s <- as.matrix(unique.covars[,-PkeyCol]) %*% matrix(beta,ncol=1)
      sigma <- exp(s)  # link function here

      temp.scaler <- sapply(sigma 
                      , FUN = function(s, Seqx, KK){ 
                        seqy <- 1 - exp(-(Seqx/s)^(-KK))
                        scaler <- (Seqx[2] - Seqx[1])*sum(seqy[-length(seqy)] + seqy[-1]) / 2
                        scaler }
                      , Seqx = units::drop_units(seqx)
                      , KK = K)
    } else if(identical(density, negexp.like) & expansions == 0){
      # This case is LINES, NEGEXP, COVARS, NO EXPANSIONS
      s <- as.matrix(unique.covars) %*% matrix(c(a,0),ncol=1)
      beta <- exp(s)
      temp.scaler <- unname((exp(-beta * units::drop_units(w.lo)) - 
                               exp(-beta * units::drop_units(w.hi)))/beta)
    } else {
      # For case is for LINES, COVARS, LIKE in {Logistic, User, Gamma} 
      # and ALL LIKELIHOODS with expansions > 0
      #
      # We could do all likelihoods this way (i.e., numerical integration); but,
      # the above special cases are faster (I think) and more accurate in some cases because
      # we know the theoretical integral (i.e., for normal and exponential)
      seqx = seq(w.lo, w.hi, length=nTrapazoids) 

      # function to apply density to each row of covariates
      likeApply <- function(covs
                          , Seqx
                          , W.lo 
                          , W.hi 
                          , A 
                          , Expansions 
                          , Series  
                          ){
        # Matrix of constant covariates for this case
        temp.covars <- matrix(covs
                              , nrow = length(Seqx)
                              , ncol = length(covs)
                              , byrow = TRUE
        )
        seqy <- density(dist = Seqx
                             , covars = temp.covars
                             , scale = FALSE
                             , w.lo = W.lo
                             , w.hi = W.hi
                             , a = A
                             , expansions = Expansions
                             , series = Series 
        )        
        scaler <- units::drop_units(Seqx[2] - Seqx[1])*sum(seqy[-length(seqy)] + seqy[-1]) / 2
        scaler
      }
     
      temp.scaler <- apply(X = unique.covars[, -PkeyCol, drop = FALSE] 
                         , MARGIN = 1
                         , FUN = likeApply
                         , Seqx = seqx
                         , W.lo = w.lo 
                         , W.hi = w.hi
                         , A = a
                         , Expansions = expansions 
                         , Series = series
                         )
      
    }

    df <- data.frame(unique.covars, temp.scaler)

    z <- merge(covars, df, by.x="zzzPkey", by.y="zzzPkey", sort=F)
    scaler <- z$temp.scaler
    if(pointSurvey){
      scaler <- scaler/units::drop_units(dist)
    }
  } else if( pointSurvey ){
    # This case is POINTS - NO Covariates
    seqx = seq(w.lo, w.hi, length=nTrapazoids) 
    seqy <- units::drop_units(seqx) * density( dist = seqx, scale = FALSE, 
                            w.lo = w.lo, w.hi = w.hi, a = a, 
                            expansions = expansions, series=series)

    #   trapezoid rule
    scaler <- units::drop_units(seqx[2]-seqx[1]) * sum(seqy[-length(seqy)]+seqy[-1]) / (2*units::drop_units(dist))
  } else {
    # This case is LINES - NO Covariates
    # Density should return unit-less numbers (height of density function)
    seqx = seq(w.lo, w.hi, length=nTrapazoids) 
    seqy <- density( dist = seqx
                   , scale = FALSE
                   , w.lo = w.lo
                   , w.hi = w.hi
                   , a = a
                   , expansions = expansions
                   , series=series
                   )

    #   trapezoid rule
    scaler <- units::drop_units(seqx[2]-seqx[1]) * sum(seqy[-length(seqy)]+seqy[-1]) / 2
  }
  
  # there are cases where the guess at parameters is so bad, that the integration
  # constant is 0 (consider pnorm(100,0,2e30)). But, we don't want to return 0
  # because it goes in denominator of likelihood and results in Inf, which is 
  # not informative.  nlminb guesses NaN after that sometimes. We want to return 
  # the smallest possible number that does not result in log(x) = -Inf.
  # Because of the negative applied in nLL function we actually mant to return
  # the largest possible numbers such that when we sum them and others we don't get Inf

  if( any(indZeros <- is.na(scaler) | 
               is.infinite(scaler) | 
               is.nan(scaler) |
               (scaler <= .Machine$double.xmin)) ){
    scaler[ indZeros ] <- .Machine$double.xmax / sum(indZeros)
  }
  
  # cat(paste("\tscaler = \n\t", paste(scaler, collapse = ", "), "\n"))
  
  scaler
}