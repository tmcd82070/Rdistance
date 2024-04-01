#' @title Compute the integration constant for distance density functions
#'
#' @description Using numerical integration, this function computes
#' the area under a distance function between two limits (\code{w.lo}
#' and \code{w.hi}).
#'
#' @inheritParams nLL
#' 
#' @inheritParams startLimits
#' 
#'
#' @details The trapezoid rule is used to numerically integrate
#' \code{ml$likelihood} from \code{ml$w.lo} to \code{ml$w.hi}. 
#' Two-hundred (200) equal-sized trapezoids are used in the 
#' integration.  The number
#' of trapezoids is fixed and cannot be changed without
#' re-writing this routine.
#'
#' @return A vector of scalars the same length as the 
#' number of distance observations where each value
#' is the area under \code{ml$likelihood} between 
#' \code{w.lo} and \code{w.hi}.
#' This scalar can be used (as a divisor) to scale 
#' likelihood values such that
#' it integrates to 1.0. i.e., if x = density(\ldots), then
#' x / \code{integration.constant(density, \ldots)} will integrate to 1.0.
#'
#' @seealso \code{\link{dfuncEstim}}, \code{\link{halfnorm.like}}
#'
#' @examples
#' set.seed(238642)
#' 
#' d <- rnorm(1000, mean = 0, sd = 40)
#' d <- units::set_units(d[0 <= d], "m")
#' obs <- factor(rbinom(length(d),1,0.5), labels = c("obs1", "obs2"))
#' 
#' # Close to what happens in Rdistance
#' ml <- list(
#'     mf = model.frame(d ~ obs) 
#'   , likelihood = "halfnorm"
#'   , expansions = 0
#'   , w.lo = units::set_units(0, "m")
#'   , w.hi = units::set_units(125, "m")
#' )
#' integration.constant(c(log(40), log(.5)), ml)


# Can put any number for first argument (1 used here)
#' scl <- integration.constant(dist=units::set_units(1,"m")
#'                           , density=logistic.like
#'                           , covars = NULL
#'                           , pointSurvey = FALSE
#'                           , w.lo = units::set_units(0,"m")
#'                           , w.hi = units::set_units(100,"m")
#'                           , expansions = 0
#'                           , a=c(75,25))
#' print(scl) # Should be 75.1
#'
#' x <- units::set_units(seq(0,100,length=200), "m")
#' y <- logistic.like( c(75,25), x, scale=FALSE ) / scl
#' int.y <- (x[2]-x[1]) * sum(y[-length(y)]+y[-1]) / 2  # the trapezoid rule, should be 1.0
#' print(int.y) # Should be 1
#'
#' @keywords models
#' @importFrom stats integrate
#' @export

integrationConstant <- function(a, ml){

  fx <- match.fun(paste( ml$likelihood, ".like", sep=""))
  dist <- Rdistance::distances(ml$mf)
  X <- model.matrix(ml)
  w.lo <- ml$w.lo
  w.hi <- ml$w.hi

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

  # For case is for LINES, COVARS, LIKE in {Logistic, User, Gamma} 
  # and ALL LIKELIHOODS with expansions > 0
  #
  # We could do all likelihoods this way (i.e., numerical integration); but,
  # the above special cases are faster (I think) and more accurate in some cases because
  # we know the theoretical integral (i.e., for normal and exponential)
  #
  # Because we have covariates always, we need to integrate
  # once per row of dist. i.e., each dist could be coming from 
  # a separate distance function defined by separate parameters.
  
  seqx = seq(w.lo, w.hi, length=nTrapazoids) 

  # Find covariate classes ----
  # Rep the integration sequence over covariate rows.
  # i.e., each x sequence will be associated with one set of 
  # covariates from one row. This makes a big data frame.
  uniqueX <- data.frame(X) |>
    dplyr::mutate(.covClsRow = dplyr::row_number()) |> 
    dplyr::distinct( dplyr::across(-.covClsRow)
                     , .keep_all = TRUE )

  covClsMap <- data.frame(X) |> 
    dplyr::mutate(.rowNumber = dplyr::row_number()) |> 
    dplyr::group_by(dplyr::across(-.rowNumber)) |> 
    dplyr::mutate( .covClsRow = dplyr::first(.rowNumber)) |> 
    dplyr::ungroup() |> 
    dplyr::select(.rowNumber, .covClsRow)
  
  
  # Internal function to evaluate key ----
  # Eval fx for each set of constant covariates (rowid)
  # This is a function to apply density to each row of covariates
  # designed for dplyr::group_modify()
  evalKey <- function(covs
                      , .y
                      , a
                      , Seqx
                      , pointSurvey
                      ){
    # covars are constant across all rows of Seqx
    covs <- matrix(as.numeric(covs), nrow = 1)
    seqy <- fx(
        a = a
      , dist = Seqx
      , covars = covs
    )$key

    if(ml$expansions > 0){
      # 'if' saves a little compute time if no expansions
      # but not necc. b/c expansionTerms = 1 if none
      key <- key * expansionTerms(a, mf)
    }
    
    if(pointSurvey){
        key <- units::drop_units(Seqx) * key 
    }
      
    scaler <- units::drop_units(Seqx[2] - Seqx[1]) * 
      sum(seqy[-length(seqy)] + seqy[-1]) / 2
    
    data.frame(scaler = scaler)
  }
  
  # Call internal evalKey function using group_modify ----
  scalers <- uniqueX |> 
    dplyr::group_by(.covClsRow) |> 
    dplyr::group_modify(.f = evalKey
                        , a = a
                        , Seqx = seqx)
  
  # This join expands from unique covariate classes
  # to the original size of covars
  scalers <- covClsMap |> 
    dplyr::left_join(scalers, by = ".covClsRow") |> 
    dplyr::arrange(.rowNumber) |>   # probably not necc. to sort
    dplyr::pull(scaler)

   # } else if( pointSurvey ){
  #   # This case is POINTS - NO Covariates
  #   seqx = seq(w.lo, w.hi, length=nTrapazoids) 
  #   seqy <- units::drop_units(seqx) * density( dist = seqx, scale = FALSE, 
  #                           w.lo = w.lo, w.hi = w.hi, a = a, 
  #                           expansions = expansions, series=series)
  # 
  #   #   trapezoid rule
  #   scaler <- units::drop_units(seqx[2]-seqx[1]) * sum(seqy[-length(seqy)]+seqy[-1]) / (2*units::drop_units(dist))
  # } else {
  #   # This case is LINES - NO Covariates
  #   # Density should return unit-less numbers (height of density function)
  #   seqx = seq(w.lo, w.hi, length=nTrapazoids) 
  #   seqy <- density( dist = seqx
  #                  , scale = FALSE
  #                  , w.lo = w.lo
  #                  , w.hi = w.hi
  #                  , a = a
  #                  , expansions = expansions
  #                  , series=series
  #                  )
  # 
  #   #   trapezoid rule
  #   scaler <- units::drop_units(seqx[2]-seqx[1]) * sum(seqy[-length(seqy)]+seqy[-1]) / 2
  # }
  # 
  # there are cases where the guess at parameters is so bad, that the integration
  # constant is 0 (consider pnorm(100,0,2e30)). But, we don't want to return 0
  # because it goes in denominator of likelihood and results in Inf, which is
  # not informative.  nlminb guesses NaN after that sometimes. We want to return
  # the smallest possible number that does not result in log(x) = -Inf.
  # Because of the negative applied in nLL function we actually want to return
  # the largest possible numbers such that when we sum them and others we don't get Inf

  if( any(indZeros <- is.na(scalers) |
               is.infinite(scalers) |
               is.nan(scalers) |
               (scalers <= getOption("Rdistance_fuzz"))) ){
    scalers[ indZeros ] <- getOption("Rdistance_posInf")
  }
  
  # # cat(paste("\tscaler = \n\t", paste(scaler, collapse = ", "), "\n"))
  # 
  scalers
}