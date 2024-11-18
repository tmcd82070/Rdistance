#' @title integrationConstant - Area under detection functions
#'
#' @description Compute
#' the area under a distance function between two limits (\code{w.lo}
#' and \code{w.hi}) using numerical integration
#'
#' @inheritParams nLL
#' 
#' @inheritParams startLimits
#' 
#'
#' @details This routine uses Simpson's composite 1/3 rule 
#' to numerically integrate
#' \code{ml$likelihood} from \code{ml$w.lo} to \code{ml$w.hi}
#' (https://en.wikipedia.org/wiki/Simpson's_rule). 
#' The distance function is evaluated at 201 equal-spaced locations between 
#' the limits, {f(x0), f(x1), ..., f(200), f(201)}.  Simpson's composite
#' approximation to the area under the curve is
#' \deqn{\frac{1}{3}h(f(x_0) + 4f(x_1) + 2f(x_2) + 
#'      4f(x_3) + 2f(x_4) + ... + 2f(x_{199}) + 
#'      4f(x_{200}) + f(x_{201}))}{(1/3)h(f(x0) + 4f(x1) + 2f(x2) + 
#'      4f(x3) + 2f(x4) + ... + 2f(x199) + 4f(x200) + f(x201))}
#' where \eqn{h} is the interval size (w.hi - w.lo) / 201.
#' 
#' @inheritSection ESW Numeric Integration
#'
#' @return A vector of scalars the same length as the 
#' number of distance observations where each value
#' is the area under \code{ml$likelihood} between 
#' \code{w.lo} and \code{w.hi}.
#' This scalar can be used (as a divisor) to scale 
#' likelihood values such that
#' the functions integrate to 1.0. i.e., if x = density(\ldots), then
#' x / \code{integration.constant(density, \ldots)} will integrate to 1.0.
#'
#' @seealso \code{\link{dfuncEstim}}, \code{\link{halfnorm.like}}
#'
#' @examples
#' # Close to what happens in Rdistance
#' d <- units::set_units(rep(1,4),"m") # Only units needed, not values
#' obs <- factor(rep(c("obs1", "obs2"), 2))
#' ml <- list(
#'     mf = model.frame(d ~ obs) 
#'   , likelihood = "halfnorm"
#'   , expansions = 0
#'   , w.lo = units::set_units(0, "m")
#'   , w.hi = units::set_units(125, "m")
#'   , outputUnits = units(units::set_units(1,"m"))
#'   , transType = "line"
#' )
#' class(ml) <- "dfunc"
#' integrationConstant(c(log(40), log(.5)), ml)
#' 
#' # Check:
#' w.hi <- 125
#' w.lo <- 0
#' s1 <- 40
#' s2 <- exp(log(s1) + log(0.5))
#' obs1Scaler <- (pnorm(w.hi, mean=w.lo, sd = s1) - 0.5) * sqrt(2*pi)*s1
#' obs2Scaler <- (pnorm(w.hi, mean=w.lo, sd = s2) - 0.5) * sqrt(2*pi)*s2
#' c(obs1Scaler, obs2Scaler)
#' 
#'
#' @keywords models
#' @export

integrationConstant <- function(a, ml){

  fx <- utils::getFromNamespace(paste0( ml$likelihood, ".like"), "Rdistance")
  distUnits <- ml$outputUnits
  X <- model.matrix(ml)
  w.lo <- ml$w.lo
  w.hi <- ml$w.hi

  # We need w.lo, w.hi, and dist to have same units. 
  # This is important because we occasionally drop units in integral calculations below. 
  # I cannot think of a case where units(w.lo) != units(dist), 
  # but just in case...
  
  if( units(w.lo) != distUnits){
    w.lo <- units::set_units(w.lo, distUnits, mode = "standard")
  }
  if( units(w.hi) != distUnits){
    w.hi <- units::set_units(w.hi, distUnits, mode = "standard")
  }
  
  # Now, we can safely compute sequence of x values for numerical integration.
  # This is done below, in each case where its needed. 
  # Because we have covariates always, we need to integrate
  # once per row of dist. i.e., each dist could be coming from 
  # a separate distance function defined by separate parameters.
  
  
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
  
  # Numerical integration coefficients ----
  # Here, use composite Simpson's 1/3 rule
  nEvalPts <- Rdistance:::checkNEvalPts(getOption("Rdistance_intEvalPts")) 
  nInts <- nEvalPts - 1 # this will be even if nEvalPts is odd
  seqx = seq(w.lo, w.hi, length=nEvalPts) 
  deltax <- units::set_units(seqx[2] - seqx[1], NULL)  # or (w.hi - w.lo) / (nInts)

  # Simpson's rule coefficients on f(x0), f(x1), ..., f(x(nEvalPts))  
  intCoefs <- rep( c(2,4), (nInts/2) )
  intCoefs[1] <- 1
  intCoefs <- c(intCoefs, 1)
  
  # Trapazoid rule would be 
  # intCoefs <- rep( 2, nEvalPts )
  # intCoefs[c(1,nEvalPts)] <- 1

  # Internal function to evaluate key ----
  # Eval fx for each set of constant covariates (rowid)
  # This is a function to apply density to each row of covariates
  # designed for dplyr::group_modify()
  evalKey <- function(covs
                      , .y
                      , a
                      , Seqx
                      , ml
                      , intCoefs
                      ){
    # covars are constant across all rows of Seqx
    covs <- matrix(as.numeric(covs), nrow = 1)
    seqy <- fx(
        a = a
      , dist = Seqx
      , covars = covs
    )$L.unscaled

    if(ml$expansions > 0){
      # 'if' saves a little compute time if no expansions
      # but not necc. b/c expansionTerms = 1 if none
      seqy <- seqy * Rdistance::expansionTerms(a = a
                                             , d = Seqx
                                             , series = ml$series
                                             , nexp = ml$expansions
                                             , w = ml$w.hi - ml$w.lo)
    }
    
    if(is.points(ml)){
      seqy <- units::set_units(Seqx, NULL) * seqy 
    }
 
    # Apply numeric integration coefficients
    # fy <-  h*sum(seqy[-length(seqy)] + seqy[-1]) / 2
    fy <- sum(intCoefs * seqy)
    
    data.frame(integral = fy)
  }
  
  # Call internal evalKey function using group_modify ----
  scalers <- uniqueX |> 
    dplyr::group_by(.covClsRow) |> 
    dplyr::group_modify(.f = evalKey
                        , a = a
                        , Seqx = seqx
                        , ml = ml
                        , intCoefs = intCoefs)
  
  # Do not forget to multiply by interval size and constant
  scalers <- scalers |> 
    dplyr::mutate(integral = deltax * integral / 3 )
    # dplyr::mutate(integral = deltax * integral / 2 ) # if Trapazoid
  
  
  # This join expands from unique covariate classes
  # to the original size of covars
  scalers <- covClsMap |> 
    dplyr::left_join(scalers, by = ".covClsRow") |>
    dplyr::pull(integral)

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