#' @title Estimate single-observer line-transect distance function
#' 
#' @description Fits a detection function to off-transect 
#' distances collected by a single observer. 
#' 
#' @inheritParams dfuncEstim
#' 
#' @param formula A standard formula object.  For example, `dist ~ 1`, 
#' `dist ~ covar1 + covar2`). The left-hand side (before `~`)
#' is the name of the vector containing off-transect or radial detection distances.  
#' The right-hand side contains the names of covariate 
#' vectors to fit in the detection
#' function, and potentially group sizes. Group sizes are specified 
#' by including `+ groupsize(<variable>)` in the RHS 
#' (see 'Group Sizes' section).
#' Covariates can be either detection level 
#' or transect level and can appear in  `data` or exist in the 
#' global working environment. Regular R scoping 
#' rules apply.  
#' 
#' 
#' @param likelihood String specifying the likelihood to fit. Built-in 
#' likelihoods at present are "halfnorm", "hazrate", and "negexp". 
#' 
#' @param w.lo Lower or left-truncation limit of the distances in distance data. 
#' This is the minimum possible off-transect distance. Default is 0.  If 
#' `w.lo` is greater than 0, it must have measurement units. 
#' See `help(unitHelpers)` for assistance assigning units.
#'  
#' @param w.hi Upper or right-truncation limit of the distances 
#' in `dist`. This is the maximum off-transect distance that 
#' could be observed. If unspecified (i.e., NULL), 
#' right-truncation is set to the maximum of the observed 
#' distances.  If `w.hi` is specified, it must have  
#' measurement units.  
#' See `help(unitHelpers)` for assistance assigning units.
#' 
#' @param expansions A scalar specifying the number of terms 
#' in `series` to compute. Depending on the series, 
#' this could be 0 through 5.  The default of 0 equates 
#' to no expansion terms of any type.  No expansion terms 
#' are allowed (i.e., `expansions` is forced to 0) if 
#' covariates are present in the detection function 
#' (i.e., right-hand side of `formula` includes
#' something other than `1`). 
#' 
#' @param series If `expansions` > 0, this string 
#' specifies the type of expansion to use. Valid values at 
#' present are 'simple', 'hermite', and 'cosine'. 
#' 
#' @param x.scl The x coordinate (a distance) at which the 
#'   detection function will be scaled.  `g.x.scl` can be a distance
#'   or the string "max".  
#'   When `x.scl` is specified (i.e., not 0 or "max"), it must have measurement 
#'   units assigned. 
#'   See `help(unitHelpers)` for assistance assigning units. 
#'   
#' @param g.x.scl Height of the distance function at coordinate `x`. 
#'   The distance function 
#'   will be scaled so that g(`x.scl`) = `g.x.scl`. 
#'   If `g.x.scl` is not 
#'   a data frame, it must be a numeric value (vector of length 1) 
#'   between 0 and 1. 
#' 
#' @param warn A logical scalar specifying whether to issue 
#' an R warning if the estimation did not converge or if one 
#' or more parameter estimates are at their boundaries.  
#' For estimation, `warn` should generally be left at
#' its default value of `TRUE`.  When computing bootstrap 
#' confidence intervals, setting `warn = FALSE` 
#' turns off annoying warnings when an iteration does 
#' not converge.  Regardless of `warn`, after 
#' completion all messages about 
#' convergence and boundary conditions are printed 
#' by `print.dfunc`, `print.abund`, and 
#' `plot.dfunc`. 
#' 
#' @param outputUnits A string specifying the symbolic measurement 
#' units for results. Valid units are listed in `units::valid_udunits()`.
#' The strings for common distance symbolic units are: 
#' "m" - meters, "ft" - feet, "cm" - centimeters, "mm" - 
#' millimeters, "mi" - miles, "nmile" - 
#' nautical miles ("nm" is nano meters), "in" - inches, 
#' "yd" - yards, "km" - kilometers, "fathom" - fathoms, 
#' "chains" - chains, and "furlong" - furlongs.  
#' If `outputUnits` is unspecified (NULL),
#' output units will be the same as those on 
#' distances in `data`.  
#' 
#' @param asymptoticSE Logical variable for whether to calculate 
#' asymptotic standard errors. The default (TRUE) estimates an
#' asymptotic variance-covariance matrix for parameters based on the 
#' likelihood's Hessian (2nd derivative). If maximization 
#' has been performed by Nlminb or HookesJeeves, the asymptotic 
#' Hessian is estimated using numeric second derivatives 
#' of the likelihood at the maximum likelihood solution. If 
#' maximization was performed by Optim, the last Hessian of 
#' the maximization is returned 
#' by Optim and used
#' (see [varcovarEstim()] and [secondDeriv()]). 
#' Asymptotic standard errors will not be estimated if 
#' `asymptoticSE = FALSE`. If not estimated, 
#' bootstrap iterations will run faster because the numeric Hessian, 
#' which is discarded during bootstrapping,
#' will not be calculated every iteration.
#'
#'
#' @section Group Sizes: 
#' To specify non-unity group sizes,  use `groupsize()`
#' on the RHS of `formula`. When group sizes are not all 1, they must appear in a column 
#' of the 'detections' list-column of `data`. 
#' For example, `d ~ habitat + groupsize(number)` specifies  
#' distances in column `d`, one covariate 
#' named `habitat`, and that column `number` 
#' contains the number of individuals 
#' associated with each detection.  If group sizes are not specified, 
#' all group sizes are assumed to be 1.
#' 
#' @section Contrasts: 
#' Factor contrasts in `Rdistance` are specified 
#' the same way as in `lm` or `glm`. 
#' By default, `Rdistance` uses 
#' contrasts in `getOption("contrasts")`.  To change contrasts, use a statement
#' like `options(contrasts = c(unordered = "contr.SAS", 
#' ordered = "contr.poly"))`.  Or, to set contrasts for a 
#' specific factor in the input data frame, use 
#' `contrasts(df$A) <- "contr.sum"` or similar. 
#' See [contrasts()] or the `contrasts.arg`
#' of [model.matrix()].
#' 
#' @section Transect types: 
#' `Rdistance` accommodates two kinds of transects: continuous and point.  
#' Detections can occur at any point on continuous transects.
#' `Rdistance` calls these 'line-transects' even though routes are not
#' necessarily a straight line.
#' On point transects, detections occur at a series of stops 
#' (points). `Rdisance` calls these point-transects. Transects are the basic 
#' sampling unit in both cases. `Rdistance` assumes each row of `data` 
#' contains information from one transect. See [RdistDf()] for 
#' more details. 
#' 
#' @section Measurement Units: 
#' As of `Rdistance` version 3.0.0, measurement units are 
#' require on all physical distances. 
#' Requiring units ensures that internal calculations and results 
#' (e.g., ESW and abundance) are correct 
#' and that output units are clear.   
#' Physical distances are required on
#' off-transect distances, radial distances, truncation distances 
#' (`w.lo`, unless it is zero; and `w.hi`, unless it is NULL), 
#' scale locations (`x.scl`, unless it is zero),
#' line-transect lengths, and study area size. All units are 
#' 1-dimensional except those on study area, which are 2-dimensional. 
#' 
#' Physical measurement units can vary. For example, 
#' off-transect distances can be meters ("m"), `w.hi` can be inches ("in"), 
#' and `w.lo` can be kilometers ("km").  Internally, all distances are 
#' converted to the units specified by `outputUnits` 
#' (or the units of input distances if 
#' `outputUnits` is NULL), and 
#' all output is reported 
#' in units of `outputUnits`. Valid conversions must exist between 
#' units or an error is thrown.  For example, meters cannot be converted
#' into hectares.
#'   
#' Measurement units can be assigned using one of Rdistance's 
#' unit helper routines (see `help(unitHelpers)`), or from 
#' routines in the `units` package (e.g., 
#' `x <- units::set_units(x, "<units>")`). 
#' See `units::`[units::valid_udunits()]
#' for a list of valid symbolic units. 
#' 
#' If measurements are truly unit-less, or measurement units are unknown, 
#' set `options(Rdist_requireUnits = FALSE)`.  This suppresses 
#' all unit checks and conversions.  Users are on their own 
#' to make sure inputs are scaled correctly and that output units are known. 
#'  
#' @details
#' Optimization and estimation controls can be modified using `options()`. 
#' See [RdistanceControls()].
#' 
#' @return  An object of class 'dfunc' with the following components:
#' 
#'   \item{par}{The vector of estimated parameter values. 
#'     Length of this vector is the sum of the following: 
#'     \enumerate{
#'        \item The number of columns of the design matrix. This equals the 
#'        number of covariates in the distance function plus one for the 
#'        intercept, assuming an intercept is included.
#'        \item The number of constant parameters in the distance function.
#'        Constant parameters are those not related to covariates.  For example, 
#'        the exponent 'k' parameter for hazard rate likelihood, or 
#'        the mixing fraction 'p' for the oneStep likelihood. This can be zero.
#'        \item The number of expansion functions called for.  This equals 
#'        the input `expansions`. 
#'     }
#'   }
#'  
#'   \item{loglik}{The maximized value of the log likelihood.}
#'   
#'   \item{convergence}{The convergence code. This code 
#'     is returned by the optimizing routine (e.g., `optim` or `nlminb`).  
#'     Values other than 0 indicate suspect convergence.}
#'     
#'   \item{message}{If maximization did not converge (`convergence != 0`),
#'     this is the reason given by the optimizing routine.  
#'   }
#'     
#'   \item{varcovar}{The variance-covariance matrix for coefficients 
#'     of the distance function, either estimated by the inverse of 
#'     the fit's Hessian or by bootstrapping.  
#'     If the likelihood is smooth (i.e., those listed by 
#'     `Rdistance:::differentiableLikelihoods())`, 
#'     Rdistance initially estimates the variance-covariance matrix using the 
#'     second derivative of the log likelihood surface 
#'     at the final estimates, where second derivatives are estimated by 
#'     numeric differentiation (by routine [secondDeriv()]. 
#'     The variance-covariance matrix is re-set to NULL 
#'     if the Hessian is not positive-definite.  If bootstrap resampling
#'     has been performed (using `abundEstim`), the variance-covariance
#'     matrix is re-estimated using the bootstrap values of parameters
#'     and automatically reset.  
#'     Error estimates derived from bootstrapping are generally 
#'     preferable to the asymptotic estimates, hence the automatic 
#'     re-set.}   
#'     
#'   \item{limits}{A list containing the lower and upper limits of parameters.}
#'   
#'   \item{evaluations}{The number of likelihood evaluations performed by the 
#'   optimizer.}
#'   
#'   \item{mf}{An R 'model frame' containing the detections (within the strip 
#'   or circle) used in the fit, covariates specified in the formula, 
#'   and groupsizes.  Column 'dist' contains the 
#'   observed distances. The intercept, if included in the model, is not 
#'   included as a column in this model frame. (Test whether an intercept 
#'   is included using `attr(terms(return$mf), "intercept")`). 
#'   Column 'offset(...)' contains group sizes associated with 
#'   the values of 'dist'. Name of the group size column is "offset(...)", 
#'   not "groupsize(...)", so that group sizes can be treated offsets in 
#'   other R routines.  The `$mf` component is a proper `model.frame` and contains
#'   both 'terms' and 'contrasts' attributes. This model frame 
#'   contains only non-missing distances between `w.lo` and `w.hi`. }
#'   
#'   \item{data}{The original nested data frame subset to information required 
#'   to complete distance estimation.  This data frame contains information 
#'   on replication (i.e., rows are sites and are re-sampled during bootstrapping),
#'   missing distances, missing transect lengths, and distances outside the observation 
#'   strip from `w.lo` and `w.hi`. }
#'
#'   \item{formula}{The distance function's formula.}
#'   
#'   \item{dataName}{Name of the original nested data frame.}
#'   
#'   \item{likelihood}{The name of the likelihood fitted to observation 
#'   distances. }
#'     
#'   \item{w.lo}{Left-truncation value used during the fit.}
#'   
#'   \item{w.hi}{Right-truncation value used during the fit.}
#'   
#'   \item{expansions}{The number of expansion terms used 
#'   during the fit.}
#'   
#'   \item{series}{The type of expansion used during estimation. This is 
#'   only relevant if `expansions > 0`.}
#'   
#'   \item{x.scl}{The distance at which 
#'   the function has been scaled to some value.  
#'   This is the *x* at which the distance function 
#'   g(*x*) = `g.x.scl`. } 
#'     
#'   \item{g.x.scl}{The height of the distance function 
#'     at a distance of `x.scl`. }
#'     
#'   \item{outputUnits}{A list of type 'symbolic_units' containing the 
#'   physical measurement units used during estimation. }
#'   
#'   \item{asymptoticSE}{A logical scalar indication whether the 
#'   variance-covariance matrix in component `varcovar` is 
#'   asymptotic (TRUE; estimated from the Hessian) or bootstrap (FALSE; 
#'   estimated by bootsrap resampling).}
#'   
#'   \item{optimizer}{The optimizing routine used.}
#'     
#'   \item{call}{The original function call.}
#'   
#'   \item{nCovars}{The number of exogenous covariates fitted in the 
#'   distance function. Does not include the intercept. }
#'   
#'   \item{LhoodType}{The type of likelihood fitted. Currently, only 'parametric' 
#'   types are fitted.  }
#'     
#'     
#'     
#' @references Buckland, S.T., D.R. Anderson, K.P. Burnham, J.L. Laake, D.L. Borchers,
#'    and L. Thomas. (2001) *Introduction to distance sampling: estimating
#'    abundance of biological populations*. Oxford University Press, Oxford, UK.
#'     
#' @seealso [abundEstim()], [autoDistSamp()].
#' Likelihood-specific help files (e.g., [halfnorm.like()]). 
#' 
#' @examples 
#' # Load example sparrow data (line transect survey type)
#' data(sparrowDf)
#' 
#' dfunc <- dfuncEstim(data = sparrowDf
#'                   , formula = dist ~ 1)
#' dfunc
#' plot(dfunc)                   
#'
#' @export
dE.single <- function( data
                      , formula
                      , likelihood = "halfnorm"
                      , w.lo = setUnits(0,"m")
                      , w.hi = NULL
                      , expansions = 0
                      , series = "cosine"
                      , x.scl = w.lo
                      , g.x.scl = 1
                      , warn = TRUE
                      , outputUnits = NULL
                      , asymptoticSE = TRUE
 ){

  verboseLevel <- getOption("Rdistance_verbosity")
  
  # Parse the formula and make a model list ----
  # all parameters go into parseModel because they need to become
  # components for the output list, not just formula.
  # All checking is done in parseModel(), including 
  # check of units (via checkUnits()).
  
  modelList <- Rdistance::parseModel(data = data
                          , formula = formula
                          , likelihood = likelihood
                          , w.lo = w.lo
                          , w.hi = w.hi
                          , expansions = expansions
                          , series = series
                          , x.scl = x.scl
                          , g.x.scl = g.x.scl
                          , outputUnits = outputUnits
                          , asymptoticSE = asymptoticSE
                        )
  strt.lims <- Rdistance::startLimits(modelList)
  
  if(verboseLevel >= 2){
    cat(colorize("Starting values ----\n", col="red"))
    cat(colorize("   Start: "))
    cat(paste(paste(names(strt.lims$start), "=", colorize(strt.lims$start)), collapse=", "), "\n")
    cat(colorize("Lo Limit: "))
    cat(paste(paste(names(strt.lims$low), "=", colorize(strt.lims$low)), collapse=", "), "\n")
    cat(colorize("Hi Limit: "))
    cat(paste(paste(names(strt.lims$high), "=", colorize(strt.lims$high)), collapse=", "), "\n")
  }

  # Check whether need to use non-gradient optimizer ----
  changedOptions <- setOptimizer(modelList)
  
  # Perform optimization
  fit <- mlEstimates( ml = modelList
                    , strt.lims = strt.lims
                    )

  # Assemble results
  ans <- c(fit, modelList)
  ans$optimizer <- getOption("Rdistance_optimizer")
  class(ans) <- "dfunc"

  # Put original options back if needed ----
  options(changedOptions)

  if ( ans$likelihood != "Gamma" ){
    # not absolutely necessary. 
    # Could estimate these later in print and plot methods.
    # but this saves a little time.
    gx <- gxEstim(ans)
    ans$x.scl <- gx$x.scl
    ans$g.x.scl <- gx$g.x.scl
  } 

  # ---- Check parameter boundaries ----
  fuzz <- getOption("Rdistance_fuzz")
  if (ans$convergence != 0) {
    if (warn) warning(ans$message)
    low.bound <- FALSE
    high.bound <- FALSE
  } else {
    low.bound <- ans$par <= (ans$limits$low + fuzz)
    high.bound <- ans$par >= (ans$limits$high - fuzz)
  }
  if (any(low.bound)) {
    # if we are here, model converged but to limit
    ans$convergence <- -1
    messL <- paste(paste(strt.lims$names[low.bound], "parameter at lower boundary.")
    , collapse = "; ")
    ans$message <- messL
    if (warn) warning(ans$message)
    } 
  else {
    messL <- NULL
  }
  if (any(high.bound)) {
    ans$convergence <- -1
    messH <- paste(paste(strt.lims$names[high.bound], "parameter at upper boundary.")
    , collapse = "; ")
    ans$message <- c(messL, messH)
    if (warn) warning(ans$message)
  }
  
  ans

} # end function
