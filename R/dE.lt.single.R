#' @title dE.lt.single - Estimate single-observer line-transect distance function
#' 
#' @description Fits a detection function to off-transect 
#' distances collected by a single observer. 
#' 
#' @param formula A standard formula object.  For example, \code{dist ~ 1}, 
#' \code{dist ~ covar1 + covar2}). The left-hand side (before \code{~})
#' is the name of the vector containing off-transect distances.  
#' The right-hand side contains the names of covariate 
#' vectors to fit in the detection
#' function. Covariates can be either detection level 
#' or transect level and can appear in  \code{data} or exist in the 
#' global working environment. Regular R scoping 
#' rules apply.  
#' 
#' 
#' @param likelihood String specifying the likelihood to fit. Built-in 
#' likelihoods at present are "logistic", "halfnorm", 
#' "hazrate", "negexp", and "Gamma". 
#' 
#' @param w.lo Lower or left-truncation limit of the distances in distance data. 
#' This is the minimum possible off-transect distance. Default is 0.  If 
#' \code{w.lo} is greater than 0, it must be assigned measurement units
#' using \code{units(w.lo) <- "<units>"} or 
#' \code{w.lo <- units::set_units(w.lo, "<units>")}. 
#' See examples in the help for \code{set_units}.
#'  
#' @param w.hi Upper or right-truncation limit of the distances 
#' in \code{dist}. This is the maximum off-transect distance that 
#' could be observed. If unspecified (i.e., NULL), 
#' right-truncation is set to the maximum of the observed 
#' distances.  If \code{w.hi} is specified, it must have associated 
#' measurement units.  Assign measurement units
#' using \code{units(w.hi) <- "<units>"} or 
#' \code{w.hi <- units::set_units(w.hi, "<units>")}. 
#' See examples in the help for \code{set_units}. 
#' 
#' @param expansions A scalar specifying the number of terms 
#' in \code{series} to compute. Depending on the series, 
#' this could be 0 through 5.  The default of 0 equates 
#' to no expansion terms of any type.  No expansion terms 
#' are allowed (i.e., \code{expansions} is forced to 0) if 
#' covariates are present in the detection function 
#' (i.e., right-hand side of \code{formula} includes
#' something other than \code{1}). 
#' 
#' @param series If \code{expansions} > 0, this string 
#' specifies the type of expansion to use. Valid values at 
#' present are 'simple', 'hermite', and 'cosine'. 
#' 
#' @param x.scl The x coordinate (a distance) at which the 
#'   detection function will be scaled.  \code{g.x.scl} can be a distance
#'   or the string "max".  
#'   When \code{x.scl} is specified (i.e., not 0 or "max"), it must have measurement 
#'   units assigned using either \code{library(units);units(x.scl) <- '<units>'}
#'   or \code{x.scl <- units::set_units(x.scl, <units>)}. See
#'   \code{units::valid_udunits()} for valid symbolic units. 
#'   
#' @param g.x.scl Height of the distance function at coordinate \code{x}. 
#'   The distance function 
#'   will be scaled so that g(\code{x.scl}) = \code{g.x.scl}. 
#'   If \code{g.x.scl} is not 
#'   a data frame, it must be a numeric value (vector of length 1) 
#'   between 0 and 1. 
#' 
#' @param warn A logical scalar specifying whether to issue 
#' an R warning if the estimation did not converge or if one 
#' or more parameter estimates are at their boundaries.  
#' For estimation, \code{warn} should generally be left at
#' its default value of \code{TRUE}.  When computing bootstrap 
#' confidence intervals, setting \code{warn = FALSE} 
#' turns off annoying warnings when an iteration does 
#' not converge.  Regardless of \code{warn}, after 
#' completion all messages about 
#' convergence and boundary conditions are printed 
#' by \code{print.dfunc}, \code{print.abund}, and 
#' \code{plot.dfunc}. 
#' 
#' @param outputUnits A string specifying the symbolic measurement 
#' units for results. Valid units are listed in \code{units::valid_udunits()}.
#' The strings for common distance symbolic units are: 
#' "m" - meters, "ft" - feet, "cm" - centimeters, "mm" - 
#' millimeters, "mi" - miles, "nmile" - 
#' nautical miles ("nm" is nano meters), "in" - inches, 
#' "yd" - yards, "km" - kilometers, "fathom" - fathoms, 
#' "chains" - chains, and "furlong" - furlongs.  
#' If \code{outputUnits} is unspecified (NULL),
#' output units will be the same as those on 
#' distances in \code{data}.  
#'
#'
#' @section Group Sizes: 
#' To specify non-unity group sizes,  use \code{groupsize()}
#' on the RHS of \code{formula}. When group sizes are not all 1, they must appear in a column 
#' of the 'detections' list-column of \code{data}. 
#' For example, \code{d ~ habitat + groupsize(number)} specifies  
#' distances in column \code{d}, one covariate 
#' named \code{habitat}, and that column \code{number} 
#' contains the number of individuals 
#' associated with each detection.  If group sizes are not specified, 
#' all group sizes are assumed to be 1.
#' 
#' @section Contrasts: 
#' Factor contrasts in \code{Rdistance} are specified 
#' the same way as in \code{lm} or \code{glm}. 
#' By default, \code{Rdistance} uses 
#' contrasts in \code{getOption("contrasts")}.  To change contrasts, use a statement
#' like \code{options(contrasts = c(unordered = "contr.SAS", 
#' ordered = "contr.poly"))}.  Or, to set contrasts for a 
#' specific factor in the input data frame, use 
#' \code{contrasts(df$A) <- "contr.sum"} or similar. 
#' See \code{\link{contrasts}} or the \code{contrasts.arg}
#' of \code{\link{model.matrix.default}}.
#' 
#' @section Transect types: 
#' \code{Rdistance} accommodates two kinds of transects: continuous and point.  
#' Detections can occur at any point on continuous transects.
#' \code{Rdistance} calls these 'line-transects' even though routes are not
#' necessarily a straight line.
#' On point transects, detections occur at a series of stops 
#' (points). \code{Rdisance} calls these point-transects. Transects are the basic 
#' sampling unit in both cases. \code{Rdistance} assumes each row of \code{data} 
#' contains information from one transect. See \code{\link{RdistDf}} for 
#' more details. 
#' 
#' @section Measurement Units: 
#' As of \code{Rdistance} version 3.0.0, measurement units are 
#' require on all physical distances. 
#' Requiring units ensures that internal calculations and results 
#' (e.g., ESW and abundance) are correct 
#' and that output units are clear.   
#' Physical distances are required on
#' off-transect distances, radial distances, truncation distances 
#' (\code{w.lo}, unless it is zero; and \code{w.hi}, unless it is NULL), 
#' scale locations (\code{x.scl}, unless it is zero),
#' line-transect lengths, and study area size. All units are 
#' 1-dimensional except those on study area, which are 2-dimensional. 
#' 
#' Physical measurement units can vary. For example, 
#' off-transect distances can be meters ("m"), \code{w.hi} can be inches ("in"), 
#' and \code{w.lo} can be kilometers ("km").  Internally, all distances are 
#' converted to the units specified by \code{outputUnits} 
#' (or the units of input distances if 
#' \code{outputUnits} is NULL), and 
#' all output is reported 
#' in units of \code{outputUnits}. Valid conversions must exist between 
#' units or an error is thrown.  For example, meters cannot be converted
#' into hectares.
#'   
#' Measurement units can be assigned using  
#' \code{units()<-} after attaching the \code{units} 
#' package or with \code{x <- units::set_units(x, "<units>")}. 
#' See \code{units::valid_udunits()}
#' for a list of valid symbolic units. 
#' 
#' If measurements are truly unit-less, or measurement units are unknown, 
#' set \code{options(Rdist_requireUnits = FALSE)}.  This suppresses 
#' all unit checks and conversions.  Users are on their own 
#' to make sure inputs are scaled correctly and that output units are known. 
#'  
#' @details
#' Optimization and estimation controls can be modified using \code{options()}. 
#' See \code{\link{RdistanceControls}}.
#' 
#' @return  An object of class 'dfunc'.  Objects of class 'dfunc' 
#' are lists containing the following components:
#'   \item{parameters}{The vector of estimated parameter values. 
#'     Length of this vector for built-in likelihoods is one 
#'     (for the function's parameter) plus the 
#'     number of expansion terms plus one if the likelihood is 
#'     either 'hazrate' or 'uniform' (hazrate and uniform have
#'     two parameters). }
#'   \item{varcovar}{The variance-covariance matrix for coefficients 
#'     of the distance function, estimated by the inverse of the Hessian
#'     of the fit evaluated at the estimates.  There is no guarantee this 
#'     matrix is positive-definite and should be viewed with caution.  
#'     Error estimates derived from bootstrapping are generally 
#'     more reliable.}   
#'   \item{loglik}{The maximized value of the log likelihood 
#'     (more specifically, the minimized value of the negative 
#'     log likelihood).}
#'   \item{convergence}{The convergence code. This code 
#'     is returned by \code{optim}.  Values other than 0 indicate suspect 
#'     convergence.}
#'     
#'   \item{like.form}{The name of the likelihood. This is 
#'     the value of the argument \code{likelihood}. }
#'     
#'   \item{w.lo}{Left-truncation value used during the fit.}
#'   
#'   \item{w.hi}{Right-truncation value used during the fit.}
#'   
#'   \item{detections}{A data frame of detections within the strip 
#'   or circle used in the fit.  Column 'dist' contains the 
#'   observed distances. 
#'   Column 'groupSize' contains group sizes associated with 
#'   the values of 'dist'. Group 
#'   sizes are only used in \code{abundEstim}.  This data frame 
#'   contains only distances between \code{w.lo} and \code{w.hi}. 
#'   Another component of the returned object, i.e., \code{model.frame} 
#'   contains all observations in the input data, including those outside the strip.}
#'   
#'   \item{covars}{Either NULL if no covariates are included in the 
#'   detection function, or a \code{model.matrix} containing the covariates
#'     used in the fit. Factors in in the model.matrix version have been expanded
#'     into 0-1 indicator variables based on R contrasts in effect at the time 
#'     of the call. Only covariates associated with distances inside the strip 
#'     or circle are included. }
#'     
#'   \item{model.frame}{A \code{model.frame} object containing observed distances 
#'   (the 'response'), covariates specified in the formula, and group sizes if they
#'   were specified.  If specified, the name of the group size column is "offset(-variable-)", 
#'   not "groupsize(-variable-)", because internally it is easier to treat group sizes  
#'   as an offset in the model.  This component is a proper \code{model.frame} and contains
#'   both 'terms' and 'contrasts' attributes. }
#'   
#'   \item{siteID.cols}{A vector containing the transect ID column names in \code{detectionData}
#'   and \code{siteData}. Transect IDs can be a composite of two or more columns and hence 
#'   this component can have length greater than 1. }
#'     
#'   \item{expansions}{The number of expansion terms used 
#'   during estimation.}
#'   
#'   \item{series}{The type of expansion used during estimation.}
#'   
#'   \item{call}{The original call of this function.}
#'   
#'   \item{call.x.scl}{The \emph{input} or user requested 
#'     distance at which the distance function is scaled. }
#'     
#'   \item{call.g.x.scl}{The \code{input} value specifying the 
#'     height of the distance function at a distance 
#'     of \code{call.x.scl}.  }
#'     
#'   \item{call.observer}{The value of input parameter \code{observer}.
#'     The input \code{observer} parameter is only applicable when 
#'     \code{g.x.scl} is a data frame.}
#'     
#'   \item{fit}{The fitted object returned by \code{optim}.  
#'     See documentation for \code{optim}.}
#'     
#'   \item{factor.names}{The names of any factors in \code{formula}. }
#'   
#'   \item{pointSurvey}{The input value of \code{pointSurvey}. 
#'     This is TRUE if distances are radial from a point. FALSE 
#'     if distances are perpendicular off-transect. }
#'     
#'   \item{formula}{The formula specified for the detection function.}
#'   
#'   \item{control}{A list containing values of the 'control' parameters 
#'   set by \code{RdistanceControls}.}
#'   
#'   \item{outputUnits}{The measurement units used for output.  All 
#'     distance measurements are converted to these units internally. }
#'     
#'   \item{x.scl}{The \emph{actual} distance at which 
#'     the distance function is scaled to some value.  
#'     i.e., this is the actual \emph{x} at 
#'     which g(\emph{x}) = \code{g.x.scl}.
#'     Note that \code{call.x.scl} = \code{x.scl} unless 
#'     \code{call.x.scl} == "max", in which case \code{x.scl} is the 
#'     distance at which \emph{g}() is maximized. } 
#'     
#'   \item{g.x.scl}{The \emph{actual} height of the distance function 
#'     at a distance of \code{x.scl}. Note that \code{g.x.scl} = 
#'     \code{call.g.x.scl} unless \code{call.g.x.scl}
#'     is a multiple observer data frame, in which case \code{g.x.scl} is the 
#'     actual height of the distance function at \code{x.scl} computed 
#'     from the multiple observer data frame.   }
#'     
#' @references Buckland, S.T., D.R. Anderson, K.P. Burnham, J.L. Laake, D.L. Borchers,
#'    and L. Thomas. (2001) \emph{Introduction to distance sampling: estimating
#'    abundance of biological populations}. Oxford University Press, Oxford, UK.
#'     
#' @seealso \code{\link{abundEstim}}, \code{\link{autoDistSamp}}.
#' Likelihood-specific help files (e.g., \code{\link{halfnorm.like}}). 
#' See package vignettes for additional options. 
#' 
#' @examples 
#' # Load example sparrow data (line transect survey type)
#' data(sparrowDetectionData)
#' 
#' dfunc <- dfuncEstim(formula = dist ~ 1
#'                   , detectionData = sparrowDetectionData)
#' dfunc
#' plot(dfunc)                   
#'
#' @keywords model
#' @export
dE.lt.single <- function(   data
                            , formula
                            , likelihood = "halfnorm"
                            , w.lo = units::set_units(0,"m")
                            , w.hi = NULL
                            , expansions = 0
                            , series = "cosine"
                            , x.scl = units::set_units(0,"m")
                            , g.x.scl = 1
                            , warn = TRUE
                            , outputUnits = NULL
 ){

  if ( likelihood == "uniform" ){
    .Deprecated(new = "logistic.like"
                , package = "Rdistance"
                , msg = paste("'unform.like' is depricated. Use 'logistic'.\n"
                              , "Switching to 'logistic' likelihood.")
                , old = "uniform.like")
    likelihood <- "logistic"
  }

  # Parse the formula and make a model list ----
  # all parameters go into parseModel because they need to become
  # components for the output list, not just formula.
  # All checking is done in parseModel(), including 
  # check of units (via checkUnits()).
  modelList <- Rdistance::parseModel(data
                          , formula = formula
                          , likelihood = likelihood
                          , w.lo = w.lo
                          , w.hi = w.hi
                          , series = series
                          , x.scl = x.scl
                          , g.x.scl = g.x.scl
                          , outputUnits = outputUnits
                        )

  strt.lims <- Rdistance::startLimits(modelList)

  # Perform optimization
  fit <- mlEstimates( modelList )
  

  ans <- list(parameters = fit$par,
    varcovar = varcovar,
    loglik = fit$value,
    convergence = fit$convergence,
    like.form = likelihood,
    w.lo = w.lo,
    w.hi = w.hi,
    detections = data.frame(dist, groupSize),
    covars = covars,
    model.frame = mf,
    siteID.cols = siteID.cols,
    expansions = expansions,
    series = series,
    call = cl,
    call.x.scl = x.scl,
    call.g.x.scl = g.x.scl,
    call.observer = observer,
    fit = fit,
    factor.names = factor.names,
    pointSurvey = pointSurvey,
    formula = formula,
    control = control,
    outputUnits = outUnits)

ans$loglik <- F.nLL(ans$parameters
, ans$detections$dist
, covars = ans$covars
, like = ans$like.form
, w.lo = ans$w.lo
, w.hi = ans$w.hi
, series = ans$series
, expansions = ans$expansions
, pointSurvey = ans$pointSurvey
, for.optim = F)

# Assemble results
class(ans) <- "dfunc"
if ( ans$like.form != "Gamma" ){
# not absolutely necessary. Could estimate these later in print and plot methods.
# but this saves a little time.
gx <- F.gx.estim(ans)
ans$x.scl <- gx$x.scl
ans$g.x.scl <- gx$g.x.scl
} else {
# Special case of Gamma
ans$x.scl <- x.scl
ans$g.x.scl <- g.x.scl
}

# ---- Check parameter boundaries ----
fuzz <- 1e-06
low.bound <- fit$par <= (strt.lims$lowlimit + fuzz)
high.bound <- fit$par >= (strt.lims$uplimit - fuzz)
if (fit$convergence != 0) {
if (warn) warning(fit$message)
}
if (any(low.bound)) {
ans$convergence <- -1
messL <- paste(paste(strt.lims$names[low.bound], "parameter at lower boundary.")
, collapse = "; ")
ans$fit$message <- messL
if (warn) warning(ans$fit$message)
} else {
messL <- NULL
}
if (any(high.bound)) {
ans$convergence <- -1
messH <- paste(paste(strt.lims$names[high.bound], "parameter at upper boundary.")
, collapse = "; ")
ans$fit$message <- c(messL, messH)
if (warn) warning(ans$fit$message)
}

ans

} # end function
