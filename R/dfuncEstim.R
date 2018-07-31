#' @name dfuncEstim
#' @aliases dfuncEstim
#' 
#' @title Estimate a detection function from distance-sampling data
#' 
#' @description Fit a specific detection function off-transect 
#' or off-point (radial) distances.
#' 
#' @param formula A standard formula object (eg dist ~ 1, 
#' dist ~ covar1 + covar2). The left-hand side (before ~)
#' is the name of the vector containing distances (off-transect or 
#' from points).  The right-hand side (after ~)
#' contains the names of covariate vectors to fit in the detection
#' function. If covariates do not appear in \code{data}, they must 
#' be found in the parent frame (similar to \code{lm}, \code{glm}, etc.)
#' 
#' @param detectionData A data frame containing detection distances 
#' (either perpendicular for line-transect or radial for point-transect
#' designs), with one row per detected object or group.   
#' This data frame must contain at least the following 
#' information: 
#' \itemize{
#'   \item Detection Distances: A single column containing 
#'   detection distances must be specified on the left-hand 
#'   side of \code{formula}.
#'   \item Site IDs: The ID of the transect or point 
#'   (i.e., the 'site') where each object or group was detected.
#'   The site ID  column(s) (see arguments \code{transectID} and
#'   \code{pointID}) must 
#'   specify the site (transect or point) so that this 
#'   data frame can be merged with \code{siteData}.    
#' } 
#' Optionally, this data frame can contain the following 
#' variables: 
#' \itemize{
#'   \item Group Sizes: The number of individuals in the group
#'   associated with each detection.  If unspecified, \code{Rdistance}
#'   assumes all detections are of single individuals (i.e., 
#'   all group sizes are 1). 
#'   
#'   \item When \code{Rdistance} allows detection-level 
#'   covariates in some verson after  2.1.1, detection-level 
#'   covariates will appear in this data frame. 
#'    
#' }
#' See example data set \code{\link{sparrowDetectionData}}).
#' See also \bold{Input data frames} below 
#' for information on when \code{detectionData} and 
#' \code{siteData} are required inputs. 
#' 
#' @param siteData A data.frame containing site (transect or point)
#'  IDs and any 
#' \emph{site level} covariates to include in the detection function. 
#' Every unique surveyed site (transect or point) is represented on
#' one row of this data set, whether or not targets were sighted 
#' at the site.  See arguments \code{transectID} and 
#' \code{pointID} for an explanation of site and transect ID's. 
#' 
#' If sites are transects, 
#' this data frame must also contain transect length. By 
#' default, transect length is assumed to be in column 'length' 
#' but can be specified using argument \code{length}. 
#' 
#' The total number of sites surveyed is \code{nrow(siteData)}. 
#' Duplicate site-level IDs are not allowed in \code{siteData}. 
#' 
#' See \bold{Input data frames} 
#' for when \code{detectionData} and \code{siteData} are required inputs. 
#' 
#' 
#' @param likelihood String specifying the likelihood to fit. Built-in 
#' likelihoods at present are "uniform", "halfnorm", 
#' "hazrate", "negexp", and "Gamma". See vignette for a way to use 
#' user-define likelihoods.
#' 
#' @param pointSurvey A logical scalar specifying whether input data come
#' from point-transect surveys (TRUE),
#' or line-transect surveys (FALSE).  
#' 
#' @param w.lo Lower or left-truncation limit of the distances in distance data. 
#' This is the minimum possible off-transect distance. Default is 0.
#' 
#' @param w.hi Upper or right-truncation limit of the distances 
#' in \code{dist}. This is the maximum off-transect distance that 
#' could be observed. If left unspecified (i.e., at the default of 
#' NULL), right-truncation is set to the maximum of the observed 
#' distances.
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
#' @param x.scl This parameter is passed to \code{F.gx.estim}. 
#' See \code{F.gx.estim} documentation for definition.
#' 
#' @param g.x.scl This parameter is passed to \code{F.gx.estim}. 
#' See \code{F.gx.estim} documentation for definition.
#' 
#' @param observer This parameter is passed to \code{F.gx.estim}. 
#' See \code{F.gx.estim} documentation for definition.
#' 
#' @param warn A logical scaler specifying whether to issue 
#' an R warning if the estimation did not converge or if one 
#' or more parameter estimates are at their boundaries.  
#' For estimation, \code{warn} should generally be left at
#' its default value of \code{TRUE}.  When computing bootstrap 
#' confidence intervals, setting \code{warn = FALSE} 
#' turns off annoying warnings when an iteration does 
#' not converge.  Regardless of \code{warn}, messages about 
#' convergence and boundary conditions are printed 
#' by \code{print.dfunc}, \code{print.abund}, and 
#' \code{plot.dfunc}, so there should be little harm in 
#' setting \code{warn = FALSE}.
#' 
#' @param transectID A character vector naming the transect ID column(s) in
#' \code{detectionData} and \code{siteData}.  \code{Rdistance} 
#' accomodates two kinds of transects: continuous and point.  
#' When continuous transects are used, detections can occur at
#' any point along the route and these are generally called 
#' line-transects. When point transects are used, 
#' detections can only occur at a series of stops (points) 
#' along the route and are generally called point-transects.  
#' Transects themselves are the 
#' basic sampling unit when \code{pointSurvey}=FALSE and 
#' are synomous with sites in this case. Transects
#' may contain multiple sampling 
#' units (i.e., points) when \code{pointSurvey}=TRUE). 
#' For line-transects, the \code{transectID} column(s) alone is 
#' sufficient to specify unique sample sites. 
#' For point-transects, the amalgomation of \code{transectID} and 
#' \code{pointID} specify unique sampling sites.  
#' See \bold{Input data frames}. 
#' 
#' @param pointID When point-transects are used, this is the 
#' ID of points on a transect.  When \code{pointSurvey}=TRUE, 
#' the amalgomation of \code{transectID} and 
#' \code{pointID} specify unique sampling sites.  
#' See \bold{Input data frames}.  
#' 
#' If single points are surveyed, 
#' meaning surveyed points were not grouped into transects, each 'transect' consists
#' of one point. In this case, set \code{transectID} equal to 
#' the point's ID and set \code{pointID} equal to 1 for all points. 
#'  
#' @param length Character string specifying the (single) column in 
#' \code{siteData} that contains transect length. This is ignored if 
#' \code{pointSurvey} = TRUE.
#' 
#' @param control A list containing optimization control parameters such 
#' as the maximum number of iterations, tolerance, the optimizer to use, 
#' etc.  See the 
#' \code{\link{RdistanceControls}} function for explanation of each value,
#' the defaults, and the requirements for this list. 
#' See examples below for how to change controls.
#' 
#' @section Input data frames:
#' To save space and to easily specify 
#' sites without detections, 
#' all site ID's, regardless of whether a detection occured there,
#' and \emph{site level} covariates are stored in 
#' the \code{siteData} data frame.  Detection distances and group
#' sizes are measured at the \emph{detection level} and 
#' are stored in the 
#' \code{detectionData} data frame.  
#' 
#' \subsection{Data frame requirements}{The following explains  
#' conditions under which various combinations of the input data frames 
#' are required.
#' 
#'    \enumerate{
#'       \item \bold{Detection data and site data both required:}\cr
#'          Both \code{detectionData} and \code{siteData}  
#'          are required if \emph{site level} covariates are 
#'          specified on the right-hand side of \code{formula}. 
#'          \emph{Detection level} covariates are not currently allowed.
#'   
#'       \item \bold{Detection data only required:}\cr
#'          The \code{detectionData} data frame alone can be 
#'          specified if no covariates 
#'          are included in the distance function (i.e., right-hand side of 
#'          \code{formula} is "~1"). Note that this routine (\code{dfuncEstim})
#'          does not need to know about sites where zero targets were detected, hence
#'          \code{siteData} can be missing when no covariates are involved.
#'   
#'       \item \bold{Neither detection data nor site data required}\cr
#'          Neither \code{detectionData} nor \code{siteData}  
#'          are required if all variables specified in \code{formula} 
#'          are within the scope of this routine (e.g., in the global working
#'          environment). Scoping rules here work the same as for other modeling 
#'          routines in R such as \code{lm} and \code{glm}. Like other modeling 
#'          routines, it is possible to mix and match the location of variables in 
#'          the model.  Some variables can be in the \code{.GlobalEnv} while others 
#'          are in either \code{detectionData} or \code{siteData}. 
#'    }
#'     
#' }
#' 
#' \subsection{Relationship between data frames (transect and point ID's)}{
#' The input data frames, \code{detectionData} and \code{siteData},
#' must be merge-able on unique sites.  For line-transects, 
#' site ID's specify transects or routes and are unique values of 
#' the \code{transectID} column in \code{siteData}.  In this case,
#' the following merge must work:  
#' \code{merge(detectionData,siteData,by=transectID)}.
#' 
#' For point-transects, 
#' site ID's specify individual points are unique values 
#' of the combination \code{paste(transectID,pointID)}.
#' In this case, the following merge must work:    
#' \code{merge(detectionData,siteData,by=c(transectID, pointID)}.
#'  
#' By default,\code{transectID} and \code{pointID} are NULL and
#' the merge is done on all common columns.
#' That is, when \code{transectID} is NULL, this routine assumes unique
#' \emph{transects} are specified by unique combinations of the 
#' common variables (i.e., unique values of
#' \code{intersect(names(detectionData), names(siteData))}). 
#' 
#' An error occurs if there are no common column names between 
#' \code{detectionData} and \code{siteData}.
#' Duplicate site IDs are not allowed in \code{siteData}. 
#' If the same site is surveyed in
#' multiple years, specify another transect ID column (e.g., \code{transectID =
#' c("year","transectID")}).  Duplicate site ID's are allowed in 
#' \code{detectionData}.  
#' 
#' To help envision the relationship between data frames, bear in 
#' mind that during bootstrap estimation of variance
#' in \code{\link{abundEstim}}, 
#' unique \emph{transects} (i.e., unique values of 
#' the transect ID column(s)), not \emph{detections} or 
#' \emph{points}, are resampled with replacement. 
#' }
#'  
#' 
#' @section Likelihood functions:
#' Given a specified sighting function (e.g., "halfnorm"), 
#' maximum likelihood is used to estimate the parameter(s) of 
#' the function (e.g., standard error) that best fit the distance data.
#' 
#' When plotted (see Examples), histogram bins are plotted 
#' behind the detection 
#' function for visualization; however, the function is fit to 
#' the actual data, not to the bins.
#' 
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
#'     of the fit evaluated at the estimates.  There is no guarentee this 
#'     matrix is positive-definite and should be viewed with caution.  
#'     Error estimates derived from bootstrapping are generally more reliable.}   
#'   \item{loglik}{The maximized value of the log likelihood 
#'     (more specifically, the minimized value of the negative 
#'     log likelihood).}
#'   \item{convergence}{The convergence code. This code 
#'     is returned by \code{optim}.  Values other than 0 indicate suspect 
#'     convergence.}
#'   \item{like.form}{The name of the likelihood. This is 
#'     the value of the argument \code{likelihood}. }
#'   \item{w.lo}{Left-truncation value used during the fit.}
#'   \item{w.hi}{Right-truncation value used during the fit.}
#'   \item{dist}{The input vector of observed distances.}
#'   \item{covars}{A \code{model.matrix} containing the covariates
#'     used in the fit. }
#'   \item{expansions}{The number of expansion terms used during estimation.}
#'   \item{series}{The type of expansion used during estimation.}
#'   \item{call}{The original call of this function.}
#'   \item{call.x.scl}{The distance at which the distance function 
#'     is scaled. This is the x at which g(x) = \code{g.x.scl}.
#'     Normally, \code{call.x.scl} = 0.}
#'   \item{call.g.x.scl}{The value of the distance function at distance
#'     \code{call.x.scl}.  Normally, \code{call.g.x.scl} = 1.}
#'   \item{call.observer}{The value of input parameter \code{observer}.}
#'   \item{fit}{The fitted object returned by \code{optim}.  
#'     See documentation for \code{optim}.}
#'   \item{factor.names}{The names of any factors in \code{formula}}
#'   \item{pointSurvey}{The input value of \code{pointSurvey}. 
#'     This is TRUE if distances are radial from a point. FALSE 
#'     if distances are perpendicular off-transect. }
#'   \item{formula}{The formula specified for the detection function.}
#'     
#' @references Buckland, S.T., D.R. Anderson, K.P. Burnham, J.L. Laake, D.L. Borchers,
#'    and L. Thomas. (2001) \emph{Introduction to distance sampling: estimating
#'    abundance of biological populations}. Oxford University Press, Oxford, UK.
#'     
#' @author Trent McDonald, WEST Inc.,  \email{tmcdonald@west-inc.com}\cr
#'         Jason Carlisle, University of Wyoming and WEST Inc., \email{jcarlisle@west-inc.com}\cr
#'         Aidan McDonald, WEST Inc., \email{aidan@mcdcentral.org}
#'         
#' @seealso \code{\link{abundEstim}}, \code{\link{autoDistSamp}}.
#' See likelihood-specific help files (e.g., \code{\link{halfnorm.like}}) for
#' details on each built-in likelihood.  See package vignettes for information on custom,
#' user-defined likelihoods. 
#' 
#' @examples 
#' # Load example sparrow data (line transect survey type)
#' data(sparrowDetectionData)
#' data(sparrowSiteData)
#' 
#' 
#' # Fit half-normal detection function
#' dfunc <- dfuncEstim(formula=dist~1,
#'                     detectionData=sparrowDetectionData,
#'                     likelihood="halfnorm", w.hi=100)
#' 
#' # Fit a second half-normal detection function, now including
#' # a categorical covariate for observer who surveyed the site (factor, 5 levels)
#' # Increase maximum iterations
#' dfuncObs <- dfuncEstim(formula=dist~observer,
#'                        detectionData=sparrowDetectionData,
#'                        siteData=sparrowSiteData,
#'                        likelihood="halfnorm", w.hi=100, pointSurvey=FALSE,
#'                        control=RdistanceControls(maxIter=1000))
#' 
#' # Print results
#' # And plot the detection function for each observer
#' dfuncObs
#' plot(dfuncObs,
#'      newdata=data.frame(observer=levels(sparrowSiteData$observer)))
#'      
#' # Show some plotting options
#' plot(dfuncObs, 
#'    newdata=data.frame(observer=levels(sparrowSiteData$observer)), 
#'    vertLines = F, lty=c(1,1), 
#'    col.dfunc=heat.colors(length(levels(sparrowSiteData$observer))), 
#'    col=c("grey","lightgrey"), border=NA, 
#'    xlab="Distance (m)",
#'    main="Showing plot options")
#' 
#'
#' @keywords model
#' @export
#' @importFrom stats model.response is.empty.model model.matrix contrasts optim

dfuncEstim <- function (formula, detectionData, siteData, likelihood="halfnorm", 
                  pointSurvey = FALSE, w.lo=0, w.hi=NULL, 
                  expansions=0, series="cosine", x.scl=0, g.x.scl=1, 
                  observer="both", warn=TRUE, transectID=NULL, 
                  pointID="point", 
                  length="length",
                  control=RdistanceControls()){
  
  cl <- match.call()
  
  if(!missing(detectionData) & !missing(siteData)){
    if( is.null(transectID) ){
      transectID <- intersect( names(detectionData), names(siteData) )
      if( pointSurvey ){
        pointID <- NULL  # multiple pts per transect not allowed when no transectID specified
      } 
    }
    if( pointSurvey ){
      siteID.cols <- c(transectID, pointID)
    } else {
      siteID.cols <- c(transectID)
    }
    data <- merge(detectionData, siteData, by=siteID.cols)
  } else if(!missing(detectionData)){
    data <- detectionData
  } else{
    data <- NULL
  }
  

  # (jdc) The double-observer method hasn't been checked since updating to version 2.x
  # It's likely that an error would be raised if a user did try the double-observer option,
  # but I'm adding a warning here, just in case
  if(observer != "both") {
    stop("The double-observer routines were not been tested when updating to
          version 2.x, so they have been disables for the time being.")
  }
  
  
  mf <- getDfuncModelFrame(formula, data)
  mt <- attr(mf, "terms")
  dist <- model.response(mf,"any")
  covars <- if (!is.empty.model(mt)){
    model.matrix(mt, mf, contrasts)
  }

  if(is.null(w.hi)){
    w.hi <- max(dist, na.rm=TRUE)
  }
  
  ncovars <- ncol(covars)
  assgn <- attr(covars,"assign")
  

  # Truncate for w.lo and w.hi
  ind <- (w.lo <= dist) & (dist <= w.hi)
  dist <- dist[ind]
  covars <- covars[ind,,drop=FALSE]
  
  # Eventually, I'd like to use a constant
  # column for covars to allow intercept only 
  # models.  That is, report a beta coefficient 
  # for ~1 models, and allow expansions in this case. 
  # For now, however, we'll just do the "old" method
  # of estimating the distance function parameter directly.
  
  
  factor.names <- NULL 
  if(ncovars==1){
    if( assgn==0 ){
      # constant; intercept only model
      covars <- NULL
    }
  } 

  # We are not allowing expansion terms in presence of covariates
  if (!is.null(covars) & expansions > 0) {
    expansions=0
    if(warn) warning("Expansions not allowed when covariates are present. Expansions set to 0.")
  }
  
  
  # The Gamma doesn't work with covariates
  if (!is.null(covars) & likelihood=="Gamma") {
    stop("The Gamma likelihood does not allow covariates in the detection function.")
  }
  
  
  
  # Find which columns are factors.
  # This works when covars is NULL and must be called 
  # even when ncovars == 1 to cover case like dist ~ -1+x (no intercept)
  for(i in 1:ncol(mf)){
    if(class(mf[,i]) == "factor"){
      factor.names <- c(factor.names, names(mf)[i])
    }
  }

  
  vnames<-dimnames(covars)[[2]]


  # Stop and print error if dist vector contains NAs
  if(any(is.na(dist))) stop("Please remove detections for which dist is NA.")
  
  strt.lims <- F.start.limits(likelihood, expansions, w.lo, w.hi, 
                              dist, covars, pointSurvey)
  
  # Perform optimization
  if(control$optimizer == "optim"){
    fit <- optim(strt.lims$start, F.nLL, 
                 lower = strt.lims$lowlimit, 
                 upper = strt.lims$uplimit, 
                 hessian = TRUE,
                 control = list(trace = 0, 
                                maxit = control$maxIters,
                                factr = control$likeTol,
                                pgtol = control$likeTol), 
                 method = c("L-BFGS-B"),
                 dist = dist, 
                 like = likelihood, 
                 covars = covars,
                 w.lo = w.lo, 
                 w.hi = w.hi, 
                 expansions = expansions, 
                 series = series, 
                 pointSurvey = pointSurvey, 
                 for.optim = T)
  } else if(control$optimizer == "nlminb"){
    fit <- nlminb(strt.lims$start, F.nLL, 
                 lower = strt.lims$lowlimit, 
                 upper = strt.lims$uplimit, 
                 control = list(trace = 0,
                                eval.max = control$evalMax,
                                iter.max = control$maxIters,
                                rel.tol = control$likeTol,
                                x.tol = control$coefTol
                                ), 
                 dist = dist, 
                 like = likelihood, 
                 covars = covars,
                 w.lo = w.lo, 
                 w.hi = w.hi, 
                 expansions = expansions, 
                 series = series, 
                 pointSurvey = pointSurvey, 
                 for.optim = T 
                 )
    names(fit)[names(fit)=="evaluations"]<-"counts"
    fit$hessian <- secondDeriv(fit$par, 
                               F.nLL, 
                               eps=control$hessEps,
                               dist = dist, 
                               like = likelihood, 
                               covars = covars,
                               w.lo = w.lo, 
                               w.hi = w.hi, 
                               expansions = expansions, 
                               series = series, 
                               pointSurvey = pointSurvey, 
                               for.optim = T
                               )
  } else {
    stop("Unknown optimizer function in control object")
  }
  
  if(!any(is.na(fit$hessian)) & !any(is.infinite(fit$hessian))){
    qrh <- qr(fit$hessian)
    if (qrh$rank < nrow(fit$hessian)) {
      warning("Singular variance-covariance matrix.")
      varcovar <- matrix(NaN, nrow(fit$hessian), ncol(fit$hessian))
    } else {
      varcovar <- tryCatch(solve(fit$hessian), error=function(e){NaN})
      if(length(varcovar) == 1 && is.nan(varcovar)){
        warning("Singular variance-covariance matrix.")
        varcovar <- matrix(NaN, nrow(fit$hessian), ncol(fit$hessian))
      }
    }
  } else {
    warning("fit did not converge, or converged to (Inf,-Inf)")
    varcovar <- matrix(NaN, nrow(fit$hessian), ncol(fit$hessian))
  }
  
  names(fit$par) <- strt.lims$names
  
  ans <- list(parameters = fit$par,
              varcovar = varcovar,
              loglik = fit$value, 
              convergence = fit$convergence, 
              like.form = likelihood, 
              w.lo = w.lo, 
              w.hi = w.hi, 
              dist = dist, 
              covars = covars, 
              model.frame = mf,
              expansions = expansions, 
              series = series, 
              call = cl, 
              call.x.scl = x.scl, 
              call.g.x.scl = g.x.scl, 
              call.observer = observer, 
              fit = fit, 
              factor.names = factor.names, 
              pointSurvey = pointSurvey,
              formula = formula)
  
  ans$loglik <- F.nLL(ans$parameters, ans$dist, covars = ans$covars, 
                      like = ans$like.form, w.lo = ans$w.lo, w.hi = ans$w.hi, 
                      series = ans$series, expansions = ans$expansions, 
                      pointSurvey = ans$pointSurvey, for.optim = F)
  
  # Assemble results
  class(ans) <- "dfunc"
  gx <- F.gx.estim(ans)
  ans$x.scl <- gx$x.scl
  ans$g.x.scl <- gx$g.x.scl
  fuzz <- 1e-06
  low.bound <- any(fit$par <= (strt.lims$lowlimit + fuzz))
  high.bound <- any(fit$par >= (strt.lims$uplimit - fuzz))
  if (fit$convergence != 0) {
    if (warn) 
      warning(fit$message)
  }
  else if (low.bound | high.bound) {
    ans$convergence <- -1
    ans$fit$message <- "One or more parameters at its boundary."
    if (warn) 
      warning(ans$fit$message)
  }
  ans
  
}  # end function