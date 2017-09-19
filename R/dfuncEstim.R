#' @name dfuncEstim
#' @aliases dfuncEstim
#' 
#' @title Estimate a detection function from distance-sampling data.
#' 
#' @description Fit a specific detection function to a set of 
#' observed off-transect distances.
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
#' designs), \code{transectID}, and 
#' potentially group size (see example data set \code{\link{sparrow.detections}}).
#' \code{detectionData} contains one row per detected group. The column
#' containing detection
#' distances must be specified on the left-hand side of \code{formula}.
#' The site ID  column(s) (see argument \code{siteID}) must specify the site
#' (transect or point) associated with the detection so that this 
#' data frame can be merged with \code{siteData}. See \bold{Input data frames} 
#' for when \code{detectionData} and \code{siteData} are required inputs. 
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
#' could be observed. Default is the maximum of \code{dist}.
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
#' @param x.scl This parameter is passed to \code{F.g.estim}. 
#' See \code{F.gx.estim} documentation for definition.
#' 
#' @param g.x.scl This parameter is passed to \code{F.g.estim}. 
#' See \code{F.gx.estim} documentation for definition.
#' 
#' @param observer This parameter is passed to \code{F.g.estim}. 
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
#' \code{detectionData} and \code{siteData}.  Transects can be the 
#' basic sampling unit (when \code{pointSurvey}=FALSE) or 
#' contain multiple sampling units (e.g., when \code{pointSurvey}=TRUE). 
#' For line-transects, the \code{transectID} column(s) alone is 
#' sufficient to specify unique sample sites. 
#' For point-transects, the amalgomation of \code{transectID} and 
#' \code{pointID} specify unique sampling sites.  
#' See \bold{Input data frames}. 
#' 
#' @param length Character string specifying the (single) column in 
#' \code{siteData} that contains transect length. This is ignored if 
#' \code{pointSurvey} = TRUE.
#' 
#' @section Input data frames:
#' To save space and to easily specify 
#' sites without detections, 
#' all site ID's, regardless whether a detection occured there,
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
#' site ID's (i.e., transect ID's) are unique values of 
#' the \code{transectID} column in \code{siteData}.  In this case,
#' the following merge must work:  
#' \code{merge(detectionData,siteData,by=transectID)}.
#' For point-transects, 
#' site ID's (i.e., point ID's) are unique values 
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
#' To help explain the relationship between data frames, bear in 
#' mind that  during bootstrap estimation of variance
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
#' See \code{\link{uniform.like}} for details on the "uniform", 
#' "halfnorm", "hazrate", and "negexp" likelihoods.  
#' See \code{\link{Gamma.like}} for details on "Gamma". 
#' See package vignettes for information on custom, user-defined 
#' likelihoods. 
#' 
#' @examples 
#'# Prep example sparrow and thrasher detections. 
#'# Merge in transect-level detection covariates.
#'data(sparrow.detections)
#'data(sparrow.sites)
#'sparrow <- merge(sparrow.detections, sparrow.sites, by="siteID")
#'data(thrasher.detections)
#'data(thrasher.sites)
#'thrasher <- merge(thrasher.detections, thrasher.sites, by="siteID")
#'   
#'# Fit multiple detection functions to perpendicular, off-transect distances
#'un.dfunc <- dfuncEstim(dist ~ 1, sparrow, likelihood="uniform", 
#'            w.hi = 150)
#'            
#'hn.dfunc <- dfuncEstim(dist ~ 1, thrasher, likelihood="halfnorm", 
#'            w.hi = 150, pointSurvey = T)
#'            
#'hn2.dfunc <- dfuncEstim(dist ~ sagemean, sparrow, 
#'            likelihood="halfnorm", w.hi = 150, expansions = 1, 
#'            series = "simple")
#'            
#'hz.dfunc <- dfuncEstim(dist ~ observer + bare, thrasher, 
#'            likelihood="hazrate", w.hi = 150, pointSurvey = T, 
#'            expansions = 5, series = "cosine")
#'            
#'ga.dfunc <- dfuncEstim(dist ~ 1, sparrow, likelihood="Gamma", 
#'            w.hi = 150, x.scl="max") 
#'   
#'# Plot the first four detection functions
#'par(mfrow=c(2,2))
#'plot(un.dfunc)
#'plot(hn.dfunc)
#'plot(hn2.dfunc)
#'plot(hz.dfunc)
#'
#' @keywords model
#' @export

dfuncEstim <- function (formula, detectionData, siteData, likelihood="halfnorm", 
                  pointSurvey = FALSE, w.lo=0, w.hi=max(dist), 
                  expansions=0, series="cosine", x.scl=0, g.x.scl=1, 
                  observer="both", warn=TRUE, transectID=NULL, pointID="point", 
                  length="length"){
  
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
  
  mf <- getDfuncModelFrame(formula, data)
  mt <- attr(mf, "terms")
  dist <- model.response(mf,"any")
  covars <- if (!is.empty.model(mt)){
    model.matrix(mt, mf, contrasts)
  }
  
  # Eventually, I'd like to use a constant
  # column for covars to allow intercept only 
  # models.  That is, report a beta coefficient 
  # for ~1 models, and allow expansions in this case. 
  # For now, however, we'll just do the "old" method
  # of estimating the distance function parameter directly.
  
  ncovars <- ncol(covars)
  factor.names <- NULL 
  if(ncovars==1){
    if( attr(covars,"assign")==0 ){
      # constant; intercept only model
      covars <- NULL
    }
  } 

  # We are not allowing expansion terms in presence of covariates
  if( !is.null(covars) & expansions > 0 ){
    expansions=0
    if(warn) warning("Expansions not allowed when covariates are present. Expansions set to 0.")
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
  fit <- optim(strt.lims$start, F.nLL, lower = strt.lims$lowlimit, 
          upper = strt.lims$uplimit, method = c("L-BFGS-B"),
          control = list(trace = 0, maxit = 1000), dist = dist, 
          like = likelihood, covars = covars,
          w.lo = w.lo, w.hi = w.hi, expansions = expansions, 
          series = series, pointSurvey = pointSurvey, 
          for.optim = T, hessian = TRUE)
  
  varcovar <- solve(fit$hessian)
  
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