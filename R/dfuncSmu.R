#' @name dfuncSmu
#' @aliases dfuncSmu
#' 
#' @title Estimate a non-parametric smooth detection function 
#' from distance-sampling data
#' 
#' @description Estimates a smooth detection function for 
#' a set of either  
#' observed off-transect or off-point distances.
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
#' potentially group size (see example data set \code{\link{sparrowDetectionData}}).
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
#' @param bw Bandwidth of the smooth, which controls
#' smoothness.  Smoothing is done by \code{stats::denstiy}, and 
#' \code{bw} is 
#' passed straight to it's \code{bw} argument.  \code{bw} can be 
#' numeric, in which case it is the standard deviation of the 
#' Gaussian smoothing kernel. Or, \code{bw} can be  
#' a character string specifying the 
#' bandwidth selection rule.  Valid character string values 
#' of \code{bw} are the following:
#' \itemize{
#'   \item "nrd0" : Silverman's 'rule-of-thumb' equal to 
#'       \eqn{\frac{0.9s}{1.34n^{-0.2}}}{0.9(s)/(1.34n^(-1/5))}, where 
#'       \eqn{s} is the minimum of standard deviation of the distances 
#'       and the interquartile range.  See \code{\link{stats::bw.nrd0}}.
#'   \item "nrd" : The more common 'rule-of-thumb' variation given by 
#'       Scott (1992). This rule uses 1.06 in the denominator of the 
#'       "nrd0" bandwidth. See \code{\link{stats::bw.nrd}}
#'   \item "bcv" : The biased cross-validation method. See \code{\link{MASS::bcv}}. 
#'   \item "ucv" : The unbiased cross-validation method. See \code{\link{MASS::ucv}}.
#'   \item "SJ" or "SJ-ste" : The 'solve-the-equation' bandwidth of Sheather & 
#'       Jones (1991).  See \code{\link{stats::bw.SJ}} or \code{\link{MASS::width.SJ}}.
#'   \item "SJ-dpi" (default) : The 'direct-plug-in' bandwidth of Sheather & 
#'       Jones (1991). See \code{\link{stats::bw.SJ}} or \code{\link{MASS::width.SJ}}.
#' }  
#' 
#' @param pointSurvey A logical scalar specifying whether input data come
#' from point-transect surveys (TRUE),
#' or line-transect surveys (FALSE).  Point surveys (TRUE) have not been 
#' implemented yet.
#' 
#' @param w.lo Lower or left-truncation limit of the distances in distance data. 
#' This is the minimum possible off-transect distance. Default is 0.
#' 
#' @param w.hi Upper or right-truncation limit of the distances 
#' in \code{dist}. This is the maximum off-transect distance that 
#' could be observed. Default is the maximum of \code{dist}.
#' 
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
#' @details Distances are reflected about \code{w.lo} before being passed 
#' to \code{density}, and distances exactly equal to \code{w.lo} are not
#' reflected.  This method of density funciton smoothing greatly improves
#' performance of the kernel methods near the \code{w.lo} boundary 
#' where substantial probability of sighting typically exists.  
#' 
#' 
#' @return  An object of class 'dfunc'.  Objects of class 'dfunc' 
#' are lists containing the following components:
#'   \item{parameters}{A data frame containing the $x and $y
#'     components of the smooth. $x is a vector of length 
#'     512 (default for \code{density}) evenly spaced points
#'     between \code{w.lo} and \code{w.hi}.}
#'   \item{loglik}{The value of the log likelihood. Specifically, 
#'     the sum of the negative log heights of the smooth at observed
#'     distances, after the smoothed function has been scaled to integrate
#'     to one. }
#'   \item{w.lo}{Left-truncation value used during the fit.}
#'   \item{w.hi}{Right-truncation value used during the fit.}
#'   \item{dist}{The input vector of observed distances.}
#'   \item{covars}{NULL. Covariates are not allowed in the 
#'     smoothed distance function (yet). }
#'   \item{call}{The original call of this function.}
#'   \item{call.x.scl}{The distance at which the distance function 
#'     is scaled. This is the x at which g(x) = \code{g.x.scl}.
#'     Normally, \code{call.x.scl} = 0.}
#'   \item{call.g.x.scl}{The value of the distance function at distance
#'     \code{call.x.scl}.  Normally, \code{call.g.x.scl} = 1.}
#'   \item{call.observer}{The value of input parameter \code{observer}.}
#'   \item{fit}{The smoothed object returned by \code{density}.}
#'   \item{pointSurvey}{The input value of \code{pointSurvey}. 
#'     This is TRUE if distances are radial from a point. FALSE 
#'     if distances are perpendicular off-transect. }
#'   \item{formula}{The formula specified for the detection function.}
#'     
#' @references 
#' 
#' Buckland, S.T., D.R. Anderson, K.P. Burnham, J.L. Laake, D.L. Borchers,
#'    and L. Thomas. (2001) \emph{Introduction to distance sampling: estimating
#'    abundance of biological populations}. Oxford University Press, Oxford, UK.
#'    
#' Scott, D. W. (1992) \emph{Multivariate Density Estimation: Theory, 
#' Practice, and Visualization.} Wiley.
#' 
#' Sheather, S. J. and Jones, M. C. (1991) A reliable data-based 
#' bandwidth selection method for kernel density estimation. \emph{Journal of 
#' the Royal Statistical Society series B}, 53, 683–690.
#' 
#' Silverman, B. W. (1986) \emph{Density Estimation}. London: Chapman and Hall.
#' 
#'     
#' @author Trent McDonald, WEST Inc.,  \email{tmcdonald@west-inc.com}\cr
#'         
#' @seealso \code{\link{abundEstim}}, \code{\link{autoDistSamp}}, 
#' \code{\link{dfuncEstim}} for the parametric version.
#' 
#' @examples 
#' # Load example sparrow data (line transect survey type)
#' data(sparrowDetectionData)
#' data(sparrowSiteData)
#' 
#' 
#' # Compare smoothed and half-normal detection function
#' dfuncSmu <- dfuncSmu(dist~1, sparrowDetectionData, w.hi=150)
#' dfuncHn  <- dfuncEstim(formula=dist~1,sparrowDetectionData,w.hi=150)
#' 
#' # Print and plot results
#' dfuncSmu
#' dfuncHn
#' plot(dfuncSmu,main="",nbins=50)
#' 
#' x <- seq(0,150,length=200)
#' y <- dnorm(x, 0, predict(fit)[1])
#' y <- y/y[1]
#' lines(x,y, col="orange", lwd=2)
#' legend("topright", legend=c("Smooth","Halfnorm"), 
#'   col=c("red","orange"), lwd=2)
#'
#' @keywords model
#' @export
#' @importFrom stats model.response is.empty.model model.matrix contrasts

dfuncSmu <- function (formula, detectionData, siteData, bw="SJ-dpi", 
                        adjust=1, kernel="gaussian",
                        pointSurvey = FALSE, w.lo=0, w.hi=NULL, 
                        x.scl="max", g.x.scl=1, 
                        observer="both", warn=TRUE, transectID=NULL, 
                        pointID="point", length="length"){
  
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
  
  # (tlm) Add this back in when we allow strata (factors) in the smoothed dfuncs
  # covars <- if (!is.empty.model(mt)){
  #   model.matrix(mt, mf, contrasts)
  # }

  if(is.null(w.hi)){
    w.hi <- max(dist, na.rm=TRUE)
  }
  
  ncovars <- 0
  
  # (tlm) Add this back in when we allow strata (factors) in the smoothed dfuncs
  # ncovars <- ncol(covars)
  # assgn <- attr(covars,"assign")
  

  # Truncate for w.lo and w.hi
  ind <- (w.lo <= dist) & (dist <= w.hi)
  dist <- dist[ind]
  
  # (tlm) Add this back in when we allow strata (factors) in the smoothed dfuncs
  #covars <- covars[ind,,drop=FALSE]
  # factor.names <- NULL 
  # if(ncovars==1){
  #   if( assgn==0 ){
  #     # constant; intercept only model
  #     covars <- NULL
  #   }
  # } 


  # (tlm) Add this back in when we allow strata (factors) in the smoothed dfuncs
  # # Find which columns are factors.
  # # This works when covars is NULL and must be called 
  # # even when ncovars == 1 to cover case like dist ~ -1+x (no intercept)
  # for(i in 1:ncol(mf)){
  #   if(class(mf[,i]) == "factor"){
  #     factor.names <- c(factor.names, names(mf)[i])
  #   }
  # }
  # vnames<-dimnames(covars)[[2]]


  # Stop if dist vector contains NAs
  if(any(is.na(dist))) stop("Please remove detections for which dist is NA.")
  
  # Do the smoothing:
  #   Reflect distances about w.lo
  #   Only reflect values > w.lo (not == w.lo) because we don't want 
  #   to double the number of points at exactly w.lo
  reflectedDist <- c(-dist[dist>w.lo]+2*w.lo,dist)
  dsmu <- density(reflectedDist, bw=bw, adjust=adjust, kernel=kernel,
                  from=w.lo, to=w.hi)

    
  # Make sure none of the y-values are < 0
  dsmu$y[dsmu$y < 0] <- 0
  
  # Internal sample size for smooth is 2X too big because of reflection
  dsmu$n <- dsmu$n / 2
  
  # Store some smoothing stuff in density smooth
  dsmu$call[["x"]] <- as.character(mt[[2]])
  dsmu$call[["bw"]] <- bw
  dsmu$call[["adjust"]] <- adjust
  dsmu$call[["kernel"]] <- kernel
  dsmu$call[["from"]] <- w.lo
  dsmu$call[["to"]] <- w.hi

  param.df <- data.frame(x=dsmu$x, y=dsmu$y)

  ans <- list(parameters = param.df,
              like.form = "smu",
              covars = NULL,
              w.lo = w.lo, 
              w.hi = w.hi, 
              dist = dist, 
              fit = dsmu,
              covars = NULL, 
              #factor.names = factor.names, 
              call = cl, 
              call.x.scl = x.scl, 
              call.g.x.scl = g.x.scl, 
              call.observer = observer, 
              pointSurvey = pointSurvey,
              formula = formula)
  

  L <- smu.like(ans$parameters, dist, 
                w.lo=w.lo, w.hi=w.hi, 
                pointSurvey=pointSurvey, 
                covars=NULL, scale=TRUE)
  
  # shouldn't have any negatives here
  # could have values outside w.lo-w.hi
  ans$loglik <- -sum(log(L),na.rm=TRUE)  
  
  
  # Assemble results
  class(ans) <- "dfunc"
  gx <- F.gx.estim(ans)
  ans$x.scl <- gx$x.scl
  ans$g.x.scl <- gx$g.x.scl

  ans
  
}  # end function