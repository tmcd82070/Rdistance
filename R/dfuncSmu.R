#' @title Estimate a non-parametric smooth detection function 
#' from distance-sampling data
#' 
#' @description Estimates a smooth detection function for 
#' line-transect perpendicular distances or point-transect 
#' radial distances. 
#' 
#' @param formula A formula object (e.g., dist ~ 1). 
#' The left-hand side (before ~)
#' is the name of the vector containing distances (perpendicular or 
#' radial).  The right-hand side (after ~)
#' must be the intercept-only model as \code{Rdistance} does not 
#' currently allow covariates in smoothed distance functions. 
#' If names in \code{formula} do not appear in \code{detectionData}, 
#' the normal scoping rules for model fitting routines (e.g., 
#' \code{lm} and \code{glm}) apply.
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
#'   The site ID  column(s) (see argument \code{siteID}) must 
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
#'   covariates in some version after 2.1.1, detection-level 
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
#' @param bw Bandwidth of the smooth, which controls
#' smoothness.  Smoothing is done by \code{stats::density}, and 
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
#'       and the interquartile range.  See \code{\link[stats]{bw.nrd0}}.
#'   \item "nrd" : The more common 'rule-of-thumb' variation given by 
#'       Scott (1992). This rule uses 1.06 in the denominator of the 
#'       "nrd0" bandwidth. See \code{\link[stats]{bw.nrd}}
#'   \item "bcv" : The biased cross-validation method. See \code{\link[MASS]{bcv}}. 
#'   \item "ucv" : The unbiased cross-validation method. See \code{\link[MASS]{ucv}}.
#'   \item "SJ" or "SJ-ste" : The 'solve-the-equation' bandwidth of Sheather & 
#'       Jones (1991).  See \code{\link[stats]{bw.SJ}} or \code{\link[MASS]{width.SJ}}.
#'   \item "SJ-dpi" (default) : The 'direct-plug-in' bandwidth of Sheather & 
#'       Jones (1991). See \code{\link[stats]{bw.SJ}} or \code{\link[MASS]{width.SJ}}.
#' }  
#' 
#' @param adjust Bandwidth adjustment for the amount of smooth. 
#' Smoothing is done by \code{\link[stats]{density}}, and 
#' this parameter is 
#' passed straight to it's \code{adjust} argument.  
#' In \code{stats::density}, the bandwidth used is 
#' actually \code{adjust*bw}, and inclusion of this parameters makes 
#' it easier to specify values like 'half the default' bandwidth. 
#' 
#' @param kernel Character string specifying the smoothing kernel function. 
#' This parameters is passed unmodified to \code{stats::density}.  Valid 
#' values are:
#' \itemize{
#'   \item "gaussian" : Gaussian (normal) kernel, the default
#'   \item "rectangular" : Uniform or flat kernel
#'   \item "triangular" : Equilateral triangular kernel
#'   \item "epanechnikov" : the Epanechnikov kernel
#'   \item "biweight" : the biweight kernel
#'   \item "cosine" : the S version of the cosine kernel
#'   \item "optcosine" : the optimal cosine kernel which is the usual 
#'   one reported in the literature 
#' }
#' Values of \code{kernel} may be abbreviated to the first letter of 
#' each string. The numeric value of \code{bw} used in the smooth 
#' is stored in the \code{$fit} component of the returned object 
#' (i.e., in \code{returned$fit$bw}).
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
#' could be observed. If left unspecified (i.e., at the default of 
#' NULL), right-truncation is set to the maximum of the 
#' observed distances.
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
#' @param warn A logical scalar specifying whether to issue 
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
#' For point-transects, the amalgamation of \code{transectID} and 
#' \code{pointID} specify unique sampling sites.  
#' See \bold{Input data frames}. 
#' 
#' @param pointID When point-transects are used, this is the 
#' ID of points on a transect.  When \code{pointSurvey}=TRUE, 
#' the amalgamation of \code{transectID} and 
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
#' @section Input data frames:
#' To save space and to easily specify 
#' sites without detections, 
#' all site ID's, regardless whether a detection occurred there,
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
#' to \code{density}.  Distances exactly equal to \code{w.lo} are not
#' reflected.  Reflection around \code{w.lo} greatly improves
#' performance of the kernel methods near the \code{w.lo} boundary 
#' where substantial non-zero probability of sighting typically exists.  
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
#'   \item{fit}{The smoothed object returned by \code{stats::density}. All
#'     information returned by \code{stats::density} is preserved, and 
#'     in particular the numeric value of the bandwidth used during the 
#'     smooth is returned in \code{fit$bw}}
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
#' the Royal Statistical Society series B}, 53, 683-690.
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
#' dfuncSmu <- dfuncSmu(dist~1, sparrowDetectionData, w.hi=units::set_units(150, "m"))
#' dfuncHn  <- dfuncEstim(formula=dist~1,sparrowDetectionData,w.hi=units::set_units(150, "m"))
#' 
#' # Print and plot results
#' dfuncSmu
#' dfuncHn
#' plot(dfuncSmu,main="",nbins=50)
#' 
#' x <- seq(0,150,length=200)
#' y <- dnorm(x, 0, predict(dfuncHn)[1])
#' y <- y/y[1]
#' lines(x,y, col="orange", lwd=2)
#' legend("topright", legend=c("Smooth","Halfnorm"), 
#'   col=c("red","orange"), lwd=2)
#'
#' @keywords model
#' @export
#' @importFrom stats model.response is.empty.model model.matrix contrasts

dfuncSmu <- function (formula
                    , detectionData
                    , siteData
                    , bw = "SJ-dpi"
                    , adjust = 1
                    , kernel = "gaussian"
                    , pointSurvey = FALSE
                    , w.lo = units::set_units(0,"m")
                    , w.hi = NULL
                    , x.scl = "max"
                    , g.x.scl = 1
                    , observer = "both"
                    , warn = TRUE
                    , transectID = NULL
                    , pointID = "point"
                    , outputUnits = NULL
                    , length = "length"
                    , control = RdistanceControls()
                    ){
  
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
  
  groupSize <- model.offset(mf)
  if( is.null(groupSize) ){
    # no groupsize specified, assume 1
    groupSize <- rep(1, length(dist) )
  }
  
  # Check for measurement units 
  if( !inherits(dist, "units") & control$requireUnits ){
    dfName <- deparse(substitute(detectionData))
    respName <- as.character(attr(mt, "variables"))[attr(mt, "response") + 1]
    stop(paste("Distance measurement units are required.", 
               "Assign units by attaching 'units' package then:\n", 
               paste0("units(",dfName,"$", respName, ")"), "<- '<units of measurment>',\n", 
               "for example 'm' (meters) or 'ft' (feet). See units::valid_udunits()"))
  } else if( control$requireUnits ){
    # if we are here, dist has units
    # set units for output by converting dist units; w.lo, w.hi, and x.scl will all be converted later
    if( !is.null(outputUnits) ){
      dist <-  units::set_units(dist, outputUnits, mode = "standard")
    }
    outUnits <- units(dist)
  }
  
  if( !inherits(w.lo, "units") & control$requireUnits ){
    if( w.lo[1] != 0 ){
      stop(paste("Units of minimum distance are required.",
                 "Assign units by attaching 'units' package then:\n", 
                 "units(w.lo) <- '<units>' or", 
                 paste0("units::as_units(", w.lo,", <units>) in function call\n"), 
                 "See units::valid_udunits() for valid symbolic units."))
    }
    # if we are here, w.lo is 0, it has no units, and we require units
    w.lo <- units::set_units(w.lo, outUnits, mode = "standard")  # assign units to 0
  } else if( control$requireUnits ){
    # if we are here, w.lo has units and we require units, convert to the output units
    w.lo <-  units::set_units(w.lo, outUnits, mode = "standard")
  }
  
  if(is.null(w.hi)){
    w.hi <- max(dist, na.rm=TRUE)  # units flow through max automatically
  } else if( !inherits(w.hi, "units") & control$requireUnits ){
    stop(paste("Max distance measurement units are required.",
               "Assign units by attaching 'units' package then:\n", 
               "units(w.hi) <- '<units>' or", 
               paste0("units::as_units(", w.hi,", <units>) in function call\n"), 
               "See units::valid_udunits() for valid symbolic units."))
  } else if( control$requireUnits ){
    # if we are here, w.hi has units and we require them, convert to output units
    w.hi <-  units::set_units(w.hi, outUnits, mode = "standard")
  }
  
  # (tlm) Add this back in when we allow strata (factors) in the smoothed dfuncs
  # covars <- if (!is.empty.model(mt)){
  #   model.matrix(mt, mf, contrasts)
  # }


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
  dsmu <- stats::density(reflectedDist, bw=bw, adjust=adjust, kernel=kernel,
                  from=units::drop_units(w.lo), to=units::drop_units(w.hi))

    
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
              expansions = 0,
              covars = NULL,
              w.lo = w.lo, 
              w.hi = w.hi, 
              detections = data.frame(dist, groupSize), 
              fit = dsmu,
              covars = NULL, 
              #factor.names = factor.names, 
              call = cl, 
              call.x.scl = x.scl, 
              call.g.x.scl = g.x.scl, 
              call.observer = observer, 
              pointSurvey = pointSurvey,
              outputUnits = outUnits,
              formula = formula)
  

  L <- smu.like(ans$parameters, dist, 
                w.lo=w.lo, w.hi=w.hi, 
                pointSurvey=pointSurvey, 
                scale=TRUE)
  
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
