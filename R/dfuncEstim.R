#' @title Estimate a detection function from distance-sampling data
#' 
#' @description Fit a specific detection function to off-transect 
#' or off-point (radial) distances.
#' 
#' @param formula A standard formula object (e.g., \code{dist ~ 1}, 
#' \code{dist ~ covar1 + covar2}). The left-hand side (before \code{~})
#' is the name of the vector containing distances (off-transect or 
#' radial).  The right-hand side (after \code{~})
#' contains the names of covariate vectors to fit in the detection
#' function. Covariates can be either detection level and appear in \code{detectionData} 
#' or transect level and appear in  \code{siteData}. Regular R scoping 
#' rules apply.  
#' 
#' \bold{Group Sizes:} Non-unity group sizes are specified using \code{offset}.
#' That is, when group sizes are not all 1, they must 
#' be entered as a column in \code{detectionData} and specified 
#' using \code{offset()} as part of \code{formula}.  For example, 
#' \code{d ~ habitat + offset(groupSize)} specifies that 
#' distances appear in variable \code{d}, one covariate 
#' named \code{habitat} is to be fitted, and column \code{groupSize} 
#' contains the number of individuals 
#' associated with each detection.  If an offset is not specified, 
#' all group sizes are assumed to be 1.
#'   
#' 
#' @param detectionData A data frame containing detection distances 
#' (either perpendicular for line-transect or radial for point-transect
#' designs), with one row per detected object or group.   
#' This data frame must contain at least the following 
#' information: 
#' \itemize{
#'   \item Detection Distances: A single column containing 
#'   detection distances must be specified on the left-hand 
#'   side of \code{formula}.  As of Rdistance version 2.2.0, 
#'   the detection distances must have measurement units attached. 
#'   Attach measurements units to distances using \code{library(units);units()<-}.
#'   For example, \code{library(units)} followed by \code{units(df$dist) <- "m"} or 
#'   \code{units(df$dist) <- "ft"} will work. Alternatively, 
#'   \code{df$dist <- units::set_units(df$dist, "m")} also works.
#'   
#'   \item Site IDs: The ID of the transect or point 
#'   (i.e., the 'site') where each object or group was detected.
#'   The site ID  column(s) (see arguments \code{transectID} and
#'   \code{pointID}) must 
#'   specify the site (transect or point) so that this 
#'   data frame can be merged with \code{siteData}.    
#'  
#'   \item In a later release, \code{Rdistance} will allow detection-level 
#'   covariates.  When that happens, detection-level 
#'   covariates will appear in this data frame. 
#'    
#' }
#' See example data set \code{\link{sparrowDetectionData}}.
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
#' @inheritParams F.gx.estim
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
#' @param transectID A character vector naming the transect ID column(s) in
#' \code{detectionData} and \code{siteData}.  \code{Rdistance} 
#' accommodates two kinds of transects: continuous and point.  
#' When continuous transects are used, detections can occur at
#' any point along the route and these are generally called 
#' line-transects. When point transects are used, 
#' detections can only occur at a series of stops (points) 
#' along the route and are generally called point-transects.  
#' Transects themselves are the 
#' basic sampling unit when \code{pointSurvey}=FALSE and 
#' are synonymous with sites in this case. Transects
#' may contain multiple sampling 
#' units (i.e., points) when \code{pointSurvey}=TRUE. 
#' For line-transects, the \code{transectID} column(s) alone is 
#' sufficient to specify unique sample sites. 
#' For point-transects, the combination of \code{transectID} and 
#' \code{pointID} specify unique sampling sites.  
#' See \bold{Input data frames} below. 
#' 
#' @param pointID When point-transects are used, this is the 
#' ID of points on a transect.  When \code{pointSurvey}=TRUE, 
#' the combination of \code{transectID} and 
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
#' @param outputUnits A string giving the symbolic measurment 
#' units that results should be reported in.   Any 
#' distance measurement unit in \code{units::valid_udunits()} 
#' will work.  The strings for common distance symbolic units are: 
#' "m" for meters, "ft" for feet, "cm" for centimeters, "mm" for 
#' millimeters, "mi" for miles, "nmile" for 
#' nautical miles ("nm" is nano meters), "in" for inches, 
#' "yd" for yards, "km" for kilometers, "fathom" for fathoms, 
#' "chains" for chains, and "furlong" for furlongs.  
#' If \code{outputUnits} is unspecified (NULL),
#' output units are the same as distance measurements units in 
#' \code{data}.  
#' 
#' @section Input data frames:
#' To save space and to easily specify 
#' sites without detections, 
#' all site ID's, regardless of whether a detection occurred there,
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
#'          Only \code{detectionData} is required when 
#'          no covariates are included in the distance function (i.e., the right-hand side of 
#'          \code{formula} is "~1" or "~offset(groupSize)"). Note that \code{dfuncEstim}
#'          does not need to know transect IDs (or group sizes)  
#'          in order to estimate a distance function. Group sizes and 
#'          transect IDs are required to estimate abundance 
#'          (in function \code{abundEstim}).
#'   
#'       \item \bold{Neither detection data nor site data required}\cr
#'          Neither \code{detectionData} nor \code{siteData}  
#'          are required if all variables specified in \code{formula} 
#'          are within the scope of \code{dfuncEstim} (e.g., in the global working
#'          environment). Regular R scoping rules apply when the call 
#'          to \code{dfuncEstim} is embedded 
#'          in a function. 
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
#' site ID's specify individual points and are unique values 
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
#' \code{detectionData} because multiple detections can occur on a single transect
#' or site.   
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
#' \code{Rdistance} uses maximum likelihood to estimate the parameter(s) of 
#' distance functions. Distance functions are fit to individual 
#' distance observations despite plot methods that draw histogram 
#' bins. 
#' 
#' @section Measurement Units: 
#' As of \code{Rdistance} version 3.0.0, measurement units are 
#' require on all distances.  This includes off-transect 
#' distances, radial 
#' distances, truncation distances (\code{w.lo} and \code{w.hi}), 
#' transect lengths, and study size area. 
#' This requirement 
#' ensures that internal calculations and results 
#' (e.g., ESW and abundance) are correct 
#' and that the units of output are clear.   
#' Input distances can have variable units. For example, 
#' input distances can be in specified in "m", \code{w.hi} in "in", 
#' and \code{w.lo} in "km".  Internally, all distances are 
#' converted to the units specified by \code{outputUnits} 
#' (or the units of input distances if 
#' \code{outputUnits} is NULL), and 
#' all output is reported 
#' in units of \code{outputUnits}. In 
#' other words, specifying \code{w.hi = units::set_units(100, "m")} 
#' yields the same results as \code{w.hi = units::set_units(328.08, "ft")}.
#'   
#' Measurement units can be assigned using  
#' \code{units()<-} after attaching the \code{units} 
#' package or \code{units::set_units}. 
#' \code{units::valid_udunits()}
#' produces a list of all valid symbolic units. 
#' 
#' In \code{dfuncEstim}, units are required on the following: 
#' \code{detectionData$dist}; 
#' \code{w.lo} (unless it is zero); \code{w.hi} (unless it is NULL); 
#' and \code{x.scl}. In addition, in \code{abundEstim}, units are 
#' required on \code{siteData$length} and \code{area}. All units are 
#' 1-dimensonal except those on \code{area}, which are 2-dimensional. 
#' 
#' If measurements are truly unit-less, or measurement units are unknown, 
#' set \code{RdistanceControls(requireUnits = FALSE)}.  This suppresses 
#' all unit checks and conversions.  Users are on their own 
#' to make sure inputs are scaled correctly and that output units are known. 
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
#'   or circle used in the fit.  One column is 
#'   named 'dist' and it contains the vector of observed distances. 
#'   A second column is named 'groupSize' and contains
#'   a vector of group sizes associated with values in 'dist'. Group 
#'   sizes are only used in \code{abundEstim}.  This data frame 
#'   only contains distances between \code{w.lo} and \code{w.hi}. 
#'   Another component of the returned object, i.e., \code{model.frame} 
#'   contains all observations 
#'   in the input data, including those outside the strip.}
#'   
#'   \item{covars}{Either NULL if no covariates are included in the 
#'   detection function, or a \code{model.matrix} containing the covariates
#'     used in the fit. }
#'     
#'   \item{model.frame}{A \code{model.frame} object containing observed distances 
#'   (the 'response') and any covariates in the fit.  This component does not 
#'   contain group sizes (i.e., no offset values). This component does contain
#'   'terms' and 'contrasts' attributes. }
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
#'                     likelihood="halfnorm", 
#'                     w.hi=units::as_units(100, "m"))
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
#'    vertLines = FALSE, lty=c(1,1), 
#'    col.dfunc=heat.colors(length(levels(sparrowSiteData$observer))), 
#'    col=c("grey","lightgrey"), border=NA, 
#'    xlab="Distance (m)",
#'    main="Showing plot options")
#' 
#'
#' @keywords model
#' @export
#' @importFrom stats nlminb model.response is.empty.model 
#' @importFrom stats model.matrix contrasts optim
#' @import units

dfuncEstim <- function (formula 
                        , detectionData 
                        , siteData 
                        , likelihood = "halfnorm"
                        , pointSurvey = FALSE 
                        , w.lo = units::set_units(0,"m")
                        , w.hi = NULL 
                        , expansions = 0 
                        , series = "cosine" 
                        , x.scl = units::set_units(0,"m")
                        , g.x.scl = 1 
                        , observer = "both" 
                        , warn = TRUE 
                        , transectID = NULL 
                        , pointID = "point" 
                        , length = "length"
                        , outputUnits = NULL
                        , control = RdistanceControls()){
  
  # To-do: remove control = RdistanceControls.  Put all options in 
  # 'options(<name> = <value>)' in onAttach function.
  
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

  if( likelihood == "uniform" ){
    .Deprecated(new = "logistic.like"
                , package = "Rdistance"
                , msg = paste("'unform.like' been re-named and is deprecated.\n"
                       , "Using likelihood = 'logistic' instead.")
                , old = "uniform.like")
    likelihood <- "logistic"
  }  
  

  # (jdc) The double-observer method hasn't been checked since updating to version 2.x
  # It's likely that an error would be raised if a user did try the double-observer option,
  # but I'm adding a warning here, just in case
  if(observer != "both") {
    stop("The double-observer routines have not been tested in Rdistance 
          versions >2.x, so they have been disabled for the time being.
          Contact the Rdistance authors if you need double observer analyses
          and can help.")
  }
  
  mf <- Rdistance:::getDfuncModelFrame(formula, data)
  mt <- attr(mf, "terms")
  dist <- model.response(mf,"any")
  covars <- if (!is.empty.model(mt)){
    model.matrix(object = mt, 
                 data = mf, 
                 contrasts.arg = control$contrasts )
  }
  groupSize <- model.offset(mf)
  if( is.null(groupSize) ){
    # no groupsize specified, assume 1
    groupSize <- rep(1, length(dist) )
  }
  
  contr <- attr(covars,"contrasts")
  assgn <- attr(covars,"assign")

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

  # Units on x.scl are enforced in F.gx.estim
  ncovars <- ncol(covars)
  
  # Truncate for w.lo and w.hi
  ind <- (w.lo <= dist) & (dist <= w.hi)
  dist <- dist[ind]
  groupSize <- groupSize[ind]
  covars <- covars[ind,,drop=FALSE]  # covars looses "extra" attributes here
  
  attr(covars,"assign") <- assgn
  attr(covars,"contrasts") <- contr

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

  # Minimum number of spline basis functions
  if(expansions < 2 & series == "bspline"){
    expansions <- 2
    if(warn) warning("Minimum spline expansion terms = 2. Have used 2.")
  }
  
  
  # Override x.scl for Gamma likelihood
  if( !is.character(x.scl) ){
    if( inherits(x.scl, "units") ){ # this if needed cause drop units does not work on plain vector
      isZero <- units::drop_units(x.scl) == 0 
    } else {
      isZero <- x.scl == 0
    }
    if( isZero & likelihood == "Gamma" ){
      x.scl <- "max"
      # warning("Cannot specify g(0) for Gamma likelihood.  x.scl changed to 'max'.")
    }
  }
  
  
  
  # Find which columns are factors.
  # This works when covars is NULL and must be called 
  # even when ncovars == 1 to cover case like dist ~ -1+x (no intercept)
  for(i in 1:ncol(mf)){
    if(is.factor(mf[,i])){
      factor.names <- c(factor.names, names(mf)[i])
    }
  }

  
  vnames<-dimnames(covars)[[2]]

  # Stop and print error if dist vector contains NAs
  if(any(is.na(dist))) stop("Please remove detections for which dist is NA.")
  
  strt.lims <- F.start.limits(likelihood
                              , expansions
                              , w.lo
                              , w.hi
                              , dist
                              , covars
                              , pointSurvey)

  # Perform optimization
  if(control$optimizer == "optim"){
    fit <- optim(strt.lims$start, 
                 F.nLL, 
                 lower = units::drop_units(strt.lims$lowlimit), # safe 
                 upper = units::drop_units(strt.lims$uplimit), 
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
    fit <- nlminb(start = strt.lims$start
                , objective = F.nLL
                , lower = strt.lims$lowlimit
                , upper = strt.lims$uplimit
                , control = list(trace = 0
                              , eval.max = control$evalMax
                              , iter.max = control$maxIters
                              , rel.tol = control$likeTol
                              ,  x.tol = control$coefTol
                              )
                , dist = dist
                , like = likelihood
                , covars = covars
                , w.lo = w.lo
                , w.hi = w.hi
                , expansions = expansions
                , series = series
                , pointSurvey = pointSurvey
                , for.optim = T 
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
      if (warn) warning("Singular variance-covariance matrix.")
      varcovar <- matrix(NaN, nrow(fit$hessian), ncol(fit$hessian))
    } else {
      varcovar <- tryCatch(solve(fit$hessian), error=function(e){NaN})
      if(length(varcovar) == 1 && is.nan(varcovar)){
        if (warn) warning("Singular variance-covariance matrix.")
        varcovar <- matrix(NaN, nrow(fit$hessian), ncol(fit$hessian))
      }
    }
  } else {
    if (warn) warning("fit did not converge, or converged to (Inf,-Inf)")
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
              detections = data.frame(dist, groupSize), 
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
  if( ans$like.form != "Gamma" ){
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
  
}  # end function