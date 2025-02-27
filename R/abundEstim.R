#' @title abundEstim - Distance Sampling Abundance Estimates 
#'   
#' @description Estimate abundance (or density) from an estimated detection
#'   function and supplemental information on observed group sizes, transect
#'   lengths, area surveyed, etc.  Computes confidence intervals on
#'   abundance (or density) using a the bias corrected bootstrap method.
#'   
#' @inheritParams dfuncEstim 
#' 
#' @inheritParams predict.dfunc 
#' 
#' @param area A scalar containing the total area of inference. Usually, this is 
#' study area size.  If \code{area} is NULL (the default), 
#' \code{area} will be set to 1 square unit of the output units and density estimates
#' will be produced. 
#' If \code{area} is not NULL, it must have measurement units 
#' assigned by the \code{units} package. 
#' The units on \code{area} must be convertible
#' to squared output units. Units 
#' on \code{area} must be two-dimensional. 
#' For example, if output units are "foo", 
#' units on area must be convertible to "foo^2" by the \code{units}
#' package. Units of "km^2", "cm^2", "ha", "m^2", "acre", "mi^2", and several
#' others are acceptable.  
#'   
#' @param ci A scalar indicating the confidence level of confidence intervals. 
#'   Confidence intervals are computed using a bias corrected bootstrap
#'   method. If \code{ci = NULL} or \code{ci == NA}, confidence intervals 
#'   are not computed.
#'   
#' @param R The number of bootstrap iterations to conduct when \code{ci} is not
#'   NULL.
#'   
#' @param plot.bs A logical scalar indicating whether to plot individual
#'   bootstrap iterations.
#'   
#' @param propUnitSurveyed A scalar or vector of real numbers between 0 and 1.  
#' The proportion of the default sampling unit that 
#'   was surveyed.  If both sides of line transects were observed, 
#'   \code{propUnitSurveyed}
#'   = 1.  If only a single side of line transects were observed, set 
#'   \code{propUnitSurveyed} = 0.5. For point transects, this should be set to 
#'   the proportion of each circle that was observed. Length must either be
#'   1 or the total number of transects in \code{x}.
#'   
#' @param showProgress A logical indicating whether to show a text-based
#'   progress bar during bootstrapping. Default is \code{TRUE}. 
#'   It is handy to shut off the 
#'   progress bar if running this within another function. Otherwise, 
#'   it is handy to see progress of the bootstrap iterations.
#'   
#' @param bySite A logical variable indicating whether to estimate density and abundance 
#'   for individual transects. If TRUE, density and abundance are estimated for every 
#'   row in the input data frame (which is the \code{$data} component of a fitted model). 
#'   If \code{bySite == TRUE}, transect lengths are used to inflate density, \code{area} 
#'   is ignored, and no bootstrap re-sampling takes place.  
#'   If FALSE, density and abundance are estimated on the study area (of size \code{area})
#'   and bootstrap CI estimates are computed if \code{ci} is not NULL. 
#'   
#' @details The abundance estimate for line-transect surveys (if no covariates
#'    are included in the detection function and both sides of the transect 
#'    are observed) is 
#'    \deqn{N =\frac{n(A)}{2(ESW)(L)}}{%
#'          N = n*A / (2*ESW*L)} 
#'    where \emph{n} is total number of sighted individuals 
#'   (i.e., \code{sum(groupSizes(dfunc))}), \emph{L} is the total length of 
#'   surveyed transect (i.e., \code{sum(effort(dfunc))}),
#'   and \emph{ESW} is effective strip width
#'   computed from the estimated distance function (i.e., \code{ESW(dfunc)}).
#'   If only one side of transects were observed, the "2" in the denominator 
#'   is not present (or, replaced with a "1"). 
#'   
#'   The abundance estimate for point transect surveys (if no covariates are
#'   included) is 
#'    \deqn{N =\frac{n(A)}{\pi(ESR^2)(P)}}{%
#'          N = n*A / ((3.1415)*ESR^2*(P))} 
#'    where \emph{n} is total number of sighted individuals (i.e., \code{sum(groupSizes(dfunc))}),
#'    \emph{P} is the total number of surveyed points (i.e., \code{sum(effort(dfunc))}), 
#'    and \emph{ESR} is effective search radius 
#'    computed from the estimated distance function (i.e., \code{ESR(dfunc)}).
#'
#'  Setting \code{plot.bs=FALSE} and \code{showProgress=FALSE} 
#'     suppresses all intermediate output.  
#'     
#'  Estimation of site-specific density (e.g., on every transect) is accomplished using 
#'  \code{\link{predict.dfunc(x, type = "density")}}, while estimation of 
#'  site-specific abundance is accomplished using 
#'  \code{\link{predict.dfunc(x, type = "abundance")}} (\code{x} is an Rdistance 
#'  fitted model). 
#'     
#' @section Bootstrap Confidence Intervals:
#' 
#'   Rdistance's nested data frames (produced by \code{\link{RdistDf}})
#'   contain both all information required to estimate bootstrap CI. 
#'   Rdistance bootstrap resamples
#'   the rows of the \code{$data} component contained in an Rdistance 
#'   fitted model. 
#'   The \code{$data} component of Rdistance fitted models contains one line 
#'   per transect. Via missing value combinations, \code{$data} contains 
#'   informaiton on which observations go into the 
#'   detection functions, which into detected targets, and which into transect length. 
#'   After resampling rows of \code{$data}, Rdistance 
#'   refits the distance function using non-missing distance, recomputes detected 
#'   number of targets using non-missing group sizes on transects with non-missing length, 
#'   and re-computes total transect length using transects with non-missing lengths. 
#'   By default, \code{R} = 500 bootstrap iterations are 
#'   performed, after which bias
#'   corrected confidence intervals are computed (Manly, 1997, section 3.4).
#'   
#'   The distance function is not re-selected during bootstrap resampling. The 
#'   model in the input object is re-fitted every iteration.  
#'   
#'   During bootstrap iterations, the distance function can fail. 
#'   An iteration can fail 
#'   for a two reasons:
#'   (1) no detections on the iteration, and (2) a bad configuration 
#'   of distances that pushes the distance function's parameters to their 
#'   limits. When an iteration fails, Rdistance 
#'   skips the iteration and effectively ignores the 
#'   failed iterations. 
#'   If the proportion of failed iterations is small 
#'   (less than 20% by default), the resulting abundance confidence interval 
#'   is probably valid and no warning is issued.  If the proportion of non-convergent iterations 
#'   is not small (exceeds 20% by default), a warning is issued.  
#'   The warning can be modified  
#'   by re-setting the \code{Rdistance_maxBSFailPropForWarning} option. 
#'   Setting \code{options(Rdistance_masBSFailPropForWarning = 1.0)} will turn 
#'   off the warning. 
#'   Setting \code{options(Rdistance_masBSFailPropForWarning = 0.0)} will 
#'   warn if any iteration failed.  Results (density and effective sampling distance) 
#'   from all successful iterations are contained in the  
#'   non-NA rows of data frame 'B' in the output object.  
#'   
#' @section Missing Transect Lengths:
#' 
# FINISH WRITING DOCUMENTATION HERE!!!
#
#'   \bold{Line transects}: Transect length can be missing in the 'sites'. 
#'   NA length transects are equivalent
#'   to 0 [m] transects and do not count toward total surveyed units.  NA length
#'   transects are handy if some off-transect distance observations should be included
#'   when estimating the distance function, but not when estimating abundance. 
#'   To do this, include the "extra" distance observations in the detection data frame, with valid
#'   site IDs, but set the length of those site IDs to NA in the site data frame. 
#'   Group sizes associated with NA length transects are dropped and not counted toward density
#'   or abundance. Among other things, this allows estimation of abundance on one 
#'   study area using off-transect distance observations from another.  
#'   
#'   \bold{Point transects}: Point transects do not have length. The "length" of point transects
#'   is the number of points on the transect. \code{Rdistance} treats individual points as independent 
#'   and bootstrap resampmles them to estimate variance. To include distance obervations
#'   from some points but not the number of targets seen, include a separate "length" column 
#'   in the site data frame with NA for the "extra" points. Like NA length line transects, 
#'   NA "length" point transects are dropped from the count of points and group sizes on these 
#'   transects are dropped from the counts of targets.  This allows users to estimate their distance 
#'   function on one set of observations while inflating counts from another set of observations.  
#'   A transect "length" column is not required for point transects. Values in the \code{lengthColumn}
#'   do not matter except for NA (e.g., a column of 1's mixed with NA's is acceptable). 
#' 
#'   
#' @return An 'abundance estimate' object, which is a list of
#'   class \code{c("abund", "dfunc")}, containing all the components of a "dfunc"
#'   object (see \code{\link{dfuncEstim}}), plus the following: 
#'   
#'   \item{density}{Estimated density on the sampled area with units. The \emph{effectively}
#'   sampled area is 2*L*ESW (not 2*L*w.hi). Density has squared units of the 
#'   requested output units.  Convert density to other units with  
#'   \code{units::set_units(x$density, "<units>").}} 
#'   
#'   \item{n.hat}{Estimated abundance on the study area (if \code{area} >
#'   1) or estimated density on the study area (if \code{area} = 1), without units.}
#'  
#'   \item{n}{The number of detections (not individuals) 
#'   on non-NA length transects
#'   used to compute density and abundance.}
#'   
#'   \item{n.seen}{The total number of individuals seen on transects with non-NA
#'   length. Sum of group sizes used 
#'   to estimate density and abundance.}
#'  
#'   \item{area}{Total area of inference in squared output units.}
#'   
#'   \item{surveyedUnits}{The total length of sampled transect with units. This is the sum 
#'   of the \code{lengthColumn} column of \code{siteData}. }
#'   
#'   \item{avg.group.size}{Average group size on transects with non-NA length transects.}
#'   
#'   \item{rng.group.size}{Minimum and maximum groupsizes observed on non-NA length transects.}
#'   
#'   \item{effDistance}{A vector containing effective sample distance.  If covariates
#'   are not included, length of this vector is 1 because effective sampling distance 
#'   is constant over detections. If covariates are included, this vector has length
#'   equal to the number of detections (i.e., \code{x$n}).  This vector was produced 
#'   by a call to \code{effectiveDistance()} with \code{newdata} set to NULL.}
#'   
#'   \item{n.hat.ci}{A vector containing the lower and upper limits of the 
#'   bias corrected bootstrap confidence interval for
#'   abundance. } 
#'   
#'   \item{density.ci}{A vector containing the lower and upper limits of the 
#'   bias corrected bootstrap confidence interval for
#'   density, with units.
#'   }
#'
#'   \item{effDistance.ci}{A vector containing the lower and upper limits of the 
#'   bias corrected bootstrap confidence interval for \emph{average}
#'   effective sampling distance.
#'   }
#'   
#'   \item{B}{A data frame containing bootstrap values of coefficients, 
#'   density, and effective distances.  Number of rows is always 
#'   \code{R}, the requested number of bootstrap 
#'   iterations.  If a particular iteration fails, the
#'   corresponding row in \code{B} is \code{NA} (hence, use 'na.rm = TRUE' 
#'   when computing summaries). Columns 1 through \code{length(coef(dfunc))}
#'   contain bootstrap realizations of the distance function's coefficients. 
#'   The second to last column contains bootstrap values of
#'   density (with units).  The last column of B contains bootstrap 
#'   values of effective sampling distance or radius (with units). If the 
#'   distance function contains covariates,
#'   the effective sampling distance column is the average 
#'   effective distance over detections 
#'   used during the associated bootstrap iteration. }
#'   
#'   \item{nItersConverged}{The number of bootstrap iterations that converged.  }
#'   
#'   \item{alpha}{The (scalar) confidence level of the
#'   confidence interval for \code{n.hat}.} 
#'
#'   
#' @references Manly, B.F.J. (1997) \emph{Randomization, bootstrap, and 
#'   Monte-Carlo methods in biology}, London: Chapman and Hall.
#'   
#'   Buckland, S.T., D.R. Anderson, K.P. Burnham, J.L. Laake, D.L. Borchers,
#'    and L. Thomas. (2001) \emph{Introduction to distance sampling: estimating
#'    abundance of biological populations}. Oxford University Press, Oxford, UK.
#'   
#' @seealso \code{\link{dfuncEstim}}, \code{\link{autoDistSamp}}.
#' 
#' @examples
#' # Load example sparrow data (line transect survey type)
#' # sparrowDf <- RdistDf(sparrowSiteData, sparrowDetectionData)
#' data(sparrowDf)
#' 
#' # Fit half-normal detection function
#' dfunc <- sparrowDf |> 
#'   dfuncEstim(formula=dist ~ groupsize(groupsize)
#'            , likelihood="halfnorm"
#'            , w.hi=units::set_units(150, "m")
#'   )
#' 
#' # Estimate abundance - Convenient for programming 
#' abundDf <- estimateN(dfunc
#'                    , area = units::set_units(4105, "km^2")
#'            )
#' 
#' # Same - Nicer output 
#' # Set ci=0.95 (or another value) to estimate bootstrap CI's on ESW, density, and abundance.
#' fit <- abundEstim(dfunc
#'                 , area = units::set_units(4105, "km^2")
#'                 , ci = NULL
#'                 )
#'          
#' @export
abundEstim <- function(x
                     , area = NULL
                     , propUnitSurveyed = 1.0
                     , ci = 0.95
                     , R = 500
                     , plot.bs = FALSE
                     , showProgress = TRUE
                     ){


  # It is okay to have NA in effort, distance, and group size columns. See documentation.
  
  bootstrapping <- !is.null(ci) && !is.na(ci)
  
  # Initial setup for plotting ----
  if (bootstrapping && plot.bs) {
    par( xpd=TRUE )
    plotObj <- plot(x)
  }
  
  # ---- Set Area ----
  if( is.null(area) ){
    # doing this here saves a tiny sliver of time in estimateN
    area <- units::set_units(1, x$outputUnits, mode = "standard")^2
  }
  
  # ---- Construct Indices to Original data frame ----
  nDataRows <- nrow(x$data)
  bsData <- data.frame(id = "Original",
                       rowIndex = 1:nDataRows)

  pb <- list(tick = function(){}) # NULL tick function for not bootstrapping

  # ---- Add bootstrap indices if called for ----
  # This is essentially what rsample::bootstraps does, but rsample stores the 
  # entire data frame in each 'rsample'.  This saves space. But, entire 
  # (R*nrow(x$data)+1) X 2 data frame must be constructable in memory.
  if ( bootstrapping ) {
    
    nDigits <- ceiling(log10(R + 0.1))
    bsData <- bsData |> 
      dplyr::bind_rows(
          data.frame(
            id = rep(1:R, each = nDataRows)
          , rowIndex = sample( 1:nDataRows
                               , size = R*nDataRows
                               , replace = TRUE)
          ) |> 
          dplyr::mutate(id = paste0("Bootstrap_",
                                    formatC(id
                                            , format = "f"
                                            , digits = 0
                                            , width = nDigits
                                            , flag = "0")
          ))
      )
    
    # set up progress bar if called for, only if bootstrapping
    if(showProgress){
      pb <- progress::progress_bar$new(
          format = "Bootstrapping: [:bar] Elapsed Time: :elapsedfull "
        , total = R+1
        , show_after = 1
        , clear = FALSE
      )
    }
  } else {
    ci <- NA
  }

  # --- Apply estimation to each ID group ----
  bsEsts <- bsData |> 
    dplyr::group_by(id) |> 
    dplyr::group_modify(.f = Rdistance:::oneBsIter
                      , data = x$data
                      , formula = x$formula  
                      , likelihood = x$likelihood 
                      , w.lo = x$w.lo
                      , w.hi = x$w.hi
                      , expansions = x$expansions
                      , series = x$series
                      , x.scl = x$x.scl 
                      , g.x.scl = x$g.x.scl
                      , outputUnits = x$outputUnits
                      , warn = FALSE
                      , area = area
                      , propUnitSurveyed = propUnitSurveyed
                      , pb = pb
                      , plot.bs = plot.bs
                      , plotCovValues = plotObj$predCovValues
    )
      
  # ---- Construct output object ----
  ests <- bsEsts |> 
    dplyr::filter(id == "Original")
  if( bootstrapping ){
    B <- bsEsts |> 
      dplyr::filter( id != "Original" )
  } else {
    B <- NULL
  }
  ans <- c(x
          , estimates = list(ests)
          , B = list(B)
          )

  # ---- Plot original fit again (over bs lines) ----
  if (bootstrapping && plot.bs) {
    lines(x
          , newdata = plotObj$predCovValues
          , col = "red"
          , lwd = 3)
  } 

  # ---- Compute confidence intervals ----      
  if( bootstrapping ){
    vec2df <- function(x, nm){
      # only for vectors of length 2, not general but fast
      xx <- data.frame(lo = x[1], hi = x[2])
      names(xx) <- paste0(nm, "_", names(xx))
      xx
    }
    abCI <- Rdistance:::bcCI(bsEsts$abundance, ests$abundance, ci)
    dnCI <- Rdistance:::bcCI(bsEsts$density, ests$density, ci)
    efCI <- Rdistance:::bcCI(bsEsts$avgEffDistance, ests$avgEffDistance, ci) 
    abCI <- vec2df(abCI, "abundance") 
    dnCI <- vec2df(dnCI, "density") 
    efCI <- vec2df(efCI, "avgEffDistance") 
    
    ans$estimates <- ans$estimates |> 
      dplyr::bind_cols( abCI ) |> 
      dplyr::bind_cols( dnCI ) |> 
      dplyr::bind_cols( efCI ) 

    # rearrange columns    
    ans$estimates <- ans$estimates |> 
      dplyr::select(id, dplyr::starts_with("density"), dplyr::starts_with("abundance"), dplyr::starts_with("avgEffDistance"), dplyr::everything())
    B <- B |> 
      dplyr::select(id, dplyr::starts_with("density"), dplyr::starts_with("abundance"), dplyr::starts_with("avgEffDistance"), dplyr::everything())
    
    if ((x$LhoodType == "parametric") && (any(is.na(bsEsts$density))) && showProgress){
      cat(paste( sum(is.na(bsEsts$density)), "of", R, "iterations did not converge.\n"))
    }
  }

  # Output
  ans$ci <- ci
  class(ans) <- c("abund", class(x))
    
  return(ans)
} 
