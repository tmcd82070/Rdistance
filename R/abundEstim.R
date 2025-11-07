#' @title Distance Sampling Abundance Estimates 
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
#'  Estimation of site-specific density (e.g., on every transect) is accomplished by 
#'  \code{predict(x, type = "density")}, which returns a 
#'  tibble containing density and abundance on the area surveyed by every
#'  transect. 
#'   
#' @section Bootstrap Confidence Intervals:
#' 
#'   Rdistance's nested data frames (produced by \code{\link{RdistDf}})
#'   contain all information required to estimate bootstrap CIs. 
#'   To compute bootstrap CIs, Rdistance resamples, with replacement,
#'   the rows of the \code{$data} component contained in Rdistance 
#'   fitted models. Rdistance assumes each row of \code{$data} 
#'   contains one information on on transect.
#'   The \code{$data} component also contains 
#'   information on which observations go into the 
#'   detection functions, which should be counted as detected targets, 
#'   and which count toward transect length. 
#'   After resampling rows of \code{$data}, Rdistance 
#'   refits the distance function using non-missing distances, 
#'   recomputes the detected number of targets using non-missing 
#'   group sizes on transects with non-missing length, 
#'   and re-computes total transect length from transects 
#'   with non-missing lengths. 
#'   By default, \code{R} = 500 bootstrap iterations are 
#'   performed, after which bias
#'   corrected confidence intervals are computed (Manly, 1997, section 3.4).
#'   
#'   The distance function is not re-selected during bootstrap resampling. The 
#'   model of the input object is re-fitted every iteration.  
#'   
#'   During bootstrap iterations, the distance function can fail. 
#'   An iteration can fail for a two reasons:
#'   (1) no detections on the iteration, and (2) a bad configuration 
#'   of distances that push the distance function's parameters to their 
#'   limits. When an iteration fails, Rdistance 
#'   skips the iteration and effectively ignores the 
#'   failed iterations. 
#'   If the proportion of failed iterations is small 
#'   (less than 20% by default), the resulting abundance confidence interval 
#'   is probably valid and no warning is issued.  If the proportion of 
#'   non-convergent iterations 
#'   is not small (exceeds 20% by default), a warning is issued.  
#'   The warning can be modified  
#'   by re-setting the \code{Rdistance_maxBSFailPropForWarning} option. 
#'   Setting \code{options(Rdistance_masBSFailPropForWarning = 1.0)} will turn 
#'   off the warning. 
#'   Setting \code{options(Rdistance_masBSFailPropForWarning = 0.0)} will 
#'   warn if any iteration failed.  Results (density and effective 
#'   sampling distance) 
#'   from all successful iterations are contained in the  
#'   non-NA rows of data frame 'B' in the output object.  
#'   
#' @section Missing Transect Lengths:
#' 
#'   Transect lengths can be missing in the RdistDf object. 
#'   Missing length transects are equivalent
#'   to 0 [m] transects and do not count toward total surveyed units
#'   nor to group sizes on these transects count toward total 
#'   detected individuals.  
#'   Use NA-length transects to include their associated distances 
#'   when estimating the distance function, but not when estimating abundance. 
#'   For example, this allows estimation of abundance on one 
#'   study area using off-transect distances from another.  This allows 
#'   sightability to be estimated using two or more similar targets (e.g., 
#'   two similar species), but abundance to be estimated separate for each 
#'   target type.
#'   Include NA-length transects by including the "extra" distance observations 
#'   in the detection data frame, with valid
#'   site IDs, but set the length of those site IDs to NA in the site data frame. 
#'
#' @section Point Transect Lengths:
#'   Point transects do not have a physical measurement for length. 
#'   The "length" of point transects is the number of points on the transect. 
#'   Point transects can contain only one point.  
#'   Rdistance treats transects of points as independent 
#'   and bootstrap resamples them to estimate variance. The number of points
#'   on each point transect must exist in the RdistDf and cannot have 
#'   physical measurement units (it is a count, not a distance).
#'   
#'   
#' @return An Rdistance 'abundance estimate' object, which is a list of
#'   class \code{c("abund", "dfunc")}, containing all the components of a "dfunc"
#'   object (see \code{\link{dfuncEstim}}), plus the following: 
#'   
#'   \item{estimates}{A tibble containing fitted coefficients in the
#'   distance function, density in the area(s) surveyed, 
#'   abundance on the study area, the number of groups seen 
#'   between w.lo and w.hi, the number 
#'   of individuals seen between w.lo and w.hi, 
#'   study area size, surveyed area, average group size, and 
#'   average effective detection distance.
#'   }
#'   
#'   \item{B}{If confidence intervals were requested, a tibble 
#'   containing all bootstrap values of coefficients, 
#'   density, abundance, groups seen, individuals seen, 
#'   study area size, surveyed area size, average group size, 
#'   and average effective detection distance.  The number of rows is always 
#'   \code{R}, the requested number of bootstrap 
#'   iterations.  If an iteration fails, the
#'   corresponding row in \code{B} is \code{NA} (hence, use 'na.rm = TRUE' 
#'   when computing summaries). Columns 1 through \code{length(coef(dfunc))}
#'   contain bootstrap realizations of the distance function's coefficients. 
#'   }
#'   
#'   \item{ci}{Confidence level of the confidence intervals}
#'   
#' @references Manly, B.F.J. (1997) \emph{Randomization, bootstrap, and 
#'   Monte-Carlo methods in biology}, London: Chapman and Hall.
#'   
#'   Buckland, S.T., D.R. Anderson, K.P. Burnham, J.L. Laake, D.L. Borchers,
#'    and L. Thomas. (2001) \emph{Introduction to distance sampling: estimating
#'    abundance of biological populations}. Oxford University Press, Oxford, UK.
#'   
#' @seealso \code{\link{dfuncEstim}}, \code{\link{autoDistSamp}}, 
#'  \code{\link{predict.dfunc}} with 'type = "density"'.
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
#'            , w.hi=150 %m%.
#'   )
#' 
#' # Estimate abundance - Convenient for programming 
#' abundDf <- estimateN(dfunc
#'                    , area = 4105 %km^2%.
#'            )
#' 
#' # Same - Nicer output 
#' # Set ci=0.95 (or another value) to estimate bootstrap CI's 
#' fit <- abundEstim(dfunc
#'                 , area = 4105 %km^2%.
#'                 , ci = NULL
#'                 )
#'          
#' @export
abundEstim <- function(object
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
    graphics::par( xpd=TRUE )
    plotObj <- plot(object)
  }
  
  # ---- Set Area ----
  if( is.null(area) ){
    # doing this here saves a tiny sliver of time in estimateN
    area <- setUnits(1, object$outputUnits)^2
  }
  
  # ---- Construct Original estimates frame ----
  ests <- estimateN(object = object
                  , area = area
                  , propUnitSurveyed = propUnitSurveyed
                    )
  ests$id <- "Original"
  
  # ---- Prelims constants for bootstrapping ----
  nDataRows <- nrow(object$data)
  # bsData <- data.frame(id = "Original",
  #                      rowIndex = 1:nDataRows)

  pb <- list(tick = function(){}) # NULL tick function for not bootstrapping

  # ---- Add bootstrap indices if called for ----
  # This is essentially what rsample::bootstraps does, but rsample stores the 
  # entire data frame in each 'rsample'.  This saves space. But, entire 
  # (R*nrow(x$data)+1) X 2 data frame must be constructable in memory.
  if ( bootstrapping ) {
    
    nDigits <- ceiling(log10(R + 0.1))
    id <- rep(1:R, each = nDataRows)
    bsData <-  data.frame(
                id = paste0("Bootstrap_",
                            formatC(id
                                    , format = "f"
                                    , digits = 0
                                    , width = nDigits
                                    , flag = "0"))
              , rowIndex = sample( 1:nDataRows
                           , size = R*nDataRows
                           , replace = TRUE
                          ))

    # set up progress bar if called for, only if bootstrapping
    if(showProgress){
      pb <- progress::progress_bar$new(
          format = paste0(R, " Bootstraps: [:bar] Run Time: :elapsedfull")
        , total = R+1
        , show_after = 1
        , clear = FALSE
      )
    }

    # --- Apply estimation to each ID group ----
    B <- bsData |> 
      dplyr::group_by(id) |> 
      dplyr::group_modify(.f = oneBsIter # oneBsIter is in Rdistance, not exported
                        , data = object$data
                        , formula = object$formula  
                        , likelihood = object$likelihood 
                        , w.lo = object$w.lo
                        , w.hi = object$w.hi
                        , expansions = object$expansions
                        , series = object$series
                        , x.scl = object$x.scl 
                        , g.x.scl = object$g.x.scl
                        , outputUnits = object$outputUnits
                        , warn = FALSE
                        , asymptoticSE = FALSE
                        , area = area
                        , propUnitSurveyed = propUnitSurveyed
                        , pb = pb
                        , plot.bs = plot.bs
                        , plotCovValues = plotObj$predCovValues
      )
    
    if(showProgress){
      pb$terminate()
    }
    
    # Replace varcovar with bootstrap varcovar
    bsCoefs <- B |> 
      dplyr::ungroup() |> 
      dplyr::select(dplyr::all_of(names(stats::coef(object))))
    object$varcovar <- stats::var(bsCoefs)
    

  } else {
    ci <- NA
    B <- NULL
  }
  
  # ---- Construct output object ----
  ans <- c(object
          , estimates = list(ests)
          , B = list(B)
          )
  
  # ---- Plot original fit again (over bs lines) ----
  if (bootstrapping && plot.bs) {
    graphics::lines(object
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
    abCI <- Rdistance::bcCI(B$abundance, ests$abundance, ci)
    dnCI <- Rdistance::bcCI(B$density, ests$density, ci)
    efCI <- Rdistance::bcCI(B$avgEffDistance, ests$avgEffDistance, ci) 
    abCI <- vec2df(abCI, "abundance") 
    dnCI <- vec2df(dnCI, "density") 
    efCI <- vec2df(efCI, "avgEffDistance") 
    
    ans$estimates <- ans$estimates |> 
      dplyr::bind_cols( abCI ) |> 
      dplyr::bind_cols( dnCI ) |> 
      dplyr::bind_cols( efCI ) 

    # rearrange columns    
    ans$estimates <- ans$estimates |> 
      dplyr::select(id
                  , dplyr::all_of(names(stats::coef(object)))  
                  , dplyr::starts_with("density")
                  , dplyr::starts_with("abundance")
                  , dplyr::starts_with("avgEffDistance")
                  , dplyr::everything())
    B <- B |> 
      dplyr::select(id
                  , dplyr::all_of(names(stats::coef(object)))  
                  , dplyr::starts_with("density")
                  , dplyr::starts_with("abundance")
                  , dplyr::starts_with("avgEffDistance")
                  , dplyr::everything())
    
    if ((object$LhoodType == "parametric") && 
        (any(is.na(B$density))) && 
        showProgress){
      cat(paste( sum(is.na(B$density)), "of", R
                 , "iterations did not converge.\n"))
    }
  }

  # Output
  ans$ci <- ci
  class(ans) <- c("abund", class(object))
    
  return(ans)
} 
