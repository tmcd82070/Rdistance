#' @aliases lines.dfunc
#'   
#' @title lines.dfunc - Lines method for distance (detection) functions
#'   
#' @description Lines method for objects of class '\code{dfunc}'. Distance 
#' function line methods add distance functions to existing plots. 
#'   
#' @inheritParams plot.dfunc
#' 
#' @param \dots Parameters to \code{lines} used to control attributes like 
#' color, line width, line type, etc. 
#'   
#' @return A data frame containing the x and y coordinates of the 
#' plotted line(s) is returned invisibly.
#'   
#' @seealso \code{\link{dfuncEstim}}, \code{\link{plot.dfunc}},
#'   \code{\link{print.abund}}
#'   
#' @examples 
#' set.seed(87654)
#' x <- rnorm(1000, mean=0, sd=20)
#' x <- x[x >= 0]
#' x <- units::set_units(x, "mi")
#' dfunc <- dfuncEstim(x~1, likelihood="halfnorm")
#' plot(dfunc, nbins = 40, col="lightgrey", border=NA, vertLines=FALSE)
#' lines(dfunc, col="grey", lwd=15)
#' lines(dfunc, col="black", lwd=5, lty = 2)
#' 
#' # Multiple lines 
#' data(sparrowDetectionData)
#' data(sparrowSiteData)
#' dfuncObs <- dfuncEstim(formula = dist ~ observer 
#'                      , likelihood = "halfnorm"
#'                      , detectionData = sparrowDetectionData
#'                      , siteData = sparrowSiteData)
#' plot(dfuncObs
#'    , vertLines = FALSE
#'    , lty = 0
#'    , col = c("grey","lightgrey")
#'    , border=NA
#'    , main="Detection by observer"
#'    , legend = FALSE)
#' y <- lines(dfuncObs
#'    , newdata = data.frame(observer = levels(sparrowSiteData$observer))
#'    , col = palette.colors(length(levels(sparrowSiteData$observer)))
#'    , lty = 1
#'    , lwd = 4)
#' head(y) # values returned, same as predict method
#' 
#' @export
#' @importFrom graphics lines
lines.dfunc <- function(x
                        , newdata = NULL
                        ,  ...) {
  
  x.seq <- seq(x$w.lo, x$w.hi, length = 200)
  g.at.x0 <- x$g.x.scl
  x0 <- x$x.scl
  
  y <- predict(x
             , newdata = newdata
             , distances = x.seq
             , type = "dfunc")
  
  if( x$pointSurvey ){
    y <- y * units::drop_units(x.seq - x$w.lo)
  }
  
  if( ncol(y) > 1 ){
    # newdata has >1 row
    matlines(x.seq, y, ...)
    dimnames(y)[[2]] <- paste0("y", 1:ncol(y))
    ans <- cbind(data.frame(x = x.seq), as.data.frame(y))
  } else {
    lines(x.seq, y[,1], ...)
    ans <- data.frame(x = x.seq, y )
  }

  
  # if( x$like.form == "smu"){
  #   y <- smu.like(x$parameters, x.seq - x$w.lo, x$w.hi, 
  #                 scale = FALSE, pointSurvey = FALSE)
  #   f.at.x0 <- smu.like(x$parameters, x0 - x$w.lo, x$w.hi, 
  #                       scale = FALSE, pointSurvey = FALSE)
  # } else {
  #   if(!is.null(x$covars)){
  #     covMeanMat <-  colMeans(x$covars)
  #     covMeanMat <- matrix(covMeanMat, 1) # this has the intercept
  #     
  #     BETA <- stats::coef(x)
  #     p <- ncol(x$covars)
  #     beta <- BETA[1:p]   # could be extra parameters tacked on. e.g., knee for uniform
  #     params <- covMeanMat %*% beta
  #     params <- exp(params)  # All link functions are exp...thus far
  #     if(p<length(BETA)){
  #       extraParams <- matrix(BETA[(p+1):length(BETA)], nrow(covMeanMat), length(BETA)-p, byrow=TRUE)
  #       params <- cbind(params, extraParams)
  #     }
  #     #params <- predict.dfunc(x, newdata=covMeans, type="parameters")
  #   } else {
  #     params <- matrix(x$parameters,1)
  #   }
  #   
  #   like <- match.fun( paste( x$like.form, ".like", sep=""))
  #   zero <- units::set_units(0, "m")
  #   
  #   y <- apply(params
  #              , 1
  #              , like
  #              , dist = x.seq - x$w.lo
  #              , series = x$series
  #              , covars = NULL
  #              , expansions = x$expansions
  #              , w.lo = zero
  #              , w.hi = x$w.hi - x$w.lo
  #              , pointSurvey = FALSE )  
  #   y <- t(y)  # now, each row of y is a dfunc
  #   
  #   f.at.x0 <- apply(params
  #                    , 1
  #                    , like
  #                    , dist = x0 - x$w.lo
  #                    , series = x$series
  #                    , covars = NULL
  #                    , expansions = x$expansions
  #                    , w.lo = zero
  #                    , w.hi = x$w.hi - x$w.lo
  #                    , pointSurvey = FALSE )
  # }
  # 
  # scaler <- g.at.x0 / f.at.x0 # a length n vector 
  # 
  # y <- y * scaler  # length(scalar) == nrow(y), so this works right
  # 
  # y <- t(y)
  # 
  # if( x$pointSurvey ){
  #   yscl <- units::drop_units(x.seq[2]-x.seq[1]) * sum(y[-length(y)]+y[-1]) / 2
  #   y <- y * units::drop_units(x.seq - x$w.lo) / yscl
  # }
  

  invisible(ans)
  
}
