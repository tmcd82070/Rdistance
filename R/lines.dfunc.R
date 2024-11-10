#' @aliases lines.dfunc
#'   
#' @title lines.dfunc - Line plotting method for distance functions
#'   
#' @description Line plot method for objects of class '\code{dfunc}' 
#' that adds distance functions to an existing plot. 
#'   
#' @inheritParams plot.dfunc
#' 
#' @param prob Logical scaler for whether to scale the distance function 
#' to be a density (integrates to one). Default behavior is designed 
#' to be compatible with the plot method for distance functions 
#' (\code{\link{plot.dfunc}}). By default, line transect distance 
#' functions are not scaled to a density and integrate to the effective strip width. 
#' By default, point transects distance functions are scaled to be densities. 
#' 
#' @param \dots Parameters passed to \code{lines.default} that control attributes like 
#' color, line width, line type, etc. 
#'   
#' @return A data frame containing the x and y coordinates of the 
#' plotted line(s) is returned invisibly.  X coordinates in the 
#' return are names \code{x}. Y coordinates in the return are named 
#' \code{y1, y2, ..., yn}, i.e., one column per returned 
#' distance function.  
#'   
#' @seealso \code{\link{dfuncEstim}}, \code{\link{plot.dfunc}},
#'   \code{\link{print.abund}}
#'   
#' @examples 
#' set.seed(87654)
#' x <- rnorm(1000, mean=0, sd=20)
#' x <- x[x >= 0]
#' x <- units::set_units(x, "mi")
#' Df <- data.frame(transectID = "A"
#'                , distance = x
#'                 ) |> 
#'   dplyr::nest_by( transectID
#'                , .key = "detections") 
#' attr(Df, "detectionColumn") <- "detections"
#' attr(Df, "obsType") <- "single"
#' attr(Df, "transType") <- "line"
#' 
#' dfunc <- Df |> dfuncEstim(distance ~ 1, likelihood="halfnorm")
#' plot(dfunc, nbins = 40, col="lightgrey", border=NA, vertLines=FALSE)
#' lines(dfunc, col="grey90", lwd=15)
#' lines(dfunc, col="black", lwd=5, lty = 2)
#' 
#' # Multiple lines 
#' x2 <- rnorm(1000, mean=0, sd=10)
#' x2 <- x2[x2 >= 0]
#' x2 <- units::set_units(x2, "mi")
#' Df <- data.frame(transectID = c(rep("A", length(x)) 
#'                               , rep("B", length(x2)))
#'                , distance = c(x,x2)
#'                 ) |> 
#'   dplyr::nest_by( transectID
#'                , .key = "detections") 
#' attr(Df, "detectionColumn") <- "detections"
#' attr(Df, "obsType") <- "single"
#' attr(Df, "transType") <- "line"
#' 
#' dfunc <- Df |> dfuncEstim(formula = distance ~ transectID)
#' plot(dfunc
#'    , vertLines = FALSE
#'    , lty = 0
#'    , plotBars = FALSE
#'    , main="Detection by transect"
#'    , legend = FALSE)
#' y <- lines(dfunc
#'    , newdata = data.frame(transectID = unique(Df$transectID))
#'    , col = palette.colors(length(unique(Df$transectID)))
#'    , lty = 1
#'    , lwd = 4)
#' head(y) # values returned, transpose of predict method
#' 
#' @export
lines.dfunc <- function(x
                        , newdata = NULL
                        , prob = NULL
                        ,  ...) {
  
  x.seq <- seq(x$w.lo, x$w.hi, length = getOption("Rdistance_intEvalPts") )
  g.at.x0 <- x$g.x.scl
  x0 <- x$x.scl
  
  y <- stats::predict(x
             , newdata = newdata
             , distances = x.seq
             , type = "dfunc")
  
  # dfuncs are in columns.
  
  if( is.points(x) ){
    y <- y * units::set_units(x.seq - x$w.lo, NULL) # integrate to > 1
    if( is.null(prob) || (prob) ){
      y <- t( t(y) / (colSums(y, na.rm = TRUE) * units::set_units(x.seq[2] - x.seq[1], NULL))) # now y integrates to 1.0
    }
  } else {
    if( !is.null(prob) && prob ){
      y <- t( t(y) / (colSums(y, na.rm = TRUE) * units::set_units(x.seq[2] - x.seq[1], NULL))) # now y integrates to 1.0
    }
  }
  
  if( ncol(y) > 1 ){
    # newdata has >1 row
    graphics::matlines(x.seq, y, ...)
    dimnames(y)[[2]] <- paste0("y", 1:ncol(y))
    ans <- cbind(data.frame(x = x.seq), as.data.frame(y))
  } else {
    graphics::lines(x.seq, y[,1], ...)
    ans <- data.frame(x = x.seq, y )
  }

  invisible(ans)
  
}
