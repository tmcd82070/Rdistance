#' @title summary.rowwise_df - Summary method for Rdistance data frames 
#' 
#' @description Summary method for distance sampling data frames. 
#' \code{Rdistance} data frames are rowwise tibbles. This routine is a 
#' replacement summary method for rowwise_df's that  
#' provides useful distance sampling descriptive statistics. 
#' 
#' @param object An \code{RdistDf} data frame. 
#' 
#' @inheritParams dE.single
#' 
#' @param ...  Other arguments for summary methods.
#' 
#' @return If \code{object} is an \code{RdistDf}, a data frame 
#' containing summary statistics relevant to distance sampling is returned
#' invisibly.
#' If \code{formula} is not specified, the number of distance observations
#' and target detections is not returned because the distances, group sizes,
#' and covariates are not known. 
#' If \code{object} is not an Rdistance data frame, return is the result of 
#' the next summary method.
#' 
#' @examples
#' data(thrasherDf)
#' summary(thrasherDf)
#' summary(thrasherDf
#'         , formula = dist ~ groupsize(groupsize)
#'         , w.hi = units::set_units(100,"m")
#'         )
#' 
#' @export
#' 
summary.rowwise_df <- function(object
                             , formula = NULL
                             , w.lo = 0
                             , w.hi = NULL
                             , ...){
  
  if( is.RdistDf(object) ){
    tranType <- Rdistance::transectType(object)
    effVec <- Rdistance::effort(object)
    nTrans <- sum(!is.na(effVec))
    lenTrans <- sum(effVec, na.rm = TRUE)
    if( !is.null(formula) ){
      mf <- Rdistance::parseModel(data = object
                               , formula = formula
                               , w.lo = w.lo
                               , w.hi = w.hi
                               )
      groupSz <- Rdistance::groupSizes(mf)
      dists <- Rdistance::distances(mf)
      
      nDistances <- sum( !is.na(dists) )
      nTargets <- sum(groupSz, na.rm = TRUE)
      nGroups <- sum( !is.na(groupSz) )
    } else {
      nDistances <- NA
      nTargets <- NA
      nGroups <- NA
    }
    
    cat(paste("Transect type: "))
    if( Rdistance::is.points(object) ){
      tType <- "point"
      cat(colorize(tType))
      lenMess <- paste(lenTrans, "[points]")
    } else {
      tType <- "line"
      cat(colorize(tType))
      lenMess <- format(lenTrans)
    }
    cat("\n")
    
    cat("Effort:\n ")
    cat(paste("  " 
            , format( c("Transects:"
                      , "Total length:")
                    , justify = "right")
            , colorize(format( c(nTrans, lenMess)))
            , "\n"
            ))
    
    if(!is.null(formula)){
      cat("Distances:\n ")
      cat(paste0("  "
                , colorize(format(mf$w.lo))
                , " to "
                , colorize(format(mf$w.hi))
                , ": "
                , colorize(nDistances)
                , "\n"))
      cat("Sightings:\n ")
      cat(paste("  "
                 , format(c("Groups:", "Individuals:"), justify = "right")
                 , colorize(c(nGroups, nTargets))
                 , "\n"))
      
    } else {
      cat(colorize("Specify 'formula', 'w.lo', and 'w.hi' to obtain distances, groups, and individuals."))
    }
    
    res <- data.frame(
      statistic = c(
        "transectType"
        , "transects"
        , "transectLength"
        , "distances"
        , "groups"
        , "individuals"
      ),
      value = c(
        tType
        , nTrans
        , lenMess
        , nDistances
        , nGroups
        , nTargets
      ))
  } else {
    res <- NextMethod("summary", object)
  }
  invisible(res)
}