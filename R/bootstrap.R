#' @title Perform bootstrap iterations 
#' 
#' @description
#' Performs bootstrap resampling iterations, either in parallel across 
#' CPU cores or in serial on a single core. 
#' 
#' @inheritParams dE.single
#' 
#' @inheritParams abundEstim
#' 
#' @inheritParams oneBsIter
#' 
#' @param parallel Logical scalar. TRUE if we are running iterations 
#' in parallel across CPU cores.  Number of cores specified in `cores`.
#' 
#' @param cores Integer scalar.  The number of CPU cores to use during 
#' parallel operations, if requested.  Ignored if `parallel == FALSE`.
#' 
#' @return A data frame containing density, abundance,  
#' and other relevant statistics for every bootstrap iteration.
#' Number of rows is `R`.  If the model from one iteration failed 
#' for any reason (e.g., non-convergence), the entire row except the ID column
#' is missing.
#' 
#' @seealso [abundEstim()]; [oneBsIter()]
#' 
#' @export
bootstrap <- function(
                      object
                    , area 
                    , propUnitSurveyed 
                    , R 
                    , plot.bs 
                    , plotCovValues
                    , showProgress 
                    , parallel
                    , cores
                    ){
  
  nDigits <- ceiling(log10(R + 0.1))
  id <- 1:R
  bsData <-  data.frame(
    id = paste0("Bootstrap_",
                formatC(id
                        , format = "f"
                        , digits = 0
                        , width = nDigits
                        , flag = "0"))
  ) |> 
    dplyr::group_by(id) 
  
  # set up progress bar if called for
  if(showProgress){
    pb <- progress::progress_bar$new(
      format = paste0(R, " Bootstraps: [:bar] ETA: :eta")
      , total = R
      , clear = FALSE
    )
  } else {
    pb <- list(tick = function(){})
  }

  # start a timer
  strtTime <- Sys.time()  
  
  # Create cluster if called for
  if( parallel ){
    cat(paste0("Creating CPU cluster with "
               , colorize(cores)
               , " cores..."))
    
    cl <- multidplyr::new_cluster(cores)  
    bsData <- bsData |> 
      multidplyr::partition(cl)
    
    cat("done.\n")
    
    cat("Copying data to cores...")
    multidplyr::cluster_library(cl, "Rdistance")
    multidplyr::cluster_copy(cl, "object")
    multidplyr::cluster_copy(cl, "area")
    multidplyr::cluster_copy(cl, "propUnitSurveyed")
    multidplyr::cluster_copy(cl, "pb") # known NULL b/c showprogress known F
    multidplyr::cluster_copy(cl, "plot.bs")  # known F
    cat("done.\n")
    
    cat(paste0(R, " bootstrap iterations initiated. Standby..."))
    strtTime <- Sys.time()
  } 
  
  # --- Check we have a RdistDf inside object ----
  if(!is.RdistDf(object$data)){
    stop(paste("Input 'object$data' must be an RdistDf."
             , "Check that 'object' is a valid Rdistance fitted"
             ,"distance function."))
  }
  
  # --- Apply estimation to each ID group ----
  B <- bsData |> 
    dplyr::summarise(
      oneBsIter(  object = object
                , area = area
                , propUnitSurveyed = propUnitSurveyed
                , pb = pb
                , plot.bs = plot.bs
                , plotCovValues = plotCovValues
                , warn = FALSE
                , asymptoticSE = FALSE
      )
    )
  
  if( parallel ){
    B <- B |> 
      dplyr::collect()
  }

  # compute run time    
  runTime <- as.numeric(difftime(Sys.time(), strtTime, units = "s"))
  runTimeUnits <- "sec"
  if(runTime > 60){
    runTime <- runTime / 60
    runTimeUnits <- "min"
  }
  if(runTime > 60){
    runTime <- runTime / 60
    runTimeUnits <- "hrs"
  }
  cat(paste0("Run Time: ", round(runTime,3), " ", runTimeUnits, "\n"))

  
  if(showProgress){
    pb$terminate()
  }
  
  B
  
}
