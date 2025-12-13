#' @title Perform non-parallel bootstrap iterations 
bootstrap <- function(bsData
                            , object
                            , area 
                            , propUnitSurveyed 
                            , R 
                            , plot.bs 
                            , plotCovValues
                            , showProgress 
                            ){
  
  # nDataRows <- nrow(object$data)
  # nDigits <- ceiling(log10(R + 0.1))
  # id <- rep(1:R, each = nDataRows)
  # bsData <-  data.frame(
  #   id = paste0("Bootstrap_",
  #               formatC(id
  #                       , format = "f"
  #                       , digits = 0
  #                       , width = nDigits
  #                       , flag = "0"))
  #   , rowIndex = sample( 1:nDataRows
  #                        , size = R*nDataRows
  #                        , replace = TRUE
  #   ))
  
  # set up progress bar if called for
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
    dplyr::group_modify(.f = oneBsIter 
                        , object = object
                        # , data = object$data
                        # , formula = object$formula  
                        # , likelihood = object$likelihood 
                        # , w.lo = object$w.lo
                        # , w.hi = object$w.hi
                        # , expansions = object$expansions
                        # , series = object$series
                        # , x.scl = object$x.scl 
                        # , g.x.scl = object$g.x.scl
                        # , outputUnits = object$outputUnits
                        , warn = FALSE
                        , asymptoticSE = FALSE
                        , area = area
                        , propUnitSurveyed = propUnitSurveyed
                        , pb = pb
                        , plot.bs = plot.bs
                        , plotCovValues = plotCovValues
    )
  
  # dplyr::collect() works in both parallel and non-parallel
  B <- B |> 
    dplyr::collect()
  
  if(showProgress){
    pb$terminate()
  }
  
}