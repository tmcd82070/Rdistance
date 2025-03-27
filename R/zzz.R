# .onLoad ----

.onLoad <- function(libname, pkgname){

  # ** If you add an option here, MAKE SURE you add it 
  # to op.Rdist in function .onUnload below. **
  
  op <- options()
  op.Rdist <- list(
      Rdistance_optimizer = "nlminb"
    , Rdistance_evalMax   = 2000
    , Rdistance_maxIters  = 1000
    , Rdistance_likeTol   = (.Machine$double.eps)^(1/1.75)
    , Rdistance_coefTol   = (.Machine$double.eps)^(1/2)
    , Rdistance_hessEps   = (.Machine$double.eps)^(1/1.75)
    , Rdistance_requireUnits = TRUE
    , Rdistance_maxBSFailPropForWarning = 0.2
    , Rdistance_negInf    = -1 / .Machine$double.neg.eps
    , Rdistance_posInf    =  1 / .Machine$double.neg.eps
    , Rdistance_fuzz      = .Machine$double.eps
    , Rdistance_zero      = .Machine$double.eps
    , Rdistance_warn      = FALSE
    , Rdistance_intEvalPts= 101  # MUST BE ODD (for Simpson's rule)
  )
  toset <- !(names(op.Rdist) %in% names(op))
  if (any(toset)) options(op.Rdist[toset])
  
  invisible()
}

.onUnload <- function(libpath){
  
  # un-option Rdist options by full name, not just grep("Rdistance_", names(op))
  # in case user has an options starting with "Rdistance_".
  op.Rdist <- list(
      "Rdistance_optimizer" = NULL
    , "Rdistance_evalMax"   = NULL
    , "Rdistance_maxIters"  = NULL 
    , "Rdistance_likeTol"   = NULL
    , "Rdistance_coefTol"   = NULL
    , "Rdistance_hessEps"   = NULL
    , "Rdistance_requireUnits" = NULL
    , "Rdistance_maxBSFailPropForWarning" = NULL
    , "Rdistance_negInf"    = NULL
    , "Rdistance_posInf"    = NULL
    , "Rdistance_fuzz"      = NULL
    , "Rdistance_zero"      = NULL
    , "Rdistance_warn"      = NULL
    , "Rdidtance_intEvalPts"= NULL
  )
  
  options(op.Rdist)
  
  invisible()
}


# .onAttach ----

.onAttach <- function(libname, pkgname){
  
  # Startup message
  v <- utils::packageVersion("Rdistance")  # this requires utils package, which is base
  
  packageStartupMessage(paste0(crayon::bold(
                                crayon::blue("Rdistance")
                               ), " (v", v ,")"))
  
  invisible()
}
