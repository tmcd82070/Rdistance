# .onLoad ----

.onLoad <- function(libname, pkgname){

  # ** If you add an option here, MAKE SURE you add it 
  # to op.Rdist in function .onUnload below. **
  
  op <- options()
  op.Rdist <- list(
      Rdist_optimizer = "nlminb"
    , Rdist_evalMax = 2000
    , Rdist_maxIters = 1000
    , Rdist_likeTol = 1e-8
    , Rdist_coefTol = 1.5e-8
    , Rdist_hessEps = 1e-8
    , Rdist_requireUnits = TRUE
    , Rdist_maxBSFailPropForWarning = 0.2
  )
  toset <- !(names(op.Rdist) %in% names(op))
  if (any(toset)) options(op.Rdist[toset])
  
  invisible()
}

.onUnload <- function(libpath){
  
  # un-option Rdist options by full name, not just grep("Rdist_", names(op))
  # in case user has an options starting with "Rdist_".
  op.Rdist <- list(
    "Rdist_optimizer" = NULL
    , "Rdist_evalMax" = NULL
    , "Rdist_maxIters" = NULL 
    , "Rdist_likeTol" = NULL
    , "Rdist_coefTol" = NULL
    , "Rdist_hessEps" = NULL
    , "Rdist_requireUnits" = NULL
    , "Rdist_maxBSFailPropForWarning" = NULL
  )
  
  options(op.Rdist)
  
  invisible()
}


# .onAttach ----

.onAttach <- function(libname, pkgname){
  
  # Startup message
  v <- utils::packageVersion("Rdistance")  # this requires utils package, which is base
  
  packageStartupMessage(paste0(crayon::red("Rdistance"), " (v", v ,")"))
  
  invisible()
}
