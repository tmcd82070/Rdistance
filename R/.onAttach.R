.onAttach <- function(libname, pkgname){
  
  # To automatically update the version number  
  v <- utils::packageVersion("Rdistance")  # this requires utils package, which is base
  
  # Startup message
  packageStartupMessage(paste("Rdistance (version ", v ,")", sep=""))

  # Default options
  options(list(
      Rdist_optimizer = "nlminb"
    , Rdist_evalMax = 2000
    , Rdist_maxIters = 1000
    , Rdist_likeTol = 1e-8
    , Rdist_coefTol = 1.5e-8
    , Rdist_hessEps = 1e-8
    , Rdist_requireUnits = TRUE
    , Rdist_maxBSFailPropForWarning = 0.2
    )
  )
}
