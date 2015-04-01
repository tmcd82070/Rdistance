.onAttach <- function(libname, pkgname){
  
  # Automatic version number update
  #v <- packageVersion("Rdistance")  # this requires utils package, and I don't want to make this dependent on Utils  
  
  # Manual version number update (you have to change version number yourself)
  v <- 1.2
  
  # Printed message
  packageStartupMessage(
    cat("Rdistance (version ", v, ")", "\n",
          "Distance Sampling Analyses for Line Transect Data", "\n",
          "WEST Inc. (tmcdonald@west-inc.com)",
          sep="")
    )
}
