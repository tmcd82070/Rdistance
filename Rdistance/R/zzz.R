.onAttach <- function(libname, pkgname){
  
  # To automatically update the version number  
  #v <- packageVersion("Rdistance")  # this requires utils package, and I don't want to make this dependent on Utils
  
  # To manually update the version number
  v <- 1.2
  
  # Startup message
  packageStartupMessage(paste("Rdistance (version)", v ,")", sep=""))
  packageStartupMessage("\nDistance Sampling Analyses for Line Transect Data")
	packageStartupMessage("\nWEST Inc. (tmcdonald@west-inc.com)") 

}
