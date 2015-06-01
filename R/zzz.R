.onAttach <- function(libname, pkgname){
  
  # To automatically update the version number  
  v <- packageVersion("Rdistance")  # this requires utils package, which is base
  
  # To manually update the version number
  #v <- "1.2.2"
  
  # Startup message
  packageStartupMessage(paste("Rdistance (version ", v ,")", sep=""))
  #packageStartupMessage("Distance Sampling Analyses for Line Transect Data")
	#packageStartupMessage("WEST Inc. (tmcdonald@west-inc.com)") 

}
