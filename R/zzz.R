.onAttach <- function(libname, pkgname){
  
  # To automatically update the version number  
  v <- utils::packageVersion("Rdistance")  # this requires utils package, which is base
  
  # Startup message
  packageStartupMessage(paste("Rdistance (version ", v ,")", sep=""))

}
