.onAttach<-function(libname, pkgname){

    #v <- packageVersion("Rdistance")  # this requires utils package, and I don't want to make this dependent on Utils
    
    v <- 1.2

    packageStartupMessage( paste("Rdistance - Distance Sampling Analyses for Line Transect Data (vers ", v ,")", sep=""))  # You have to change this every version
	packageStartupMessage("\nWEST Inc. (tmcdonald@west-inc.com)") 


}
