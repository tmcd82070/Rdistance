.onAttach<-function(libname, pkgname){

    #v <- packageVersion("Rdistance")  # this requires utils package, and I don't want to make this dependent on Utils
    
    v <- 1.1

    packageStartupMessage( paste("Rdistance - Distance and Line Transect Analysis (vers ", v ,")", sep=""))  # You have to change this every version
	packageStartupMessage("\nWEST Inc. (tmcdonald@west-inc.com)") 


}
