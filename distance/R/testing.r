#
#   Test the distance package.
#

set.seed(29122)
x <- runif(1000) * 100
x <- rnorm(1000) * 100
x <- x[ 0 < x & x < 100 ]

x <- rnorm(1000) * 100
x <- x[ 0 < x & x < 100 ]



F.brainless.CDS( x, area=1, total.trans.len=10000, w.hi=100 )



#for( i in dev.list()) dev.off(i)

#for( like in c("negexp","halfnorm", "hazrate", "uniform","gamma")){
#    for( ser in c("cosine", "hermite", "simple")){
#        for( expan in c(0,1,2,3)){
        
#            dfunc <- F.dfunc.estim(x, likelihood=like, w.hi=100, expansions=expan, series=ser)
#            plot(dfunc, main=paste( like, ",", ser, " expansion, ", expan, " expansions", sep=""))
#            cat(paste( like, ",", ser, " expansion, ", expan, " expansions\n", sep=""))
#            cat(paste( "parameters=" ))
#            cat( dfunc$param )
#            cat(paste( "\nExit code=", dfunc$fit$convergence, "(", dfunc$fit$message, ")\n" ))
            
#            k <- readline("Enter or 'quit'> ")
#            if( k == "quit" ) break 
#        }
#        if( k == "quit" ) break 
#    }
#    if( k == "quit" ) break 
#}


