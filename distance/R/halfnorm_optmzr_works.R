
##-----------------------------------------------------------------------------
###here's a way to check how it is working in estimating the sd, if ex=0------------------
###----------------------------------------------------------------------------

#   This is Trent and Patrick's testing program. 

set.seed(39820834) 

reps=10

sd.optz=sd.opt=sd.nlm=sd.the=nlm.con=matrix(reps*2, nrow=reps, ncol=2)

#sink("dat2dist.txt")

for(i in 1:reps){
    cat("----------------------------------------\n")
    sd=20
    n=60  #below ~n=60, the optimize routines start to break down.
	       #it gives results similar to the seq.x results I've been getting
    lowlimit <- 0.001

    distances=rnorm(n,0,sd)
    distances=distances[distances>0]
    
    w=max(distances)
    cat(paste("W=",w,"\n"))
    ex=0

    # --- Optimize
    # This seems to work better than others for uniform distribution 
    optimize.param=optimize(hazrate.nLL,interval=c(lowlimit,w),series=ser,
		expansions=ex,dist=distances)
    
    sd.optz[i,1]=optimize.param$minimum
    sd.optz[i,2]=optimize.param$objective

    cat( paste( "Optimize:", sd.optz[i,1], sd.optz[i,2], "\n"))

    # --- Optim
    optimize.param=optim( c(0.5*w, 0),hazrate.nLL,method="L-BFGS-B",hessian=T, lower=lowlimit, 
		upper=.98*w,control=list(trace=0,maxit=1000),
        series=ser,expansions=ex, dist=distances)
    
    sd.opt[i,1]=optimize.param$par[1]
    sd.opt[i,2]=optimize.param$value

    cat( paste( "Optim   :", sd.opt[i,1], sd.opt[i,2], optimize.param$message, "\n"))


    # --- nlminb
    fit <- F.dfunc.estim( distances, likelihood="hazrate" )
    #optimize.param=nlminb( c(0.5*w),halfnorm.nLL,hessian=T, lower=lowlimit, 
	#	upper=w,control=list(trace=0,iter.max=1000),
    #    series=ser,expansions=ex, dist=distances)
    
    sd.nlm[i,1]=fit$parameters[1]
    sd.nlm[i,2]=fit$loglik

    cat( paste( "Nlminb  :", sd.nlm[i,1], sd.nlm[i,2], fit$convergence, "\n"))
    cat( fit$fit$message )
    
    plot(fit)
    menu(c(0,1))

    # ---- Write to file
    #cat( paste( i, distances, "\n" ))
    
}

#sink()


