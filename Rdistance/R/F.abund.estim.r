F.abund.estim <- function( dfunc, avg.group.size=1, group.sizes, area=1, tot.trans.len=1, n=length(dfunc$dist), 
    ci=0.95, R=500, plot.bs=FALSE, transects=NULL){
#
#   Estimate abundance using an estimated distance function
#
#   Inputs:
#   dfunc = an estimated distance function of class 'dfunc'
#   avg.group.size = average group size, from some other source.
#   
#   group.size = vector of group sizes associated with each sighted object
#   area = total area of the study area.  To estimate density, set area = 1.
#   tot.trans.len = total length of transects surveyed.  Units are square root of units of area
#   n = number of groups seen, Normally, this is length(dist), but left as a parameter 
#       so that situations where you want to apply a distance function to another set of surveys 
#       can be used. 
#   se=logical, if true, estimate standard error and ci of abundance using a bootstrap method.
#   R = number of bootstrap iterations to do when se=TRUE
#   plot = logical, if True, plot all the BS functions on top of one another
#   transects = vector delineating transects.  If not NULL, transects are resampled.  If NULL, individual 
#       observations are sampled.  If NULL, must assume sightings are independent and fundamental unit of 
#       replication. 
#
#   There are two situations:  you have avg.group.size and n; you have group.sizes for every sighted group.

#   ------------------------------
#   An internal function to plot only the line 
f.plot.bs <- function( x, xscl, yscl, ... ){
    x.seq <- seq( x$w.lo, x$w.hi, length=200)
    g.at.x0 <- x$g.x.scl
    x0 <- x$x.scl
    y <- like( x$parameters, x.seq - x$w.lo, series=x$series, expansions=x$expansions, w.lo=x$w.lo, w.hi=x$w.hi )
    f.at.x0 <- like( x$parameters, x0 - x$w.lo, series=x$series, expansions=x$expansions, w.lo=x$w.lo, w.hi=x$w.hi )
    yscl <- g.at.x0 / f.at.x0
    lines( x.seq, y * yscl, ... )
}
#   ------------------------------

if( plot.bs ){
    tmp <- plot.dfunc( dfunc )
    x.scl.plot <- tmp$xscl.plot
    y.scl.plot <- tmp$yscl
    like <- match.fun( paste( dfunc$like.form, ".like", sep=""))
}

if( missing(group.sizes) ){
    #   use avg group size times n
    n.tot <- avg.group.size * n
} else if( length(group.sizes) == n ){
    n.tot <- sum(group.sizes)
    avg.group.size <- mean(group.sizes)
} else {
    stop("Either avg.group.size or group.sizes vector must be specified")
}



esw <- ESW( dfunc )

n.hat <- n.tot * area / (2*esw*tot.trans.len)

ans <- dfunc
ans$n.hat <- n.hat
ans$n <- n
ans$area <- area
ans$esw <- esw
ans$tran.len <- tot.trans.len
ans$avg.group.size <- avg.group.size

#   Run bootstrap confidence interval, if called for
if( !is.null(ci) ){
    d.orig <- dfunc$dist
    ind <- !is.na(d.orig) & (dfunc$w.lo <= d.orig) & (d.orig <= dfunc$w.hi) 
    d.orig <- d.orig[ ind ]
    g.x.scl.orig <- dfunc$call.g.x.scl

    #print(transects)
    if( is.null( transects )){
        transects <- 1:length(d.orig)
    } else {
        transects <- transects[ind]  # incase there are NA's or distances outside lo and hi
    }
#    if( is.null(count.transects)){
#        count.transects <- transects
#    }
    #print(transects)
    unique.transects <- sort(unique( transects))
    data.list<-tapply(d.orig, transects, function(x){x})  # splits data, list item contains data from one transect 
    n.bs <- rep( NA, R) 
    
    tmp.const <- n.tot* area / (2*tot.trans.len)
    
    #   Set up a progress bar, but, if utils package is not installed, don't require it.
    if( "utils" %in% installed.packages()[,"Package"] ){
        require(utils)
        pb <- txtProgressBar( 1, R )
        show.progress=TRUE
    } else {
        show.progress=FALSE
    }
    
    cat("Computing bootstrap confidence interval on N...\n")
    #print(data.list)
    for( i in 1:R ){
        bs.trans <- sample( unique.transects, replace=TRUE )
        
        d.bs <- unlist(tapply(bs.trans, 1:length(bs.trans), FUN=function(x, df){df[[as.character(x)]]}, data.list))  # samples whole transects
        
        if( is.data.frame( g.x.scl.orig ) ){
            #   double observer data came with this fit.  Bootstrap resample it too.
            g.x.scl.bs <- g.x.scl.orig[ sample( 1:nrow(g.x.scl.orig), replace=TRUE ), ]
        } else {
            g.x.scl.bs <- g.x.scl.orig
        }
        
        #print(d.bs)
        #readline()
        
        dfunc.bs <- F.dfunc.estim(d.bs, 
                        likelihood=dfunc$like.form, 
                        w.lo=dfunc$w.lo, 
                        w.hi=dfunc$w.hi, 
                        expansions=dfunc$expansions, 
                        series=dfunc$series, 
                        x.scl=dfunc$call.x.scl, 
                        g.x.scl=g.x.scl.bs, 
                        observer=dfunc$call.observer, 
                        warn=FALSE)

        if( dfunc.bs$convergence == 0 ){  # note this also excludes solutions with parameters at their boundaries
            esw <- ESW(dfunc.bs)
            if( esw <= dfunc$w.hi ){
                
                # If you want to, resample the count transects to get variation in counts
                n.bs[i] <- tmp.const / esw
            }
            
            if( plot.bs ) f.plot.bs( dfunc.bs, x.scl.plot, y.scl.plot, col="blue", lwd=.5 )
        }
        
        if( show.progress ) setTxtProgressBar(pb, i)

    }
    if( show.progress ) close( pb )
    if( plot.bs ) f.plot.bs( dfunc, x.scl.plot, y.scl.plot, col="red", lwd=3 )
    
    #print(n.bs)
    
    #   Compute bias corrected bootstrap estimate and CI. (Manly, p. 52)
    p <- mean( n.bs > n.hat, na.rm=TRUE)
    #print(p)
    z.0 <- qnorm( 1 - p )
    #print(z.0)
    z.alpha <- qnorm( 1 - ((1 - ci)/2)) 
    #print(z.alpha)
    p.L <- pnorm( 2*z.0 - z.alpha )
    #print(p.L)
    
    p.H <- pnorm( 2*z.0 + z.alpha )
    #print(p.H)
    ans$ci <- quantile( n.bs[ !is.na(n.bs) ], p=c(p.L, p.H) )
    #print(ans$ci)
    
    ans$B <- n.bs
    if( any(is.na(n.bs)) ) cat(paste( sum(is.na(n.bs)), "of", R, "iterations did not converge.\n"))
} else {
    ans$B <- NA
    ans$ci <- c(NA, NA)   
}
ans$alpha = ci

class(ans) <- c( "abund", class(dfunc))

ans

}
