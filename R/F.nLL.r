F.nLL <- function(a, dist, covars = NULL, like, w.lo=0, w.hi=max(dist), series, expansions=0, point.transects, for.optim = F){

    f.like <- match.fun(paste( like, ".like", sep=""))
   
    L <- f.like( a = a, dist = dist, covars = covars, w.lo = w.lo, w.hi = w.hi, series = series, expansions = expansions, point.transects = point.transects)
    
    if(for.optim){
      L <- L*10^9
    }
    
    #print(L)
    L[ !is.na(L) & (L <= 0) ] <- 1e-6   # happens at very bad values of parameters

    nLL <- -sum(log(L), na.rm=TRUE)  # Note that distances > w in LL are set to NA
    #print(nLL)
    nLL
}
