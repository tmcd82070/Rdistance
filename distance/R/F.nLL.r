F.nLL <- function(a, dist, like, w.lo=0, w.hi=max(dist), series, expansions=0){

    f.loglike <- match.fun(paste( like, ".like", sep=""))

    LL <- f.loglike( a, dist, w.lo=w.lo, w.hi=w.hi, series, expansions )

    LL[ !is.na(LL) & (LL <= 0) ] <- 1e-6   # happens at very bad values of parameters

    nLL <- -sum(log(LL), na.rm=T)  # Note that distances > w in LL are set to NA
    nLL
}
