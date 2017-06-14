F.nLL <- function(a, dist, covars = NULL, like, w.lo=0, w.hi=max(dist), series, expansions=0){

    f.like <- match.fun(paste( like, ".like", sep=""))

    L <- f.like( a = a, dist = dist, covars = covars, w.lo = w.lo, w.hi = w.hi, series = series, expansions = expansions)

    L[ !is.na(L) & (L <= 0) ] <- 1e-6   # happens at very bad values of parameters

    nLL <- -sum(log(L), na.rm=TRUE)  # Note that distances > w in LL are set to NA
    #print(nLL)
    nLL
}
