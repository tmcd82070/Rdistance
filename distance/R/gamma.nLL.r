gamma.nLL <- function(a, dist, w.lo=0, w.hi=max(dist), series="cosine", expansions=0){
#
#   Compute the negative log likelihood for hazard rate density.
#
#   Input:
#   a = parameter values.
#   dist = observed distance data
#   w = right truncation value, same units as dist
#   series, expansions = NOT USED
#
#   Output:
#   sum of negative log likelihood values for all observations in dist.
#   Note this is the objective function to be minimized to get max likelihood estimates
#   of parameters.
#	


    LL = gamma.like( a, dist, w.lo, w.hi )
    
    if( any(!is.na(LL) & (LL <= 0)) ) LL[ LL <= 0 ] <- 1e-6   # happens at very bad values of parameters

    nLL <- -sum(log(LL), na.rm=T)  # Note that distances > w in LL are set to NA
    nLL
}
