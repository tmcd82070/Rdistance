uniform.nLL <- function(a, dist, w.lo=0, w.hi=max(dist), series, expansions=0){
#
#   Compute the negative log likelihood assuming uniform density.
#
#   Input:
#   a = parameter values. Length and meaning depend on series and expansions.
#       a must be at least length = 1.  a[1] is upper end of support for uniform distribution. 
#       i.e., density is uniform on [0, a[1]].  a[2:length(a)] are coefficients 
#       of expansion terms.
#   dist = input observed distance data
#   w = right truncation value, same units as dist
#   series = character values specifying type of expansion.  Currently only
#       "cosine" and "Hermite" work. Default is no series expansions.
#   expansions = number of expansion terms.  This controls whether series
#       expansion terms are fitted. Default = 0 does not fit any terms.
#       If >0, fit the number of expansion terms specified of the type
#       specified by series.  Max terms depends on series.
#
#   Output:
#   sum of negative log likelihood values for all observations in dist.
#   Note this is the objective function to be minimized to get max likelihood estimates
#   of parameters.
#	

    LL = uniform.like( a, dist, w.lo=w.lo, w.hi=w.hi, series=series, expansions=expansions )

    if( any(!is.na(LL) & (LL <= 0)) ) LL[ LL <= 0 ] <- 1e-6   # happens at very bad values of parameters

    nLL <- -sum(log(LL), na.rm=T)  # Note that distances > w in LL are set to NA
    nLL
}
