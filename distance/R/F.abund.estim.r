F.abund.estim <- function( dfunc, avg.group.size=1, group.sizes, area=1, tot.trans.len=1, n=length(dfunc$dist) ){
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

#   There are two situations:  you have avg.group.size and n; you have group.sizes for every sighted group.

if( missing(group.sizes) ){
    #   use avg group size times n
    n.tot <- avg.group.size * n
} else if( length(group.sizes) == n ){
    n.tot <- sum(group.sizes)
} else {
    stop("Either avg.group.size or group.sizes vector must be specified")
}

esw <- ESW( dfunc )

n.hat <- n.tot * area / (2*esw*tot.trans.len)

ans <- dfunc
ans$n.hat <- n.hat
ans$se.n.hat <- NA   #  Need to get this from the book.  Make a summary method that uses bootstrapping. 

class(ans) <- c( "abund", class(dfunc))

ans

}
