F.abund.estim <- function( dfunc, group.size=1, area=1, tot.trans.len=1 ){
#
#   Estimate abundance using an estimated distance function
#
#   Inputs:
#   dfunc = an estimated distance function of class 'dfunc'
#   group.size = vector of group sizes associated with each sighted object
#   area = total area of the study area.  To estimate density, set area = 1.
#   tot.trans.len = total length of transects surveyed.
#


if( length( group.size ) == 1 ){
    group.size <- rep( group.size, length(dist) )
}

n <- sum( group.size )
esw <- ESW( dfunc )

n.hat <- n * area / (2*esw*tot.trans.len)

ans <- dfunc
ans$n.hat <- n.hat
ans$se.n.hat <- NA

class(ans) <- c( "abund", class(dfunc))

ans

}
