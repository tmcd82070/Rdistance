F.gx.estim <- function( fit, likelihood, w.lo ){
#
#   Estimate g0 or gx for the distance function in fit.
#

if( likelihood == "gamma" ){
    r <- fit$par[1]
    lam <- fit$par[2]
    b <- (1/gamma(r)) * (((r - 1)/exp(1))^(r - 1))
    x.scl <- lam * b * (r - 1)
    g.x <- 1    # or, eventually, h
} else {
    #   The default for all likelihoods not specified above
    x.scl <- w.lo
    g.x <- 1
}

list( x.scl = x.scl, g.x.scl = g.x )
}
