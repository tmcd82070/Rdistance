powerexp.start.limits <- function(dist, expansions, w.lo, w.hi){
  np <- expansions + 2
  
  start <- c(sqrt(sum( (dist - w.lo)^2 )/length(dist)), 2, rep(0, np - 2))
  low   <- c(min(dist,na.rm=TRUE)/2, 0.1, rep(-Inf, np - 2 ))
  high  <- c(Inf, 100, rep( Inf, np - 2 ))
  nms <- c("Sigma","K")
  if(expansions > 0) nms <- c(nms, paste( "a", 1:(np-2), sep=""))
  
  list(start=start,
       lowlimit=low,
       highlimit=high,
       names=nms)
}
