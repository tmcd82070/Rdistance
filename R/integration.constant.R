integration.constant <- function( density, w.lo, w.hi, covars, ... ){
#
#   Return the scalar so that integral from 0 to w of underlying density
#   is 1.0
#
#   Input:
#   density = a function to compute integration constant for.
#       this function must be capable of evaluating values from 0 to w
#   w = upper limit of integral.
#   ... = additional arguments to density.  These vary by density function,
#       but generally are parameter values, series, expansion terms, etc.
#
#   Output:
#   a divisor scalar such that density / scalar integrates to 1.0. i.e.,
#   this output scalar is the integral of unscaled density from 0 to w.
#

  density = match.fun(density)
  seqx = seq(w.lo, w.hi, length=200)
  
  if(!is.null(covars)){
    unique.covars <- unique(covars)
    temp.covars <- matrix(nrow = length(seqx), ncol = ncol(unique.covars))
    seqy <- list()
    temp.scaler <- vector(length = nrow(unique.covars))
    scaler <- vector(length = nrow(covars), "numeric")
    for(i in 1:nrow(unique.covars)){
      for(j in 1:length(seqx)){
        temp.covars[j,] <- unique.covars[i,]
      }
      seqy[[i]] <- density( dist = seqx, covars = temp.covars, scale=FALSE, w.lo=w.lo, w.hi=w.hi,...)
      temp.scaler[i] <- (seqx[2]-seqx[1]) * sum(seqy[[i]][-length(seqy[[i]])]+seqy[[i]][-1]) / 2
    }
    df <- data.frame(unique.covars,temp.scaler)
    z <- merge(covars, df, by.x = names(as.data.frame(covars)), by.y = names(df[, names(df) != "temp.scaler"]), sort = F)
    scaler <- z$temp.scaler
  }
  else{
    seqy = density( dist=seqx, scale=FALSE, w.lo=w.lo, w.hi=w.hi,...)
    
    #   Trapazoid rule
    scaler= (seqx[2]-seqx[1]) * sum(seqy[-length(seqy)]+seqy[-1]) / 2
  }
  #print("working...")
  scaler
  
}
