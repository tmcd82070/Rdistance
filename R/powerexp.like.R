powerexp.like <- function(a, dist, w.lo=0, w.hi=max(dist), series="cosine", expansions=0, scale=TRUE){
#   Computes likelihood like an inverted normal but estimates the power, 
#   
#   equation is 1 - exp(-(dist/s)^(-k))
#
#


    dist[ (dist < w.lo) | (dist > w.hi) ] <- NA

    sigma <- a[1]
    k <- a[2]
    key <- 1 - exp(-(dist/sigma)^(-k))
    dfunc <- key
    w <- w.hi - w.lo
	
    # If there are expansion terms
    if(expansions > 0){

        nexp <- min(expansions,length(a)-2)  # should be equal. If not, fire warning next
        
        if( length(a) != (expansions+2) ) {
            warning("Wrong number of parameters in expansion. Should be (expansions+1). High terms ignored.")
        }

		if (series=="cosine"){
            dscl = dist/w
            exp.term <- cosine.expansion( dscl, nexp )
		} else if (series=="hermite"){
            dscl = dist/sigma
            exp.term <- hermite.expansion( dscl, nexp )
		} else if (series == "simple") {
            dscl = dist/w
            exp.term <- simple.expansion( dscl, nexp )
        } else {
            stop( paste( "Unknown expansion series", series ))
        }

        dfunc <- key * (1 + (exp.term %*% a[3:(nexp+2)]))


    } else if(length(a) > 1){
        warning("Wrong number of parameters in powerexp. Only 2 needed if no expansions. High terms ignored.")
    }

    if( scale ){
        dfunc = dfunc / integration.constant(powerexp.like, w.lo=w.lo, w.hi=w.hi, a=a,series=series,expansions=expansions)  
    }
    
    

    c(dfunc)
    
}
