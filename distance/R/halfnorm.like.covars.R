halfnorm.like.covars <- function(a, dist, w=max(dist), series="cosine", expansions=0, scale=TRUE, covars=NULL, formla=~1){
#   Computes half norm likelihood, scaled appropriately to integrate to 1.0, for every
#   observation. I.e., returns a vector. 
#
#   Input:
#   a = parameter values. Length and meaning depend on series and expansions and covariates
#   dist = input observed distance data
#   w = right truncation value, same units as dist
#   series = character values specifying type of expansion.  Currently only 
#       "cosine" and "hermite" work. Default is no series expansions.
#   expansions = number of expansion terms.  This controls whether series 
#       expansion terms are fitted. Default = 0 does not fit any terms. 
#       If >0, fit the number of expansion terms specified of the type 
#       specified by series.  Max terms depends on series.
#   scale = logical, whether or not to scale the likelihood values so 
#       that the underlying density integrates to 1. This parameter is used 
#       to stop the recursion. 
#   
#   Output:
#   A vector same length as dist, containing likelihood values, scaled appropriately. 
#   Likelihood for all distances >w are set to NA
#


    dist[ dist > w ] = NA
    dscl = dist/w
        
    # Convential Distance Analysis
    if(missing(formla)){
        sigma = a[1]    # a[1] is always constrained to be >= w/100 in DISTANCE
        key = exp(-dist^2/(2*sigma^2))
        dfunc = key
       
        # If there are expansion terms
        if(expansions > 0){
            if (series=="cosine"){
                expansion = F.cosine(x=dscl, expansions=expansions)
            }
            if (series=="hermite"){
                expansion = F.hermite(x=dscl, expansions=expansions)
            }
            for(i in 1:expansions){
                dfunc = dfunc + key*a[i+1]*expansion[[i]]
            }
        }

    } 
    
    # Analysis with covariates
    if(!missing(formla)){
        betas = length(a) - expansions
        sigma = F.covars(sigma, dist=dist, beta=beta, covars=covars)
        key = exp(-dist^2/(2*sigma^2))
        dfunc = key
    
        # If there are expansion terms
        if(expansions > 0){
            if (series=="cosine"){
                expansion = F.cosine(x=dscl, expansions=expansions)
            }
            if (series=="hermite"){
                expansion = F.hermite(x=dscl, expansions=expansions)
            }
            for(i in 1:expansions){
                dfunc = dfunc + key*a[length(a)-expansions+1]*expansion[[i]]
            }
        }

    } 
        
    if( scale ){
        dfunc = dfunc / integration.constant(halfnorm.like,w,a=a,series=series,expansions=expansions)   # scales underlying density to integrate to 1.0
    }

    dfunc

}


nlminb(start=c(0.5*w, 0), halfnorm.nLL, hessian=T, 
    control=list(trace=0,iter.max=1000, eval.max=500), 
    dist=dst, w=w, lower=c(10, -Inf), upper=c(w, Inf),
    covars=data.frame(agl=goea$agl[is.na(goea$distance) == F]),
    formla="~agl")
