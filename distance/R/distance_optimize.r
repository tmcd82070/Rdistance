distance.fn <- function(a,dfunc,startvals){
#
#   This is a function to maximize the likelihood of a distance function in order to estimate parameters.
#   
    # -------------------------------------------------

     likelihood <- function(dfunc){          
     #  Log likelihood for distance problem
     #  Input:      
          
          
          -sum(log(dfunc)) 
     }

    # -------------------------------------------------

    






    
       a<-rep(.5,key_num+expansions)
    strt.vals <- a
    
    out <- optim(strt.vals, likelihood, "BFGS",hessian=TRUE) 
        
    est <- out$par
    
    cls <- "dfunc"





# hessian = F.2nd.deriv(out$par, likelihood )

 se_coeffs<- sqrt(diag(solve(out$hessian)))

