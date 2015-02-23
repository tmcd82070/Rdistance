
# Density function for adaptive kernel estimation, 5-phase plugin and 2-phase plugin.

# The following function provides estimates a distance kernel density function using a choice of 3 bandwith estimators:
# 2-phase plug-in, 5-phase plug-in or the adaptive bandwidth method.
# Required data is a dataframe with a column of perpendicular distances with column heading 'distance'. 

F.density_adaptive <- function(distance, W1, W2){
  
  # install and load, or just load, two required packages:
  # quantreg (adaptive kernel density estimation)
  if(!require(quantreg)) { 
      install.packages("quantreg")
      require(quantreg)
  }
  
  # KernSmooth (5 phase plug-in)
  if(!require(KernSmooth)) { 
    install.packages("KernSmooth")
    require(KernSmooth)
  }
  
  # Shift obs distance so min available distance = 0. Fit kernel and keep fitted values between 0 and W2-W1
  distance = distance - W1

#--------------------------------------------------------------------------------------------------------------------------  
  # Use adaptive bandwidth method of Silverman, B. (1986) Density Estimation, pp 100–104.  Default kernel is the normal.

  # Use kappa = 0.5 - constant multiplier for initial (default) window width.  See justification for this in ref above.
  #x<- seq(0,W2-W1,length = 1000)  # points at which density is calculated; defaults to an equispaced sequence covering.
                                  # the range of distance data.
  #f.d = akj(sort(c(distance,-distance)), x, alpha = 0.5, kappa = 0.5 )

  # Scale probabilities for adaptive method.
  #f.d$dens = f.d$dens/((x[2]-x[1])*sum(f.d$dens))

#-------------------------------------------------------------------------------------------------------------------------------

  # Use direct plugin method (Sheather and Jones 1991).

  #f.d = density(c(distance,-distance), n=1000,width="nrd0", kernel="gaussian",  from=0, to=(W2-W1))
  # direct plug in: 
  # width="SJ-dpi",
  # rule of thumb:
  # width="nrd",
  # width = "SJ-ste"
  # width = "nrd0"
  # width = "bcv"

  # Scale probabilities for adaptive method.
  # f.d$dens = f.d$dens/((x[2]-x[1])*sum(f.d$dens))
#--------------------------------------------------------------------------------------------------------------------------
  # Use  direct plug-in (Sheather and Jones 1991) where user controls number of phases (level =  5Lis 5 phase) to estimate functionals.

  bwidth<- dpik(sort(c(distance)),level=5L,kernel="normal",gridsize=1000)
  f.d<- bkde(sort(c(distance,-distance)),bandwidth=bwidth,range.x=c(0,W2-W1),gridsize=1000)
 

  # Shift distance back so min = W1 and not 0.
  #f.d$x = x + W1 
    
  # Scale probabilities 
  f.d$y = f.d$y/((f.d$x[2]-f.d$x[1])*sum(f.d$y))
  f.d$dens = f.d$y    

         
#------------------------------------------------------------------------------------------------------------------------------------  
  return(f.d)

}





# Integration function
F.int <- function(x,y){

  sum(diff(x)*(y[-1]+y[-length(y)]))/2 

}



# Calling function.  This function calls the F.density_adaptive function above, returns estimates and plots the fits. 

F.DF_adaptive <- function(obs.data, W1, W2,n,stripwidth,transect_length){ 

  # Shifting and truncating data
  distances = obs.data$distance[obs.data$distance >= W1]
    
  # Fitting kernel
  f.d = F.density_adaptive(distances, W1, W2)
  
  # Scaling max height of kernel = 1
  f.d.prob = f.d$dens/max(f.d$dens)

  # Area under curve (p-hat)
  phat = F.int(f.d$x, f.d.prob)/(W2-W1) 

  # Plot scaled detection function on histogram of distances
  plt=T 
  nbins=20
  if(plt == T){
    fdy.25 = f.d$dens[round(f.d.prob, 2) == 0.25][1]
    if(is.na(fdy.25) == FALSE) {
      fdy.50 = fdy.25*2
      fdy.15 = 1.5*fdy.50/5
      fdy.75 = fdy.25*3
      fdy.100 = fdy.25*4
    } else {
      fdy.50 = f.d$dens[round(f.d.prob, 2) == 0.50][1]
      fdy.15 = 1.5*fdy.50/5
      fdy.25 = fdy.50/2
      fdy.75 = fdy.25*3
      fdy.100 = fdy.25*4
    }
    if(is.na(fdy.25) == TRUE) {
      fdy.10 = f.d$dens[round(f.d.prob, 2) == 0.10][1]
      fdy.15 = fdy.10*1.5
      fdy.25 = fdy.10*2.5
      fdy.50 = fdy.25*2
      fdy.75 = fdy.25*3
      fdy.100 = fdy.25*4
    }
    plot(x=c(0, W2), y=c(0, fdy.100*1.20), type='n', xlab="Distance (m)", 
		ylab="Probability of Detection", cex.lab=1.5, yaxt='n')
    hist(distances[distances <= W2], prob=T, breaks=seq(W1, W2, by=(W2-W1)/nbins), 
		add=T, col="grey")  
    axis(2, at=c(0, fdy.25, fdy.50, fdy.75, fdy.100),  
		labels=c("0", "0.25", "0.50", "0.75", "1.0"))
    lines(f.d$x, f.d$dens, lty=1, lwd=2)    # detection function
  }
# Stake Data
   #D<- (642/11)/(2*1000*20*phat)
   #D<- D*10000 # in Hectares
# Goltees data:
 D<- n/(stripwidth*transect_length*phat)
 N<- D*stripwidth*transect_length
  # Answers
  #ans = list(P.hat = phat, f.d.y=f.d$dens,prob=f.d.prob, f.d.x=f.d$x,D,N)
  
  # For R library(boot)
  #return(ans)
  cat('N hat = ', N, '\n')
  cat('D hat = ', D, '\n')

}

