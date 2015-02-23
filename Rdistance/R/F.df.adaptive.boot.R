F.DF_Adaptive_boot <- function(W1,W2,obs.data,n,stripwidth,transect_length,indices){
  library(boot) 
  # Run R boot function. First input >library(boot):
  # golf2.dpi.boot<- boot(data=golf2,statistic=F.DF_Adaptive_boot,W1=0,W2=3.84,n=142,stripwidth=4,transect_length=210,R=1000)
  # golf2.dpi.boot.ci<- boot.ci(golf2.dpi.boot,conf= 0.95,type="all")
  # Shifting and truncating data
  #distances = obs.data$distance[obs.data$distance >= W1]
  distances<- obs.data[indices,]
  #distances<- obs.data
  # Fitting kernel
  f.d = F.density_adaptive(distances, W1, W2)
  
  # Scaling max height of kernel = 1
  f.d.prob = f.d$dens/max(f.d$dens)

  # Area under curve (p-hat)
  phat = F.int(f.d$x, f.d.prob)/(W2-W1) 

  
   D<- n/(stripwidth*transect_length*phat)
   N<- D*stripwidth*transect_length
  # Answers
  #ans = list(P.hat = phat, f.d.y=f.d$y, f.d.x=f.d$x)
  
  # For R library(boot)
  return(N)

} 

