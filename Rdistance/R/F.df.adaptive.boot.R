F.DF_Adaptive_boot <- function(W1, W2, obs.data, n, stripwidth, transect_length, indices){
  #library(boot) 

  # Shifting and truncating data
  #distances = obs.data$distance[obs.data$distance >= W1]
  distances <- obs.data[indices,]
  #distances<- obs.data
  # Fitting kernel
  f.d = F.density_adaptive(distances, W1, W2)  # this function is in kernel.analysis.R
  
  # Scaling max height of kernel = 1
  f.d.prob = f.d$dens/max(f.d$dens)

  # Area under curve (p-hat)
  phat = F.int(f.d$x, f.d.prob)/(W2-W1)  # this function is in kernel.analysis.R

  
   D <- n/(stripwidth*transect_length*phat)
   N <- D*stripwidth*transect_length
  # Answers
  #ans = list(P.hat = phat, f.d.y=f.d$y, f.d.x=f.d$x)
  
  # For R library(boot)
  return(N)

}