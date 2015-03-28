F.abund.estim <- function(dfunc, distdata, covdata,
                          area=1, ci=0.95, R=500,
                          bs.method="transects", plot.bs=FALSE){
  # the alternative bs.method would be observations (aka detections), but not programmed yet
   
  
    
  # Stop and print error if distdata or covdata contain NAs
  if(anyNA(distdata)==TRUE)
    stop("Please remove detections for which dist is NA.")
  if(anyNA(covdata)==TRUE)
    stop("covdata cannot contain NAs.")
  
  
  
  ########Plotting################
  f.plot.bs <- function(x, xscl, yscl, ...) {
    x.seq <- seq(x$w.lo, x$w.hi, length = 200)
    g.at.x0 <- x$g.x.scl
    x0 <- x$x.scl
    y <- like(x$parameters, x.seq - x$w.lo, series = x$series, 
              expansions = x$expansions, w.lo = x$w.lo, w.hi = x$w.hi)
    f.at.x0 <- like(x$parameters, x0 - x$w.lo, series = x$series, 
                    expansions = x$expansions, w.lo = x$w.lo, w.hi = x$w.hi)
    yscl <- g.at.x0/f.at.x0
    lines(x.seq, y * yscl, ...)
  }
  if (plot.bs) {
    tmp <- plot(dfunc) 
    x.scl.plot <- tmp$xscl.plot
    y.scl.plot <- tmp$yscl
    like <- match.fun(paste(dfunc$like.form, ".like", sep = ""))
  }
  
  #Apply truncation specified in dfunc object (including dists equal to w.lo and w.hi)
  (distdata <- distdata[distdata$dists >= dfunc$w.lo & distdata$dists <= dfunc$w.hi, ])

  # sample size (number of detections, NOT individuals)
  (n <- nrow(distdata))
  
  # group sizes
  (avg.group.size <- mean(distdata$groupsize))

  # total transect length and ESW
  (tot.trans.len <- sum(covdata$length))
  (esw <- ESW(dfunc))  #get effective strip width

  # estimate abundance
  (n.hat <- avg.group.size * n * area/(2 * esw * tot.trans.len))
  
  # store output
  ans <- dfunc
  ans$n.hat <- n.hat
  ans$n <- n
  ans$area <- area
  ans$esw <- esw
  ans$tran.len <- tot.trans.len
  ans$avg.group.size <- avg.group.size



  if (!is.null(ci)) {
    ######Compute bootstrap CI
    # Option 1:  resample transects
    if(bs.method=="transects"){
      g.x.scl.orig <- dfunc$call.g.x.scl  # g(0) or g(x) estimate
      
      n.hat.bs <- rep(NA, R)  # preallocate space for bootstrap replicates of nhat
      
      # Turn on progress bar (if utils is installed)
      if ("utils" %in% installed.packages()[, "Package"]) {
        require(utils)
        pb <- txtProgressBar(1, R)
        show.progress = TRUE
      } else show.progress = FALSE
      
      
      # bootstrap
      cat("Computing bootstrap confidence interval on N...\n")
      for(i in 1:R){
        # sample rows, with replacement, from site covariates
        new.covdata <- covdata[sample(nrow(covdata), nrow(covdata), replace=TRUE), ]
        
        new.trans <- as.character(new.covdata$siteID)  # which transects were sampled?
        trans.freq <- data.frame(table(new.trans))  # how many times was each represented in the new sample?
        
        # subset distance data from these transects
        new.trans <- unique(droplevels(new.covdata$siteID))
        new.distdata <- distdata[distdata$siteID %in% new.trans, ]  # this is incomplete, since some transects were represented > once
        
        # replicate according to freqency in new sample
        # merge to add Freq column to indicate how many times to repeat each row
        red <- merge(new.distdata, trans.freq, by.x="siteID", by.y="new.trans")
        # expand this reduced set my replicating rows
        new.distdata <- red[rep(seq.int(1, nrow(red)), red$Freq), -ncol(red)]
        
        # Extract distances
        new.x <- new.distdata$dists
        
        #update g(0) or g(x) estimate.
        if (is.data.frame(g.x.scl.orig)) {
          g.x.scl.bs <- g.x.scl.orig[sample(1:nrow(g.x.scl.orig), 
                                            replace = TRUE), ]
        } else g.x.scl.bs <- g.x.scl.orig
        
        
        # estimate distance function
        dfunc.bs <- F.dfunc.estim(new.x, likelihood = dfunc$like.form, 
                                  w.lo = dfunc$w.lo, w.hi = dfunc$w.hi, expansions = dfunc$expansions, 
                                  series = dfunc$series, x.scl = dfunc$call.x.scl, 
                                  g.x.scl = g.x.scl.bs, observer = dfunc$call.observer, 
                                  warn = FALSE)
        
        
        
        #Store ESW if it converged
        if (dfunc.bs$convergence == 0) {
          esw.bs <- ESW(dfunc.bs)
          if (esw.bs <= dfunc$w.hi) {
            
            
            
            ###Calculate observed metrics####
            # sample size
            n.bs <- nrow(new.distdata)
            
            # group sizes
            avg.group.size.bs <- mean(new.distdata$groupsize)
            
            
            #####Store observed metrics
            #esw <- ESW(dfunc.bs)  #get effective strip width
            tot.trans.len.bs <- sum(new.covdata$length)
            n.hat.bs[i] <- avg.group.size.bs * n.bs * area/(2 * esw.bs * tot.trans.len.bs)  # area stays same as original?   
            
            
            #calculate n.hat for this rep
            #n.bs[i] <- (n.tot.rep * area/(2 * trans.len.rep))/esw  # ? this math doesn't match calculating nhat elsewhere
            #End Augustine modifications
            
          }  # end if esw.bs <= w.hi
          if (plot.bs) 
            f.plot.bs(dfunc.bs, x.scl.plot, y.scl.plot, col = "blue", lwd = 0.5)
          
          if (show.progress) setTxtProgressBar(pb, i)
        }  # end if dfunc.bs converged
      }  # end bootstrap
    }  # end option 1, transects
    
    
    # close progress bar  
    if (show.progress) close(pb)
    
    # plot red line of original fit again (over bs lines)
    if (plot.bs) f.plot.bs(dfunc, x.scl.plot, y.scl.plot, col = "red", lwd = 3)
    
    
    # Calculate CI from bootstrap replicates
    p <- mean(n.hat.bs > n.hat, na.rm = TRUE)
    z.0 <- qnorm(1 - p)
    z.alpha <- qnorm(1 - ((1 - ci)/2))
    p.L <- pnorm(2 * z.0 - z.alpha)
    p.H <- pnorm(2 * z.0 + z.alpha)
    ans$ci <- quantile(n.hat.bs[!is.na(n.hat.bs)], p = c(p.L, p.H))
    ans$B <- n.hat.bs
    if (any(is.na(n.hat.bs))) cat(paste(sum(is.na(n.hat.bs)), "of", R, "iterations did not converge.\n"))
    
  }  # end if is.null ci



   ######Don't compute CI##############
   else {
     ans$B <- NA
     ans$ci <- c(NA, NA)
   }
  
  ####output####
  ans$alpha <- ci
  class(ans) <- c("abund", class(dfunc))
  ans

  
  
}  # end function




# Junk
#     # OLD start bootstrap
# 
#     for (i in 1:R) {
#       #sample transects
#       bs.trans <- sample(unique.transects, replace = TRUE)
#       #Observation distances for this replicate
#       d.bs <- unlist(tapply(bs.trans, 1:length(bs.trans), 
#                             FUN = function(x, df) {
#                               df[[as.character(x)]]
#                             }, data.list))
#       
# 
#       
#       #estimate distance function
#       dfunc.bs <- F.dfunc.estim(d.bs, likelihood = dfunc$like.form, 
#                                 w.lo = dfunc$w.lo, w.hi = dfunc$w.hi, expansions = dfunc$expansions, 
#                                 series = dfunc$series, x.scl = dfunc$call.x.scl, 
#                                 g.x.scl = g.x.scl.bs, observer = dfunc$call.observer, 
#                                 warn = FALSE)
#       
#       #Store ESW if it converged
#       if (dfunc.bs$convergence == 0) {
#         esw <- ESW(dfunc.bs)
#         if (esw <= dfunc$w.hi) {
#           
#           #Begin Augustine modifications
#           #Calculate n for this rep
#           n.rep=sum(!is.na(d.bs))
#           #Calculate total transect length for this rep
#           trans.len.rep=sum(transect.lengths[match(bs.trans,transect.lengths[,1]),2])
#           #Calculate group sizes and total n for this rep
#           if(!missing(group.sizes)){
#             n.tot.rep=sum(group.sizes[sort(bs.trans)],na.rm=T) #Augustine modification
#           }else{
#             n.tot.rep=n.rep*avg.group.size
#           }
#           #calculate n.hat for this rep
#           n.bs[i] <- (n.tot.rep * area/(2 * trans.len.rep))/esw
#           #End Augustine modifications
#           
#         }
#         if (plot.bs) 
#           f.plot.bs(dfunc.bs, x.scl.plot, y.scl.plot, 
#                     col = "blue", lwd = 0.5)
#       }
#       if (show.progress) 
#         setTxtProgressBar(pb, i)
#     }
#     if (show.progress) 
#       close(pb)
#     if (plot.bs) 
#       f.plot.bs(dfunc, x.scl.plot, y.scl.plot, col = "red", 
#                 lwd = 3)
#     p <- mean(n.bs > n.hat, na.rm = TRUE)
#     z.0 <- qnorm(1 - p)
#     z.alpha <- qnorm(1 - ((1 - ci)/2))
#     p.L <- pnorm(2 * z.0 - z.alpha)
#     p.H <- pnorm(2 * z.0 + z.alpha)
#     ans$ci <- quantile(n.bs[!is.na(n.bs)], p = c(p.L, p.H))
#     ans$B <- n.bs
#     if (any(is.na(n.bs))) 
#       cat(paste(sum(is.na(n.bs)), "of", R, "iterations did not converge.\n"))
#   }