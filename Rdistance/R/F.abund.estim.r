F.abund.estim2=function(dfunc, avg.group.size = 1, group.sizes, area = 1, 
          ci = 0.95, R = 500, plot.bs = FALSE, 
          transects = NULL, transect.lengths=NULL) 
{
  
###I changed default of n = length(dfunc$dist) to  n=sum(!is.na(dfunc$dist))
##since there will be NA's in dfunc$dist
##removed tot.trans.length as input as it is calculated from transect lengths
##removed n from input as it is calculated from dfunc
  
  
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
    #tmp <- plot.dfunc(dfunc)    this didn't work
    tmp <- plot(dfunc)  #this did  
    x.scl.plot <- tmp$xscl.plot
    y.scl.plot <- tmp$yscl
    like <- match.fun(paste(dfunc$like.form, ".like", sep = ""))
  }
  
###Calculate observed metrics####
  n=sum(!is.na(dfunc$dist))
  if (missing(group.sizes)) {
    n.tot <- avg.group.size * n
  }
  #else if (length(group.sizes) == n) {
  else if (sum(!is.na(group.sizes)) == n) { #Augustine modification
    #n.tot <- sum(group.sizes)
    n.tot=sum(group.sizes,na.rm=T) #Augustine modification
    #avg.group.size <- mean(group.sizes)
    avg.group.size <- mean(group.sizes,na.rm=T)
  }else{
    stop("Either avg.group.size or group.sizes vector must be specified")
  }

#####Store observed metrics
  esw <- ESW(dfunc)  #get effective strip width
  tot.trans.len = sum(transect.lengths[,2])
  n.hat <- n.tot * area/(2 * esw * tot.trans.len)
  ans <- dfunc
  ans$n.hat <- n.hat
  ans$n <- n
  ans$area <- area
  ans$esw <- esw
  ans$tran.len <- tot.trans.len
  ans$avg.group.size <- avg.group.size

######Compute bootstrap CI
  if (!is.null(ci)) {
    
  #Orignial code:  Remove invalid distances before bootrapping.  NA, too high or low
  #Code no longer does this
    d.orig <- dfunc$dist #observed distances
    #ind <- !is.na(d.orig) & (dfunc$w.lo <= d.orig) & (d.orig <= dfunc$w.hi)
    #d.orig <- d.orig[ind]
    g.x.scl.orig <- dfunc$call.g.x.scl #g(0) or g(x) estimate
    
    #This only bootstraps transects with observations
    #else {
      #transects <- transects[ind]
    #}
    
    #Get unique transects
    unique.transects <- sort(unique(transects)) 

    #Make data struture of transects and distances for bootstrapping
    data.list <- tapply(d.orig, transects, function(x) {
      x
    })
    
    #Make data struture of group sizes for bootstrapping, Augustine modification
    if (!missing(group.sizes)) {
      group.sums <- tapply(group.sizes, transects, function(x) {
        sum(x)
      })
    }
    
    n.bs <- rep(NA, R)#preallocate space for bootstrap replicates
    
    #n.tot and tot.trans.len not constant if bootstrapping all transects
    #tmp.const <- n.tot * area/(2 * tot.trans.len)
    
    #Turn on progress bar
    if ("utils" %in% installed.packages()[, "Package"]) {
      require(utils)
      pb <- txtProgressBar(1, R)
      show.progress = TRUE
    }
    else {
      show.progress = FALSE
    }
    #show.progress=F
    #start bootstrap
    cat("Computing bootstrap confidence interval on N...\n")
    for (i in 1:R) {
      #sample transects
      bs.trans <- sample(unique.transects, replace = TRUE)
      #Observation distances for this replicate
      d.bs <- unlist(tapply(bs.trans, 1:length(bs.trans), 
                            FUN = function(x, df) {
                              df[[as.character(x)]]
                            }, data.list))
      
      #update g(0) or g(x) estimate.
      if (is.data.frame(g.x.scl.orig)) {
        g.x.scl.bs <- g.x.scl.orig[sample(1:nrow(g.x.scl.orig), 
                                          replace = TRUE), ]
      }else{
        g.x.scl.bs <- g.x.scl.orig
      }
      
      #estimate distance function
      dfunc.bs <- F.dfunc.estim(d.bs, likelihood = dfunc$like.form, 
                                w.lo = dfunc$w.lo, w.hi = dfunc$w.hi, expansions = dfunc$expansions, 
                                series = dfunc$series, x.scl = dfunc$call.x.scl, 
                                g.x.scl = g.x.scl.bs, observer = dfunc$call.observer, 
                                warn = FALSE)
      
      #Store ESW if it converged
      if (dfunc.bs$convergence == 0) {
        esw <- ESW(dfunc.bs)
        if (esw <= dfunc$w.hi) {
          
          #Begin Augustine modifications
          #Calculate n for this rep
          n.rep=sum(!is.na(d.bs))
          #Calculate total transect length for this rep
          trans.len.rep=sum(transect.lengths[match(bs.trans,transect.lengths[,1]),2])
          #Calculate group sizes and total n for this rep
          if(!missing(group.sizes)){
            n.tot.rep=sum(group.sizes[sort(bs.trans)],na.rm=T) #Augustine modification
          }else{
            n.tot.rep=n.rep*avg.group.size
          }
          #calculate n.hat for this rep
          n.bs[i] <- (n.tot.rep * area/(2 * trans.len.rep))/esw
          #End Augustine modifications
          
        }
        if (plot.bs) 
          f.plot.bs(dfunc.bs, x.scl.plot, y.scl.plot, 
                    col = "blue", lwd = 0.5)
      }
      if (show.progress) 
        setTxtProgressBar(pb, i)
    }
    if (show.progress) 
      close(pb)
    if (plot.bs) 
      f.plot.bs(dfunc, x.scl.plot, y.scl.plot, col = "red", 
                lwd = 3)
    p <- mean(n.bs > n.hat, na.rm = TRUE)
    z.0 <- qnorm(1 - p)
    z.alpha <- qnorm(1 - ((1 - ci)/2))
    p.L <- pnorm(2 * z.0 - z.alpha)
    p.H <- pnorm(2 * z.0 + z.alpha)
    ans$ci <- quantile(n.bs[!is.na(n.bs)], p = c(p.L, p.H))
    ans$B <- n.bs
    if (any(is.na(n.bs))) 
      cat(paste(sum(is.na(n.bs)), "of", R, "iterations did not converge.\n"))
  }


######Don't compute CI##############
  else {
    ans$B <- NA
    ans$ci <- c(NA, NA)
  }

####output####
  ans$alpha = ci
  class(ans) <- c("abund", class(dfunc))
  ans
}