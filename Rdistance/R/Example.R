#This demonstrates the new and old function to compare input format and differences
#in results.
require(Rdistance)
require(VGAM) #To simulate group sizes to make sure that works correctly
source("F.abund.estim2.R")
source("F.automated.CDA2.R")
source("F.dfunc.estim2.R")



turtle <- read.delim("TurtleBootInput.txt")
attach(turtle)
Transects <- read.delim("tblTransects.txt")
translen=cbind(Transects$TransectID,Transects$LengthKM) #map transects to lengths



#old function
N1 <- F.automated.CDA( turtle$CalcDist[!is.na(turtle$CalcDist)], area=1, total.trans.len=sum(translen[,2]), w.hi = 50, 
                       likelihood=c("halfnorm", "hazrate","uniform", "Gamma"), series=c("cosine", "simple"), 
                       expansions=c(0,1,2,3))

N1 <- F.automated.CDA2( turtle$CalcDist, area=1, w.hi = 50, transects = turtle$TransectID, transect.lengths=translen,likelihood=c("halfnorm", "hazrate","uniform", "Gamma"), series=c("cosine", "simple"), expansions=c(0,1,2,3))


#dfunc produces same output, but needed to be altered to carry through the NAs is dfunc$dist
dfunc=F.dfunc.estim(turtle$CalcDist, likelihood = "Gamma", w.hi = 50, expansions = 0)
dfunc2=F.dfunc.estim2(turtle$CalcDist, likelihood = "Gamma",w.hi=50,expansions=0)



out1=F.abund.estim2(dfunc, ci = 0.95, R = 500, plot.bs = FALSE, 
                             transects = turtle$TransectID, transect.lengths=translen)

out2=F.abund.estim(dfunc, tot.trans.len = sum(translen[,2]),n = sum(!is.na(dfunc$dist)), ci = 0.95, R = 500, plot.bs = FALSE, 
                   transects = turtle$TransectID)


####Now with group sizes

#generate group sizes
group.sizes=rep(NA,length(dfunc$dist))
idx=which(!is.na(dfunc$dist))
group.sizes[idx]=rpospois(length(idx),3)

#new file
out1=F.abund.estim2(dfunc, group.sizes=group.sizes,ci = 0.95, R = 500, plot.bs = FALSE, 
                    transects = turtle$TransectID, transect.lengths=translen)

#old file
out2=F.abund.estim(dfunc=dfunc2, group.sizes=group.sizes[!is.na(dfunc$dist)],tot.trans.len = sum(translen[,2]),n = sum(!is.na(dfunc$dist)), ci = 0.95, R = 500, plot.bs = FALSE, 
                   transects = turtle$TransectID[!is.na(dfunc$dist)])