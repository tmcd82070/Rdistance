# Vignette for the Rdistance Package
# An example distance-sampling analysis workflow using the Rdistance package

# Prepared by:
# Jason Carlisle
# WY Cooperative Fish & Wildlife Research Unit
# and intern at WEST, Inc.
# jason.d.carlisle@gmail.com

# Last updated 1/28/2015


# Load or Install/load the Rdistance package (currently version 1.1)
if(!require(Rdistance)) { 
    install.packages("Rdistance")
    require(Rdistance)
}

# Set working directory
setwd("C:\\Users\\jcarlis3\\Box Sync\\Classes\\s8_Spr_15\\DistanceSampling\\ExampleVignette")


# Read in example datasets
counts <- read.csv("Sparrows.Counts.csv")
covs <- read.csv("Sparrows.Covariates.csv")



# Add NA rows to counts where no sparrows were observed
unique(covs$TranID)
absences <- data.frame(table(counts$TranID))
absences <- as.character(absences[absences$Freq==0, 1])

(toadd <- data.frame(matrix(nrow=length(absences), ncol=ncol(counts))))
toadd[, 1] <- absences
names(toadd) <- names(counts)

counts <- rbind(counts, toadd)





# Compute the off-transect (aka perpendicular) distances from the observer's sight distance and angle
counts$PerpDist <- perp.dists(obs.dist=counts$SightDist, obs.angle=counts$SightAngle, digits=1)

# Save vector of distances
x <- counts$PerpDist

# Examine the histogram of distances
hist(x)
summary(x)


# Throws error prior to bootstrapping:
#Error in key * (1 + (exp.term %*% a[2:(nexp + 1)])) : 
#    non-conformable arrays
F.automated.CDA(x, area=10000, total.trans.len=500, w.hi = 150,
                likelihoods=c("uniform", "halfnorm", "hazrate", "negexp", "Gamma"),
                series=c("simple", "cosine"), expansions=c(0, 1, 2, 3), plot=TRUE)

# uniform, 0 expansions selected as best
# density (per ha) is 0.96 (CI=0.84, 1.19)
F.automated.CDA(x, area=10000, total.trans.len=(72*500), w.hi = 150,
                likelihoods=c("uniform", "halfnorm", "Gamma"),
                series=c("simple", "cosine"), expansions=c(0, 1, 2), plot=TRUE)

F.automated.CDA(x, area=10000, total.trans.len=(72*500), w.hi=150,
                likelihoods=c("halfnorm", "uniform", "hazrate", "negexp", "Gamma"),
                series=c("simple", "cosine"), plot=TRUE)




# Fit detection function
# from ?F.dfunc.estim

(fit <- F.dfunc.estim(x, likelihood="uniform", w.hi=150))

(fit <- F.abund.estim(fit, tot.trans.len=(72*500), area=10000))
print(fit)



