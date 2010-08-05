`GoF.GMRDS` <-
function(FittedModel,USE.KS=TRUE)
{ 
###########################
# GoF.GMRDS calculates the p-value for a goodness of fit hypothesis
# test for a particular model.  As the gamma rv's are transformed
# to Unif(0,1) rv's, the test in the originial version of the code
# divides the Ui1's and Ui2's into 15 bins on (0,1) and tests whether
# their frequency is uniform (using a chi square test).  The second
# form is given when USE.KS=TRUE.  It gives the results of a two-
# sided Kolmogorov-Smirnov test of each of the Ui's versus a Unif(0,1).
#
# Note that the chi square test can be very unstable, as the results
# of the test are highly dependent upon the number of bins selected.
# The ks test doesn't force a choice of bins, but may become more
# conservative since the parameters are estimated from the data.

if (missing(FittedModel)) stop("Missing fitted Gamma MRDS model.")
if (is.null(FittedModel)) stop("Model fitting did not converge. Try different starting values.")


USE.PGAMMA.WB <- FALSE

n1 <- FittedModel$hvec[1]
n2 <- FittedModel$hvec[2]
n3 <- FittedModel$hvec[3]
cat("Pilot Observations = ",(n1 + n3),"\n")
cat("Passenger Observations = ",(n2 + n3),"\n")
cat("Total Observations = ",(n1 + n2 + n3),"\n")
nbin1 <- 15
nbin2 <- 15


#if (min((n1+n3),(n2+n3))< 120)
#   nbin <- 10

#creates lambda, r, and b for Pilot and Passenger models
lam1 <- exp(FittedModel$XdesPilotF[,-4] %*% FittedModel$b1[1:(length(FittedModel$b1)-1)])
lam2 <- exp(FittedModel$XdesPassengerF[,-4] %*% FittedModel$b2[1:(length(FittedModel$b2)-1)])
r1 <- FittedModel$b1[length(FittedModel$b1)]
r2 <- FittedModel$b2[length(FittedModel$b2)]
bb1 <- (1/gamma(r1)) * (((r1 - 1)/exp(1))^(r1 - 1))
bb2 <- (1/gamma(r2)) * (((r2 - 1)/exp(1))^(r2 - 1))


if (USE.PGAMMA.WB == T)
    {Q1 <- pgamma(FittedModel$wMax/(bb1*lam1), r1) - pgamma(FittedModel$wb/(bb1*lam1), r1)
     Q2 <- pgamma(FittedModel$wMax/(bb2*lam2), r2) - pgamma(FittedModel$wb/(bb2*lam2), r2)
     Ui1 <- (pgamma( (FittedModel$Ydata[,3])/(bb1*lam1),r1) -pgamma(FittedModel$wb/(bb1*lam1),r1))/Q1
     Ui2 <- (pgamma( (FittedModel$Ydata[,3])/(bb2*lam2),r2) -pgamma(FittedModel$wb/(bb2*lam2),r2))/Q2 }
else
    {Ui1 <- pgamma((FittedModel$Ydata[,3]/(bb1*lam1)),r1)/pgamma(FittedModel$wMax/(bb1*lam1), r1) 
     Ui2 <- pgamma((FittedModel$Ydata[,3]/(bb2*lam2)),r2)/pgamma(FittedModel$wMax/(bb2*lam2), r2) }
#Ui1 and Ui2 should be distributed as Unif(0,1) after transformation

#pick out those observations for Passenger/Pilot
Ui1 <- Ui1[FittedModel$Ydata[,2] != 2]
Ui2 <- Ui2[FittedModel$Ydata[,2] != 1]

#creates histograms of number of observed in each bin for Pilot and Passenger
#also creates equal sized breaks on the unit interval
par(mfrow=c(1,2))
ChiObs1 <- hist(Ui1,breaks=(0:nbin1)/nbin1,main=paste("Pilot PIT values (Ui1)"))
abline(h=length(Ui1)/nbin1,lwd=2, lty=2)
ChiObs2 <- hist(Ui2,breaks=(0:nbin2)/nbin2,main=paste("Passenger PIT values (Ui2)"))
abline(h=length(Ui2)/nbin2,lwd=2, lty=2)
#ChiObs1$counts is a vector with the number in each bin
#ChiSq1 and ChiSq2 create test statistics
#
ChiSq1 <- sum((ChiObs1$counts - length(Ui1)/nbin1)^2)/(length(Ui1)/nbin1)
ChiSq2 <- sum((ChiObs2$counts - length(Ui2)/nbin2)^2)/(length(Ui2)/nbin2)

cat("Pilot ChiSq = ",ChiSq1,"; Pvalue = ",pchisq(ChiSq1,nbin1-length(FittedModel$b1)-1,ncp=0, lower.tail = FALSE),"\n")
cat("Passenger ChiSq = ",ChiSq2,"; Pvalue = ",pchisq(ChiSq2,nbin2-length(FittedModel$b2)-1, ncp=0, lower.tail = FALSE),"\n")
#critical value is ChiSq1 or ChiSq2
#df = (number of bins) - (number of parameters) - 1

cat("\n")

#if USE.KS=T, gives p-values for Kolmogorov-Smirnov tests for Pilot/Passenger
if(USE.KS==T)
    {
    require("stats", quietly=TRUE, character.only=TRUE)
    KSPilot<-stats::ks.test(Ui1,punif,exact=NULL);KSPassenger<-stats::ks.test(Ui2,punif,exact=NULL)
    cat("Results from the Kolmogorov-Smirnov test for Pilot:  test statistic = ",KSPilot$statistic," and p-value = ",KSPilot$p.value,".\n")
    cat("Results from the Kolmogorov-Smirnov test for Passenger:  test statistic = ",KSPassenger$statistic," and p-value = ",KSPassenger$p.value,".\n")}
else{cat("You did not perform a Kolmogorov-Smirnov test.\n")}
}

