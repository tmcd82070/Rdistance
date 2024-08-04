library(Rdistance)

data("sparrowDetectionData")
data("sparrowSiteData")
# data("sparrowDf")


data("thrasherDetectionData")
data("thrasherSiteData")

scrub_environ <- function(x){
  # x is vector of lines in snapshot. 
  # Scrub stochastic environment line from output
  toss <- grepl("<environment:", x)
  x[!toss]
}