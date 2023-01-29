# test_getDfuncModelFrame.r
library(Rdistance)
# context("getDfuncModelFrame")

# generate data for testing function
dist <- c(1,2,3,4,5,6,7,8,9,10,11,12) 
siteID <- c("a","a","a","b","b","b","c","c","c","d","d","d")
df_1 <- data.frame("dist" = dist, "siteID" = siteID)
observer <- c("obs1", "obs1", "obs2", "obs2")
siteID_b <- c("a", "b", "c", "d")
df_2 <- data.frame("observer" = observer, "siteID" = siteID_b)
# data to send to function
test_data <- merge(df_1, df_2, by="siteID")
# subset of test_data to compare with object returned by function
sub_test_data <- test_data[,c("dist","observer")]

# getDfuncModelFrame returns dataframe with different attributes from test_data
# so equality must be performed by columns
test_that("function(test_data) gives proper output: 1st column: dist", {
  expect_equal(getDfuncModelFrame(dist~observer, test_data)[,"dist"], sub_test_data[, "dist"])
})

test_that("function(test_data) gives proper output: 2nd column: observer", {
  expect_equal(getDfuncModelFrame(dist~observer, test_data)[,"observer"], sub_test_data[, "observer"])
})