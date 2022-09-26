
# Compare to Distance
require(Distance)

# Format detection data
d.data <- thrasherDetectionData[c("dist", "groupsize")]
names(d.data) <- c("distance", "size")  # meet naming conventions
d.data$object <- 1:nrow(d.data)  # add object ID

# Produce and format 3 other required data.frames
d.region <- data.frame(Region.Label="main", Area=10000)

d.sample <- data.frame(Sample.Label = thrasherSiteData$siteID, Effort = 1)  # Effort for points is number of visits (see ?flatfile)
d.sample$Region.Label <- "main"

d.obs <- data.frame(object=1:nrow(d.data), Region.Label="main", Sample.Label=thrasherDetectionData$siteID)

# Fit model and estimate abundance
# THIS NEXT LINE DOES NOT RUN.  NEW VERSION OF DISTANCE IS DIFFERENT. THESE ARE DEPRECATED INPUTS.
(ds.fit <- ds(data=d.data, region.table=d.region, sample.table=d.sample, obs.table=d.obs,
              truncation=trunc, transect="point", key="hn", adjustment=NULL, dht.group=FALSE))

plot(ds.fit)
print(ds.fit)
summary(ds.fit)  # Because distance is measured in m, and I set Area to 10000 (m^2 to ha), the Abundance estimate is really density per ha



# Results very, very similar
# AIC
AIC(fit)  # Rdistance = 1761.916
AIC(ds.fit)  # Distance = 1761.898

# Abund
fit$n.hat  # 0.4687061
fit$ci  # depends on the bootstrap, about 0.38 - 0.57
ds.fit$dht$individuals$N  # 0.4686923 (0.3792886 - 0.5791696)
# ds.fit$dht$individuals$N$Estimate
# ds.fit$dht$individuals$N$lcl
# ds.fit$dht$individuals$N$ucl

# Sigma
coef(fit)  # 73.57595
exp(ds.fit$ddf$ds$aux$ddfobj$scale$parameters)  # 73.57655
})
