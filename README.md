# Rdistance

[![CRAN Version](http://www.r-pkg.org/badges/version/Rdistance)](http://www.r-pkg.org/pkg/Rdistance)
[![CRAN Downloads](http://cranlogs.r-pkg.org/badges/Rdistance)](http://cran.rstudio.com/web/packages/Rdistance/index.html)

**Rdistance - Distance Sampling Analyses for Abundance Estimation**

**Description:**
Distance-sampling is a popular method for abundance estimation in
  ecology. This package contains functions and associated routines to analyze
  distance-sampling data collected on point or line transects. 
  Line and point transect analayses are accomodated in one routine 
  that accepts a regression-like formula object. Abundance routines
  perform automated bootstrapping and automated detection-function 
  selection. Overall (study area) and site-level abundance estimates are
  available. A large suite of classical, parametric detection functions are 
  included, and the package can accommodate user-defined detection functions. 
  
**Rdistance Resources:**
The best place to start learning about `Rdistance` is at one of the upcoming workshops (see below) or the package's [GitHub Wiki](https://github.com/tmcd82070/Rdistance/wiki).

___

**MAJOR UPDATE IN PROGRESS:**
The stable version of `Rdistance` is on CRAN (v. 1.3.2, released 7/22/2015).  We are undertaking a major update during summer/fall 2017 in preparation for two workshops that depend on `Rdistance`.  Primary new features include the ability to analyze data from point transects (1.3.2 only allowed line transects) and the ability to include covariates in the detection function (1.3.2 did not allow covariates).  The development version (2.x.x) is not yet on CRAN, but can be installed from GitHub using the `devtools` package:

`devtools::install_github("tmcd82070/Rdistance")`

___

**Upcoming Workshops:**

* *Distance Sampling: Conventional and Hierarchical Methods for Abundance in R*
    * Full-day workshop, 9/23/2017, The Wildlife Society, Albuquerque, NM
    * Contact:  Trent McDonald, WEST, Inc. (tmcdonald@west-inc.com)
    * More info:  http://twsconference.org/workshops/

* *Distance Sampling: Conventional and Hierarchical Methods for Abundance in R*
    * Full-day workshop, 10/29/2017, The Society for Marine Mammalogy, Halifax, Nova Scotia
    * Contact:  Trent McDonald, WEST, Inc. (tmcdonald@west-inc.com)
    * More info:  http://www.smmconference.org/WorkshopDescriptions
