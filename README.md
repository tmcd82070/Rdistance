# Rdistance

[![CRAN Version](http://www.r-pkg.org/badges/version/Rdistance)](http://www.r-pkg.org/pkg/Rdistance)
[![CRAN Downloads](http://cranlogs.r-pkg.org/badges/Rdistance)](http://cran.rstudio.com/web/packages/Rdistance/index.html)
[![Travis-CI Build Status](https://travis-ci.org/tmcd82070/Rdistance.svg?branch=master)](https://travis-ci.org/tmcd82070/Rdistance)

**Rdistance - Distance Sampling Analyses for Abundance Estimation**

**Description:**
Distance-sampling is a popular method for abundance estimation in
  ecology. This package contains functions and associated routines to analyze
  distance-sampling data collected on point or line transects. 
  Both types of transects are accomodated in one routine 
  that accepts a regression-like formula object. Abundance routines
  perform automated bootstrapping and automated detection-function 
  selection. Overall (study area) and site-level abundance estimates are
  available. A large suite of classical, parametric detection functions are 
  included, and the package can accommodate user-defined detection functions. 
  
**Rdistance Resources:**
The best place to start learning about `Rdistance` is at a workshop (see below) or the package's [GitHub Wiki](https://github.com/tmcd82070/Rdistance/wiki).

___

**MAJOR UPDATE COMPLETE:**
The stable version of `Rdistance` is on CRAN (v. 1.3.2, released 7/22/2015).  
The version available here is a major update and we are 
currently making final preparations before re-submitting to CRAN.  
Primary new features include:
* point transect analysis (1.3.2 only allowed line transects)
* covariates in the detection function (1.3.2 did not allow covariates).  
* non-parametric (smoothed) distance functions
* enhanced plotting abilities
* enhanced vignettes

The development version (2.x.x) is coming to CRAN soon.  You can install 
the latest version directly from GitHub using the `devtools` package:

`devtools::install_github("tmcd82070/Rdistance")`

___

**Workshops:**

***Recent***

* *Distance Sampling: Conventional and Hierarchical Methods for Abundance in R*
    * Full-day workshop, 10/29/2017, The Society for Marine Mammalogy, Halifax, Nova Scotia
* *Distance Sampling: Conventional and Hierarchical Methods for Abundance in R*
    * Full-day workshop, 9/23/2017, The Wildlife Society, Albuquerque, NM
    
Interest in upcoming workshops? Contact Trent McDonald (tmcdonald@west-inc.com) for more info.
