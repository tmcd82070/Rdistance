![](README_files/RdistanceTopBanner.png)

# Rdistance - Distance Sampling Analysis Made Easy in R

[![CRAN Version](http://www.r-pkg.org/badges/version/Rdistance)](http://www.r-pkg.org/pkg/Rdistance)
[![CRAN Downloads](http://cranlogs.r-pkg.org/badges/Rdistance)](http://cran.rstudio.com/web/packages/Rdistance/index.html)
[![Travis-CI Build Status](https://travis-ci.org/tmcd82070/Rdistance.svg?branch=master)](https://travis-ci.org/tmcd82070/Rdistance)


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

<img src="readmeFiles/RdistanceSeparator.png" width="4032" />

___

## MAJOR UPDATE COMPLETE:
The stable version of `Rdistance` is [![CRAN Version](http://www.r-pkg.org/badges/version/Rdistance)](http://www.r-pkg.org/pkg/Rdistance).  

The stable version of `Rdistance` is a major update from the previous version. 
New features include:
* point transect analysis (v1.3.2 only allowed line transects)
* covariates in the detection function (v1.3.2 did not allow covariates).  
* non-parametric (smoothed) distance functions
* enhanced plotting abilities
* enhanced vignettes

<img src="readmeFiles/RdistanceSeparator.png" width="4032" />

## Installation

**Stable version:** `install.packages("Rdistance")`

**Development version:** Directly from GitHub using the `devtools` package:

`devtools::install_github("tmcd82070/Rdistance")`

Or, one can clone the repository and install using regular R 
package installation methods.  In a command window:

`git clone https://github.com/tmcd82070/Rdistance.git Rdistance
r CMD install Rdistance`


___

## Workshops:

***Recent***

* *Distance Sampling: Conventional and Hierarchical Methods for Abundance in R*
    * Full-day workshop, 10/29/2017, The Society for Marine Mammalogy, Halifax, Nova Scotia
* *Distance Sampling: Conventional and Hierarchical Methods for Abundance in R*
    * Full-day workshop, 9/23/2017, The Wildlife Society, Albuquerque, NM
 
## Contact

Questions of interested in workshops? Contact the maintainer: Trent McDonald (tmcdonald@west-inc.com) 

<img src="readmeFiles/RdistanceBottomBanner.png" width="4032" />
