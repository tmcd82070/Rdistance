<img src="README_files/RdistanceTopBanner.PNG" width="4032" />

# Distance Sampling Analysis Made Easy in R

[![CRAN Version](http://www.r-pkg.org/badges/version/Rdistance)](http://www.r-pkg.org/pkg/Rdistance)
[![CRAN Downloads](http://cranlogs.r-pkg.org/badges/Rdistance)](http://cran.rstudio.com/web/packages/Rdistance/index.html)
[![Travis-CI Build Status](https://travis-ci.org/tmcd82070/Rdistance.svg?branch=master)](https://travis-ci.org/tmcd82070/Rdistance)
[![CRAN Total Downloads](http://cranlogs.r-pkg.org/badges/grand-total/Rdistance)](http://www.r-pkg.org/pkg/Rdistance)

<!--img src="http://cranlogs.r-pkg.org/badges/grand-total/survminer?color=orange" alt="Total Downloads">
-->

**Description:**
Rdistance contains routines that  
  assist with analysis of
  distance-sampling data collected on point or line transects. 
  Distance models are specified using regression-like formula (similar 
  to lm, glm, etc.). Abundance routines
  perform automated bootstrapping and automated detection-function 
  selection. Overall (study area) and site-level (transect or point) 
  abundance estimates are available. A large suite of classical, 
  parametric detection functions are 
  included along with some uncommon parametric 
  functions (e.g., Gamma, negative exponential) and non-parametric
  smoothed distance functions. Custom (user-defined) detection functions
  are easily implemented (see vignette). 
  The help files and vignettes have been 
  vetted by multiple authors and tested in workshop 
  settings. 
  
Distance-sampling is a popular method for estimating density and 
  abundance of organisms in ecology. `Rdistance` contains 
  functions and associated routines that assist analysis of 
  distance-sampling data collected on point or line transects. 
  Both types of transects are accomodated in one routine 
  that accepts a regression-like formula (similar to `lm` or `glm`). 
  Abundance routines
  perform automated bootstrapping and automated detection-function 
  selection. Overall (study area) and site-level abundance estimates are
  available. A large suite of classical, parametric detection functions are 
  included, and the package can accommodate user-defined detection functions. 
  
**Rdistance Resources:**
The best place to start learning about `Rdistance` is at a workshop, 
processing a vignette, or the package's [GitHub Wiki](https://github.com/tmcd82070/Rdistance/wiki).

<img src="README_files/RdistanceSeparator.PNG" width="4032" />



## Installation

**Stable version:** 
`install.packages("Rdistance")`

**Development version:** Directly from GitHub using the `devtools` package:

`devtools::install_github("tmcd82070/Rdistance")`

Or, one can clone the repository and install using regular R 
package installation methods.  In a command window:

`git clone https://github.com/tmcd82070/Rdistance.git Rdistance`<br>
`r CMD install Rdistance`

<img src="README_files/RdistanceSeparator.PNG" width="4032" />


## RECENT CHANGES

**Version 2.1.3: Bug Fixes**
Version 2.1.3 addressed the following issues:
* Fixed scaling error in the Gamma likelihood causing mode to be less than 1.0
* Fixed inverted plots that occured when w.lo > 0 (left-trunctation)
* Fixed bug in `autoDistSamp` when `bySite=TRUE`
* Fixed bug causing bootstrap failure of point transect surveys 
when `siteData` contained only one column 



**Version 2.1.2: A Major Update**
Version 2.1.2 of `Rdistance` represented a major update 
from the previous version (v1.3.2). 
New features included:
* Point transect analysis (v1.3.2 only allowed line transects)
* Covariates in the detection function (v1.3.2 did not allow covariates).  
* Non-parametric (smoothed) distance functions
* Enhanced plotting abilities
* Enhanced vignettes
* Many bug fixes

<img src="README_files/RdistanceSeparator.PNG" width="4032" />


## Past Workshops

* *Distance Sampling: Conventional and Hierarchical Methods for Abundance in R*
    * Full-day workshop, 10/29/2017, The Society for Marine Mammalogy, Halifax, Nova Scotia
* *Distance Sampling: Conventional and Hierarchical Methods for Abundance in R*
    * Full-day workshop, 9/23/2017, The Wildlife Society, Albuquerque, NM
 
<img src="README_files/RdistanceSeparator.PNG" width="4032" />
 
## Contact

Questions or interested in a future workshop? Contact the maintainer: Trent McDonald (tmcdonald@west-inc.com) 
