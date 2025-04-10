Changes in version 4.0.4 (2025-04-08)
==============
Bug fixes:

*   Fixed bug in `predict` method when `type = "density"` causing NaN 
estimates on transects with observations outside the strip.
*   Fixed bug in `RdistDf` when merge parameter `by` was named.  When `by`
was named, and hence we merge on different variables, the names and values 
in `by` were reversed because we nest first then merge. Bug did not 
affect merges on same-named variables. 
*   *Non-bug* change: Moved all the examples from README to tutorials 
on the [McDonald Data Science website](https://mcdonalddatasciences.com/Rdistance.html) 
because we have more control, 
they are indexed by date and keyword, and they look better.


Changes in version 4.0.3 (2025-03-28)
==============
Version numbers >4.0.0 are substantially different from prior versions. 

* ***BIGGEST CHANGE***: Input data frames are now nested tibbles with one 
row per transect, 
and detection information in a data frame contained within a list column. 
Technically, the new data frame format is a grouped tibble with one row 
per group, and each group is a transect. 
Survey type, observer system, and name of the effort column are recorded 
as attributes in the new data frame. 
  + `RdistDf` constructs the new Rdistance data frames from separate 
     transect and detection data frames formatted for use in prior versions.
     Use this function on old sets of site and detection data frames to 
     construct the new nested data frames.  See examples in `?RdistDf`.
  + `is.RdistDf` checks the validity of the new Rdistance data frames.  
  + `summary` prints a summary of number of transects, number of groups seen, 
     number of individuals, etc. 
* ***Temporary Functionality Removal***: Versions >4.0.0 are a near 
complete re-write of versions <3.1.3.  I felt the re-write was necessary to 
improve stability of the code and lay the ground work for future functionality
improvements. But, to keep from going insane, I am rolling out new versions 
that build on one another, with core functions first (in 
4.0.* versions). As such, I have temporarily removed the following functionality
from versions with numbers 4.0.*:
  + Logistic distance function form.
  + Smoothed distance functions.
  + Beta distribution distance functions.
  + Double-observer methods. 
  + All vignettes.
* `print`, `summary`, and `plot` methods have been improved.
* Documentation updated and improved.
* Rdistance options can now be assessed by the regular `options` function.
  All Rdistance options are prefixed with 'Rdistance_' to distinguish them 
  from other options.
* New data frame format is pipe-able through the estimation workflow.  For
  example, the basic workflow is `df |> dfuncEstim(dist~1) |> abundEstim()`, 
  which will estimate a distance function and density in one go.




Changes in version 3.1.3 (2025-03-11)
==============
Version 3.1.3 contains three patches.  Several updates to 
documentation and one bug fix precipitated
by changes to the 'units' package. 

Changes in version 3.1.0 (2023-08-28)
==============
Version 3.1.0 primarily addresses [GitHub issues](https://github.com/tmcd82070/Rdistance/issues).

* ***Summary Methods Implemented***: Issue [#57](https://github.com/tmcd82070/Rdistance/issues/57).  Rdistance did not implement `summary` methods for dfunc and abund objects prior to version v3.1.0. This version implements both `summary` and `print` methods for the main outputs.  `print.dfunc` and `print.abund` are modeled on print methods for lm and glm objects.  New methods `summary.dfunc` and `summary.abund` are modeled on the summary methods for lm and glm and will produce fuller (relative to print) outputs. 
* Documentation fixes:
  + Added reference to general method in DESCRIPTION as suggested by CRAN editors.
  + Updated and clarified Description field of DESCRIPTION file.
  + Updated numerous links in README and NEWS.
* Bug Fixes:
  + Fixed bug when `abundEstim` was called with tibbles. 



Changes in version 3.0.0 (2023-06-12)
==============
Version 3.0.0 is a substantial change and upgrade. 

* ***BIGGEST CHANGE***: Measurement units are now required on all distances
(e.g., off-transect distances, strip widths, study area size, etc.),
and it is possible to specify output units.
Unit conversion makes use of the `units` package, is internal, and automatic. 
* ***One-sided Transects***: Added the ability to analyze single-sided transects 
by setting `singleSided` = TRUE in call to `abundEstim`. 
* ***User Requested***: Added computation of bootstrap confidence intervals 
for effective sampling distances (ESW and EDR). 
* ***NA Length Transects***: NA length transects are now allowed. Observations 
on NA length transects are used to estimate distance functions, but are dropped
when estimating density.  This allows, for example, off-transect distance observations
from one study area or year to estimate a distance function which is then applied 
to the observations from another study area or year. 
* Added calculation of target density (number per unit area) 
to `abundEstim`, and it is included in bootstrapping.
* Allowed expansion terms and covariates in the same distance function.
* Renamed `uniform` likelihood to `logistic`. Uniform is now deprecated.
* Added covariates to Gamma likelihood estimation.
* ***Change in default parameter***: Changed default value of `area` in
`abundEstim` from 1 to NULL. NULL now translates to 1 square output 
unit.  
* Added color to print methods.  Results are in green, if the R terminal 
allows it (i.e., in Rstudio, but not R gui).  Errors are in red.
* Reformatted default output to include target density and abundance when 
printing an `abund` object.
* Fixed bug in `F.gx.estim` that occasionally popped up when sighting 
function was monotonically decreasing.
* Updated starting values for faster estimation.
* Decreased lower limit of `negexp` likelihood parameter to achieve more valid
fits. 
* Warning messages now report which parameters are at their boundary, 
and which boundary (lower or upper).
* Bug fixes:
  + Fixed issue with contrasts in `model.matrix`
  + Fixed issue of no observations during bootstrapping
  + Fixed issues with changing scale locations (i.e., `x.scl` and `g.x.sxl`)
  + Fixed `ESW` for `w.lo` > 0
  + Restricted likelihood values to be positive
* Substantial documentation updates
* Substantial testing facilities added. 


Changes in version 2.1.5 (2020-06-17)
==============
* Fixed bug when bootstrap iteration selects zero targets 
* Many updates to vignettes
* Updates to testthat files
* Added descriptive error for case when bySite==T and detection level covariates are present (Issue #72)

Changes in version 2.1.4 (2019-06-11)
==============
* Fixed bug caused by empty factor levels
* Included automated testing (testthat)

Changes in version 2.1.3 (2019-01-02)
==============
* Fixed scaling error in the Gamma likelihood causing mode to be less than 1.0
* Fixed inverted plots that occurred when w.lo > 0 (left-truncation)
* Fixed bug in 'autoDistSamp' when 'bySite=TRUE'
* Fixed bug causing bootstrap failure of point transect surveys when 'siteData' contained only one column


Changes in version 2.1.2 (2018-08-23)
==============
* Included methods for point transects. Previous version (v1.3.2) allowed only line transects.
* Implemented covariates in the detection functions.
* Implemented non-parametric (smoothed) distance functions
* Enhanced plotting abilities
* Enhanced vignettes
* Many bug fixes
