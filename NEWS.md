Changes in version 2.2.0 (2022-05-06)
==============
* ~~BIG CHANGE~~: Require measurement units on all distance inputs and parameters. 
Measurement units are converted to appropriate values internally, and output 
reports units. (e.g., units on strip width, study area size, etc.)
* Added density calculation to abundEstim, with bootstrapping. 
* ~~Change in default parameter~~: Changed default value for 'area' parameter 
to abundEstim from 1 to NULL, but NULL results in assignment of 1 square output 
unit.  
* Added color to print methods.  Results are in green, if the R terminal 
allows it (i.e., in Rstudio, but not R gui). 
* Reformatted default output to include density and abundance when 
printing an 'abund' object.
* Fixed bug in F.gx.estim that occasionally popped up when sighting 
function is monotonically decreassing.
* Updated and clarified documentation of several routines. 

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
* Fixed inverted plots that occured when w.lo > 0 (left-trunctation)
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