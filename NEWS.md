Changes in version 4.5.0 (2026-08-13)
==============

*   **Functionality change**: Added a family of functions for designing 
random line-transect surveys inside study-area polygons. `drawTransects()` 
is the main entry point; it calls `findSpacing()` to compute the transect 
spacing that yields a target survey length, then `makeLines()` to place the 
transects with a random start. `calcLineLength()` converts a target number of 
detected groups into a target length of transect. Both parallel 
("rectangular", or "mow-the-grass") and "zigzag" layouts are supported, over 
one or several polygons at once, with optional random replicates (`R`). 
Transects can be returned as one continuous route per polygon or as individual 
legs (`combine`), and `targetLength` can refer to total or on-effort length 
(`target`). Optimization of spacing is performed by `OSCARS::oscars`.
*   **Functionality change**: "zigzag" routes are constructed by the method of 
`RUtilities::zigZagRoute()`, which was moved into this package. Pivots are 
placed where perpendiculars to the baseline, regularly spaced with a random 
start, cross the polygon boundary, so legs now run edge to edge and cover the 
polygon completely. 
*   **Functionality change**: `spacing` is the distance between adjacent 
transects under both layouts, so the two are directly comparable. For a zigzag 
it is the baseline distance between the points where adjacent legs cross the 
baseline; one complete zig-zag cycle covers `2 * spacing`. 
*   **Functionality change**: `combine` now controls clipping as well as the 
shape of the returned object. `combine = TRUE` returns the route as flown, 
unclipped, so that it stays connected; across a concave polygon it can run 
outside the polygon, and the returned lengths sum to the total length. 
`combine = FALSE` returns one row per leg, clipped to the polygon so that no 
geometry falls outside it, and the returned lengths sum to the on-effort 
length. A leg that a concavity breaks into pieces is returned as a single 
`MULTILINESTRING` row, preserving one row per leg. The `onEffortLength` and 
`totalLength` columns are computed the same way under both settings.
*   **Functionality change**: zigzag baselines are now straight. When no 
`baseline` is supplied, the polygon's centerline is computed as before, then 
straightened by regressing Y on X; the slope of that regression sets the 
baseline's direction, which is translated to run through the polygon's 
centroid and extended past the polygon's bounding box so that transects are 
placed all the way through the polygon. A user-supplied baseline is likewise 
extended before use, but is reported back unchanged. The previous wiggly 
centerline is no longer used: strongly bent polygons should be split with 
`convexPartition()`.
*   **Functionality change**: Added `convexPartition()`, which splits a 
strongly concave polygon into a few more-convex pieces using Approximate 
Convex Decomposition (Lien & Amato 2006). Splitting improves the coverage of 
zigzag transects on bent, arc-, or L-shaped polygons.
*   **Functionality change**: `convexPartition()` gained `nPieces` and 
`method`. `nPieces` is either an integer, in which case exactly that many 
pieces are returned, or `"optimum"` (the default), in which case the count is 
chosen automatically. `method = "fast"` (the default) cuts greedily at the 
most concave vertex, as before; `method = "optimum"` searches the cut vertices 
with `OSCARS::oscars` to maximize the minimum solidity of the pieces, and with 
`nPieces = "optimum"` searches the number of pieces (capped at 10) as well. 
The default call is unchanged: it is still the deterministic, 
`concavityTol`-driven decomposition. `nStarts`, `nfmax`, and `solidityTol` 
control the search; a progress bar appears once a search passes 10 seconds.
*   **New data set**: Added `exampleSurveyPoly`, two non-convex Aleutian tern 
survey strata projected to an equal-area CRS (NAD83 / Alaska Albers), used to 
demonstrate the survey-design functions.
*   **Update**: Added `sf`, `grDevices`, and `OSCARS` to Imports, required by 
the new survey-design functions.


Changes in version 4.4.5 (2026-06-22)
==============

*   **New data set**: Added `pronghornDf`, aerial line-transect data for 
pronghorn (*Antilocapra americana*) collected by the Wyoming Game and Fish 
Department in southeast Wyoming, 2012-2019. 
*   **New data set**: Added `pronghornAreas`, the study-area (herd-unit) 
polygons associated with the pronghorn line-transect data. 
*   **Update**: Added `data-raw` scripts documenting construction of the 
bundled data sets.


Changes in version 4.4.4 (2026-05-21)
==============

*   **Functionality change**: `abundEstim()` now accepts a previously fitted 
abundance object in addition to a distance function. This lets users add 
bootstrap iterations to an existing fit, for example by fitting with 
`ci = NULL` first and running (or extending) the bootstrap later. 
*   **Documentation Updates**: Updated documentation of `abundEstim()`, 
`autoDistSamp()`, and related functions.


Changes in version 4.4.3 (2026-05-13)
==============

*   **Bug Fix**: Bug fix in ESW when likelihood is oneStep and expansions > 0. 
Before: `integrateOneStepNumeric` returned areas without units, which was okay 
for optimization, but not okay for ESW. `integrateOneStepNumeric` now returns 
a vector with the correct units and is consistent with the other integrate??? 
functions.


Changes in version 4.4.2 (2026-05-08)
==============

*   **Bug Fix**: 'outArea' was undefined when likelihood returned NA or Inf
or NaN and this stopped execution.  'outArea' is now always defined. This bug 
only effected the non-smooth likelihoods 'oneStep', 'triangle', and 'huber'. 
*   **Bug Fix**: Fixed failing bootstraps.  Base function 'rbind' mysteriously
began stripping attributes from its arguments.  Rdistance relies on these 
attributes for bootstrapping.  Attributes restored now and bootstraps 
run.
*   **Bug Fix**: Fixed the width vector for smooth likelihoods that was too 
small when expansions were present.  Ill-sized W vector caused failure when 
expansions > 0 and likelihood was halfnormal, hazrate, negexp, or Gamma. 
*   **Documentation Updates**: Updated and clarified sections of the 
documentation of abundEstim and RdistanceControls.

Changes in version 4.4.0 (2026-04-22)
==============

*   **Functionality change**: Added `triangle` distance function, a 
mixture of triangle and uniform distributions. See `help(triangle.like)` 
examples.
*   **Functionality change**: Added `huber` distance function, a 
mixture of an inverted version of Huber loss and a uniform distribution. 
See `help(huber.like)` examples.
*   **Functionality change**: Changed bounds on the `oneStep` Theta parameter 
to the minimum and maximum of distances, thus ensuring the ledge lies within
the range of observed distances. 
*   **Functionality change**: Implemented use of `stats::optim` for optimization.
Prior to this, the only non-gradient optimizer was HookeJeeves. Default 
optimizer for `oneStep`, `triangle`, and `huber` is now the Nelder-Mead 
implementation in `optim`.
*   **Update**: Minor documentation updates and clarifications.
*   *Note*: No bugs found that effect results produced by prior versions. 

Changes in version 4.3.1 (2026-02-10)
==============

*   Version v4.3.1 contains a critical bug fix. The prior version, 4.3.0, 
incorrectly reported ESW when scaling factors (`g.x.scl`) were less than 1.0.
Versions <=4.1.0 reported correct ESW, and ESW reported by 4.3.0 was correct
when `g.x.scl` equaled 1.0 (the default).
*   Default value for `w.lo` changed in `parseModel` to suppress warning re 
`x.scl` < `w.lo`. 


Changes in version 4.3.0 (2026-01-06)
==============

*   **Functionality change**: Added parallel bootstrap processing for confidence
interval estimation.  Bootstrap resampling to compute confidence intervals can 
take considerable time and running in parallel, on multiple CPU cores, speeds 
things up. Parallel processing is handled by `multidplyr`. Default is to run 
bootstraps in parallel on _n_-1 CPU cores, where _n_ is the number of 
cores available on the local machine . 
No progress bar is produced during parallel processing. Disable parallel processing
by setting `parallel = FALSE` in call to `abundEstim`.  Run on a specific number 
of cores by setting `parallel` to that number (e.g., `parallel = 3` uses three 
cores). 
*   **Functionality change**: Added the Gamma likelihood back to the suite of 
likelihoods available in Rdistance.  The Gamma was removed during the big 
re-vamp of version 4.0.3. The Gamma likelihood works for both lines and points. 
It works with both covariates and expansions.
*   **Update**: Substantial documentation updates and clarifications.


Changes in version 4.1.0 (2025-11-29)
==============

*   **Functionality change**: Added `oneStep.like`, a mixture of non-overlapping
uniform densities, as a distance function.  Included associated print, 
plot, summary, and expansion methods. This required inclusion of a new non-gradient
based optimizer. 
*   **Functionality change**: Implemented reporting of bootstrap coefficient 
standard errors, after bootstrap resampling is complete.  Prior to 
bootstrapping, asymptotic se's are reported when known. 
*   **Functionality change**: Included unit assignment helpers. Units can 
now be assigned with the `%#%` operator (e.g., 3 %#% "m"), which makes unit
assignment easier than in prior versions (which used `units::set_units`). 
Fixed unit assignment operators are included for all popular linear and 
squared units (e.g., 3 %m%. assigns meters to 3). See `help(unitHelpers)`.
*   **Functionality change**: Included b-spline expansion factors for distance 
functions. Expansion factors are now `cosine`, `hermite`, `simple`, 
and `bspline`.
*   **Functionality change**: Added verbosity option. This prints intermediate 
parameter estimates and likelihoods during maximization. 
Set `options(Rdistance_verbocity = 1)`
or higher to see progressively more detailed intermediate output. 
*   **Update**: Likelihood computation time was significantly 
decreased relative to the prior version, greatly increasing speed of 
maximizations. Now, all likelihoods for both lines and points that do not 
contain expansions use exact integration, which is much quicker than 
numerical integration.  Numeric integration is used only when expansion 
terms are included. 
*   **Update**: Substantial documentation updates and clarifications.
*   **Bug Fixes**: 
  .   Fixed bug in `plot.dfunc.para` when `w.lo` > 0
  .   Fixed bug in point transect methods resulting in incorrect 
  likelihood scalings
  .   Fixed expansions `hermite` and `simple` that was causing 
  non-convergence issues

Changes in version 4.0.5 (2025-04-10)
==============
Bug fixes:

*   Fixed bug in `predict` method when `type = "density"` causing NaN 
estimates on transects with observations outside the strip.
*   Fixed bug in `RdistDf` when merge parameter `by` was named.  When `by`
was named and merge was on different named variables, names and values 
in `by` were reversed prior to the fix due to first nesting then merging. 
This bug did not 
affect merges on same-named variables. 
*   *Non-bug* change: Moved all the examples from README to tutorials 
on the [McDonald Data Science website](https://mcdonalddatasciences.com/Rdistance.html) 
because we have more control, 
they are indexed by date and keyword, and they look better.


Changes in version 4.0.3 (2025-03-28)
==============
Methods and workflow in Rdistance versions >4.0.0 are substantially 
different from prior versions. 

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
  + Gamma distribution distance functions.
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
