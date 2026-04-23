#' @name Rdistance-package
#' 
#' @title Rdistance - Distance Sampling Analyses for Abundance Estimation
#' 
#' @description 
#' `Rdistance` contains functions and associated routines to analyze
#' distance-sampling data collected on point or line transects. 
#' Some of `Rdistance`'s features include:
#' 
#' *  Accommodation of both point and line transect analyses in one routine ([dfuncEstim()]).
#' *  Regression-like formula for inclusion of distance function covariates ([dfuncEstim()]).
#' *  Automatic bootstrap confidence intervals ([abundEstim()]).
#' *  Parallel processing of bootstrap iterations (`parallel` argument of [abundEstim()]).
#' *  Availability of both study-area and site-level abundance estimates (`help("predict.dfunc")`).
#' *  Rigorous physical measurement requirements and automated conversion when necessary (\code{\link{%#%}}, [setUnits()]).
#' *  Classic parametric distance functions ([halfnorm.like()], [hazrate.like()], [negexp.like()]), and
#'    expansion functions ([cosine.expansion()], [hermite.expansion()], [simple.expansion()]).
#' *  Mixture distance functions for non-standard shapes and thresholds ([oneStep.like()], [triangle.like()], and [huber.like()]).
#' *  Automated distance function fitting and selection [autoDistSamp()].
#' *  `print`, `plot`, `predict`, `coef`, and `summary` methods for distance function objects and 
#'    abundance classes.
#' 
#' # Background:
#' Distance-sampling is a popular method for abundance estimation in
#' ecology. Line transect surveys are conducted by traversing
#' randomly placed transects in a study area with the objective of
#' sighting animals and estimating density or abundance.  Data collected
#' during line transect surveys consists of *target* sightings, 
#' either of individuals or groups, and off-transect distances to the 
#' original location of the *target*. 
#' When *targets* are sighted in groups, data include the number 
#' of individuals in the group.
#' 
#' Point transect surveys are similar except that observers stop one 
#' or more times along the transect to observe *targets*.  Point transects are
#' popular avian survey methods where detections are often auditory 
#' cues. Point transects are also for studies using automated auditory detectors
#' or trail cameras.  Point transect data consists of radial distances from 
#' the observer to the *target*.   
#' 
#' The defining feature of distance sampling is the tendency for 
#' probability of detection to decline as
#' off-transect or radial distances increase. Targets far from 
#' the observer are generally  
#' harder to detect than those closer.  In most
#' line transect studies, *targets* on the transect (off-transect
#' distance = 0) are assumed to be sighted with 100% probability.  This
#' assumption allows researchers to estimate the proportion of missed 
#' targets and in turn adjust the number of 
#' sighted *targets* for missed detections. 
#' Some studies utilize two observers searching the same areas and are able to
#' estimate the proportion of individuals missed on the transect line and 
#' thereby eliminate the
#' assumption that all individuals on the line have been observed.
#' 
#' # Purpose: 
#' The author's aims are 
#' to provide an easy-to-use, rigorous, and flexible analysis option in R for 
#' distance-sampling data.  The authors believe that beginning
#' users need easy-to-use and easy-to-understand software,
#' while advanced users require greater flexibility and customization, and 
#' their aim is to meet the demands of both groups. 
#' 
#' # Data sets:
#' `Rdistance` contains the following example data sets: 
#' 
#' *  Line-transect sampling of Brewers sparrows in central Wyoming 
#' ([sparrowDf()]).
#' *  Point-transect sampling of Sage Thrashers in central Wyoming 
#' ([thrasherDf()]).
#' 
#' 
#' # References 
#' 
#' Buckland, S.T., Anderson, D.R., Burnham, K.P. and Laake, J.L.
#'   1993. *Distance Sampling: Estimating Abundance of Biological
#'   Populations*. Chapman and Hall, London.
#' 
#' @aliases Rdistance-package distance Rdistance point-transect line-transect
#' 
#' 
#' @author Main author and maintainer: Trent McDonald
#' <trent@mcdonalddatasciences.com>
#' 
#' Coauthors: Ryan Nielson, Jason Carlisle, and Aidan McDonald
#' 
#' Contributors: Ben Augustine, James Griswald, Joel Reynolds, Pham Quang, Earl
#' Becker, Aaron Christ, Brook Russelland, Patrick McKann, Lacey Jeroue, 
#' Abigail Hoffman, Michael Kleinsasser, and Ried Olson
#' 
#' @keywords package
#' 
#' @import units
#' 
"_PACKAGE"
