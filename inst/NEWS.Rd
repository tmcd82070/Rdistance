\name{NEWS} 
\title{News for Package 'Rdistance'}
\section{Changes in version 2.1.3 (2019-01-02)}{
 \itemize{
    \item Fixed scaling error in the Gamma likelihood causing mode to be less than 1.0
    \item Fixed inverted plots that occured when w.lo > 0 (left-trunctation)
    \item Fixed bug in `autoDistSamp` when `bySite=TRUE`
    \item Fixed bug causing bootstrap failure of point transect surveys 
when `siteData` contained only one column 
 }
}

\section{Changes in version 2.1.2 (2018-08-23)}{
  \itemize{
    \item Point transect analysis (v1.3.2 only allowed line transects)
    \item Covariates in the detection function (v1.3.2 did not allow covariates).  
    \item Non-parametric (smoothed) distance functions
    \item Enhanced plotting abilities
    \item Enhanced vignettes
    \item Many bug fixes
  }
}
