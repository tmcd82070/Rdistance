#' Rdistance - Distance Sampling Analyses for Abundance Estimation
#' 
#' Distance-sampling is a popular method for abundance 
#' estimation in ecology. This package contains functions and associated
#' routines to analyze distance-sampling data collected at point or line
#' transects. A large suite of classical, parametric detection functions are
#' included, and the package can
#' also accommodate user-defined detection functions.  Covariates can be
#' included in the detection function.
#' 
#' \bold{Background} - Line transect surveys are conducted by traversing
#' \emph{randomly} placed transects in a study area with the objective of
#' estimating density or abundance of a particular organism.  Data collected
#' during line transect surveys consists of sighting records for
#' \emph{targets}, usually either individuals or groups of some species.  Among
#' the collected data, off-transect distances are recorded or computed from
#' other information such as sighting distance and angle (see
#' \code{\link{perpDists}}).  Off-transect distances are the perpendicular
#' distances from the transect to the location of the initial sighting cue.
#' The physical locations of sighted targets are often recorded or computed.
#' When groups are the target, the number of individuals in the group is
#' recorded.
#' 
#' A fundamental characteristic of distance sampling analyses is that
#' sightability (probability of detection) of targets is assumed to decline as
#' off-transect distances increase. Targets far from the transect are assumed
#' to be harder to detect than targets close to the transect.  In most
#' classical line transect studies, targets on the transect (off-transect
#' distance = 0) are assume to be sighted with 100\% probability.  This
#' assumption allows estimation of the proportion of targets missed during the
#' survey, and thus adjust the actual number of sighted targets by this
#' proportion. Some studies utilize two observers searching the same areas to
#' estimate the proportion of indivivduals missed and thereby eliminating the
#' assumption that all individuals on the line have been observed.
#' 
#' \bold{Relationship to other software} - A detailed comparison of
#' \code{Rdistance} to other options for distance sampling analysis (e.g.,
#' Program DISTANCE, R package \code{Distance}, and R package \code{unmarked})
#' is forthcoming.  Our intent is for \code{Rdistance} to provide rigorous and
#' flexible analysis of distance-sampling data.  We understand that beginning
#' users often need software that is both easy to use and easy to understand,
#' and that advanced users often require greater flexibility and customization.
#' Our aim is to meet the demands of both user groups.  \code{Rdistance} is
#' under active development, so please contact us with issues, feature
#' requests, etc. through the package's GitHub website
#' (\url{https://github.com/tmcd82070/Rdistance}).
#' 
#' \bold{Resources} - The best place to start learning about \code{Rdistance}
#' is at the package's GitHub Wiki, which hosts several tutorial vignettes and
#' FAQs (\url{https://github.com/tmcd82070/Rdistance/wiki}).
#' Additionally, the examples in the help files for
#' \code{\link{dfuncEstim}}, \code{\link{abundEstim}}, and
#' \code{\link{autoDistSamp}} highlight the package's primary functionality.
#' 
#' \tabular{ll}{ Package: \tab Rdistance\cr Type: \tab Package\cr License: \tab
#' GNU General Public License\cr }
#' 
#' A list of routines can be obtained by loading \code{Rdistance} and issuing
#' \code{ls(pos="package:Rdistance")}.
#' 
#' @name Rdistance-package
#' 
#' @aliases Rdistance-package distance Rdistance
#' 
#' @docType package
#' 
#' @author Main author and maintainer: Trent McDonald
#' <tmcdonald@@west-inc.com>
#' 
#' Coauthors: Ryan Nielson, Jason Carlisle, and Aidan McDonald
#' 
#' Contributors: Ben Augustine, James Griswold, Joel Reynolds, Pham Quang, Earl
#' Becker, Aaron Christ, Brook Russelland, and Patrick McKann
#' 
#' @keywords package
NULL