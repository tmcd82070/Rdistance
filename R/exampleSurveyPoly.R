#' @title Example complex survey polygons
#'
#' @name exampleSurveyPoly
#'
#' @description
#' Two markedly non-convex survey polygons used to illustrate transect
#' placement with [makeLines()] and [drawTransects()], including estimation of
#' a `"zigzag"` baseline for a bent, elongated shape, the solidity warning that
#' suggests splitting such shapes with [convexPartition()], and the allocation
#' of effort across multiple polygons. The polygons are arc-shaped coastal
#' survey strata with solidities (area divided by convex-hull area) of about
#' 0.66 and 0.80.
#'
#' @docType data
#'
#' @format An `sf` data frame with two rows, each a `POLYGON` in the geometry
#' column `geometry`. Coordinates are projected in NAD83 / Alaska Albers
#' (EPSG:3338), an equal-area projection, so the linear unit is meters and
#' polygon areas are undistorted (about 367 and 325 square kilometers).
#'
#' @seealso [makeLines()], [drawTransects()]
#'
#' @source The first two rows of the `strata2_CI_surveyPolys` object from the
#' Aleutian Tern survey design work (McDonald Data Sciences / U.S. Fish &
#' Wildlife Service), reduced to their survey-polygon geometries and
#' reprojected from Web Mercator to Alaska Albers equal-area.
#'
#' @examples
#' data(exampleSurveyPoly)
#' plot(sf::st_geometry(exampleSurveyPoly))
#'
NULL
