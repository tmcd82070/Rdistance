#' @title Draw random transects given target length
#'
#' @description
#' Designs and draws random line transects inside one or more polygons. Can
#' either be given a target *total* transect length or fixed spacing 
#' determined elsewhere.
#' This is a convenience wrapper for
#' [findSpacing()], which computes spacing that yields the targeted 
#' length, then [makeLines()], which actually places transects with a 
#' given spacing using a random start. Typically
#' used after target line length has been estimated using [calcLineLength()]
#' or another method.
#'
#' @inheritParams findSpacing
#' 
#' @inheritParams makeLines
#'
#' @param spacing Optional transect spacing, as a `units` length object. When
#' supplied, computing the spacing to achieve `targetLength` in the study 
#' area is skipped and this spacing is used directly.
#' When `NULL` (the default) the spacing to achieve approximately `targetLength`
#' transects (including connectors) is computed using [findSpacing()].
#' 
#' @param R Number of independent random replicates to generate, passed to
#' [makeLines()]. Defaults to 1. When `R > 1` the return contains `R` 
#' realized transects, each with its own random start.  Replicates are 
#' identified by the `id` column.
#'
#' @param minSolidity For `"zigzag"` transects with an estimated baseline, a
#' warning is issued when a polygon's solidity (area divided by convex-hull
#' area) falls below this value, because a single zigzag baseline may not cover
#' a strongly concave polygon. The warning recommends splitting the polygon
#' into more-convex pieces with [convexPartition()] and re-running
#' `drawTransects()` on the pieces. Set to 0 to disable. Defaults to 0.75.
#'
#' @details
#' `targetLength` is the desired transect length, interpreted as either total
#' length (including off-effort transit between legs) or on-effort length,
#' according to `target`. The realized length will be as close to this
#' value as the geometry and the (integer) number of transects allow. This
#' amount of effort is
#' allocated across polygons in proportion to area.
#'
#' @inherit makeLines return
#'
#' @author Trent McDonald.
#'
#' @seealso [findSpacing()] and [makeLines()] for the two individual steps;
#' [calcLineLength()] for estimating `targetLength`.
#'
#' @examples
#' # A simple tapered survey polygon (Alaska Albers, meters).
#' poly <- sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(list(rbind(
#'   c(0, 0), c(60000, 0), c(60000, 25000), c(0, 10000), c(0, 0)))),
#'   crs = 3338))
#'
#' # Five replicate zigzag designs for a 150 km survey. Totals vary by
#' # replicate, so summarize the output columns grouped by the replicate id.
#' set.seed(439752)
#' zz  <- drawTransects(poly, units::set_units(150, "km"), type = "zigzag", R = 5)
#' tgt <- attr(zz, "summary")$targetLength
#' zz |> 
#'   sf::st_drop_geometry() |>
#'   dplyr::group_by(id) |>
#'   dplyr::summarize(onEffortLength = sum(onEffortLength),
#'                    totalLength    = sum(totalLength),
#'                    pctOfTarget    = totalLength / tgt)
#'
#' # Realistic polygons (exceeds CRAN time limits).
#' \donttest{
#' data(exampleSurveyPoly)
#' 
#' # Workflow 1: Sample size driven
#' trgLen = calcLineLength(sPoly = exampleSurveyPoly[1,]
#'                , N = 1200
#'                , p = 0.5
#'                , w = units::set_units(200, "m")
#'                , targetGroups = 300
#'                , avgGroupSize = 2.5
#'                )
#' transects <- drawTransects(sPoly = exampleSurveyPoly[1,]
#'                          , targetLength = trgLen
#'                          , target = "onEffort"
#'                          )
#'                          
#' # Workflow 2: Budget driven
#' costPer = units::set_units(1000, "1/hr") # dollars/hr for aircraft
#' budget = 4000 # dollars in budget
#' speed = units::set_units(85, "miles/hr")
#' transects <- drawTransects(sPoly = exampleSurveyPoly$geometry[1]
#'                          , targetLength = speed * budget / costPer
#'                          , target = "total"
#'                          )
#' 
#'                           
#' many <- drawTransects(exampleSurveyPoly, units::set_units(300, "km"),
#'                       type = "zigzag")
#' }
#'
#' @export
drawTransects <- function(sPoly,
                          targetLength,
                          type = c("rectangular", "zigzag"),
                          angle = 0,
                          spacing = NULL,
                          baseline = NULL,
                          combine = TRUE,
                          target = c("total", "onEffort"),
                          R = 1,
                          minLength = units::set_units(100, "m"),
                          tolerance = 0.01,
                          nfmax = 50,
                          minSolidity = 0.75,
                          plot = FALSE) {

  type   <- match.arg(type)
  target <- match.arg(target)
  makeLinesRequireLength(targetLength, "targetLength")

  if (is.null(spacing)) {
    # Suppress the solidity warning here (minSolidity = 0); makeLines() re-warns.
    spacing <- findSpacing(sPoly, targetLength, type = type, angle = angle,
                           baseline = baseline, combine = combine,
                           target = target, minLength = minLength,
                           tolerance = tolerance, nfmax = nfmax,
                           minSolidity = 0, plot = FALSE)
  } else {
    makeLinesRequireLength(spacing, "spacing")
  }

  out <- makeLines(sPoly, type = type, angle = angle, spacing = spacing,
                   baseline = baseline, combine = combine, R = R,
                   minSolidity = minSolidity, plot = plot)

  # Record the target length in the summary (in the output length units). The
  # realized on-effort/total lengths vary by replicate and are left for the user
  # to aggregate from the output columns.
  summ <- attr(out, "summary")
  uStr <- units::deparse_unit(summ$spacing)
  tgtM <- makeLinesAsMeters(targetLength)
  summ$targetLength <- units::set_units(units::set_units(tgtM, "m"),
                                        value = uStr, mode = "standard")
  summ$target <- target
  attr(out, "summary") <- summ

  out
}
