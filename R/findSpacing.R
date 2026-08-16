#' @title findSpacing - Transect spacing for a target survey length
#'
#' @description
#' Compute the approximate transect spacing that yields a target *total*
#' transect length inside one or more polygons. The spacing is found by
#' minimizing the difference between the realized total length and
#' `targetLength` with [OSCARS::oscars()]. A single spacing is returned and,
#' applied uniformly, distributes effort across polygons in proportion to
#' their area. [findSpacing()] performs the calculation; [makeLines()] does
#' the random placement; [drawTransects()] chains the two.
#'
#' @inheritParams makeLines
#'
#' @param targetLength The desired transect length across `sPoly`, interpreted
#' according to `target` (total length by default). Must be a `units` length
#' object, e.g. `units::set_units(1000, "km")`. Effort is allocated to polygons
#' in proportion to area.
#'
#' @param target Character string selecting what `targetLength` measures:
#' `"total"` (the default) is the total transect length including off-effort
#' transit; `"onEffort"` is the on-effort survey length only. The two differ by
#' the connectors between parallel legs for `"rectangular"` transects, and for
#' `"zigzag"` transects only by whatever part of the continuous route a
#' concavity pushes outside the polygon (nothing, when the polygon is convex).
#' Use `"onEffort"` when `targetLength` comes from a survey-effort calculation
#' such as [calcLineLength()].
#'
#' @param minLength Minimum length for an individual transect leg to be
#' counted, as a `units` length object. Legs shorter than `minLength` are
#' dropped when measuring the realized length. Defaults to
#' `units::set_units(100, "m")`.
#'
#' @param tolerance Relative tolerance on the total length, passed to
#' [OSCARS::oscars()] as its objective tolerance `fTol`. Defaults to 0.01.
#'
#' @param nfmax Maximum number of objective-function evaluations for
#' [OSCARS::oscars()]. Defaults to 50.
#'
#' @details
#' The returned `spacing` is the distance between adjacent transects: the
#' perpendicular distance between the parallel lines for `"rectangular"`
#' transects, and the baseline distance between adjacent crossings of the
#' baseline for `"zigzag"` transects (one complete zig-zag cycle covers twice
#' that). Because a smaller spacing packs more transect into each polygon, the
#' total length is a decreasing function of spacing, which [OSCARS::oscars()]
#' searches over a single scalar.
#'
#' Realized zigzag length depends on the random start: on where the pivots fall
#' along the baseline, and on which side of the baseline the route starts,
#' which decides whether a given station gets the far pivot or the near one. On
#' a polygon that is not symmetric about its baseline both matter, so the
#' objective averages the length over several starts of each kind and targets
#' the expectation. The single random draw taken later by [makeLines()]
#' therefore lands near, but not exactly on, `targetLength`.
#'
#' @return A `units` length object: the approximate spacing that yields
#' `targetLength`. Pass it to [makeLines()] to place transects.
#'
#' @seealso [makeLines()], [drawTransects()], [OSCARS::oscars()].
#'
#' @examples
#' poly <- sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(list(rbind(
#'   c(0, 0), c(60000, 0), c(60000, 25000), c(0, 10000), c(0, 0)))),
#'   crs = 3338))
#' findSpacing(poly, units::set_units(150, "km"), type = "zigzag")
#'
#' @export
findSpacing <- function(sPoly,
                        targetLength,
                        type = c("rectangular", "zigzag"),
                        angle = 0,
                        baseline = NULL,
                        combine = TRUE,
                        target = c("total", "onEffort"),
                        minLength = units::set_units(100, "m"),
                        tolerance = 0.01,
                        nfmax = 50,
                        minSolidity = 0.75,
                        plot = FALSE) {

  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package 'sf' is required. Please install it with install.packages('sf').")
  }
  type   <- match.arg(type)
  target <- match.arg(target)
  makeLinesRequireLength(targetLength, "targetLength")
  makeLinesRequireLength(minLength, "minLength")

  polys <- makeLinesPolygons(sPoly)
  if (length(polys) == 0) {
    stop("'sPoly' contains no POLYGON or MULTIPOLYGON geometries.")
  }
  if (isTRUE(sf::st_is_longlat(polys[[1]]))) {
    stop("'sPoly' must be projected to a planar CRS whose linear unit is ",
         "meters (e.g., a UTM zone).")
  }
  if (type == "zigzag" && !is.null(baseline) && length(polys) > 1) {
    stop("Supply 'baseline' only when 'sPoly' contains a single polygon.")
  }

  targetM <- makeLinesAsMeters(targetLength)
  minLenM <- makeLinesAsMeters(minLength)

  prep <- makeLinesPrep(polys, baseline, type, angle)

  if (type == "zigzag" && is.null(baseline) && minSolidity > 0) {
    for (k in seq_along(prep)) {
      if (!is.na(prep[[k]]$solidity) && prep[[k]]$solidity < minSolidity) {
        warning(sprintf(paste0(
          "Polygon %d is markedly non-convex (solidity %.2f < %.2f); a single ",
          "zigzag baseline may not cover it well, and coverage may be uneven. ",
          "Consider splitting it into more-convex pieces with ",
          "convexPartition(), then returning to drawTransects()/makeLines() ",
          "with the resulting pieces. Alternatively, supply your own ",
          "'baseline', or set minSolidity = 0 to silence this warning."),
          k, prep[[k]]$solidity, minSolidity))
      }
    }
  }

  # Expected length across all polygons at spacing s. Rectangular on-effort is
  # exact in closed form (A/s), so a single representative offset suffices.
  # Zigzag length depends on the (random) start of the pivots along the
  # baseline, so the objective averages the realized length over several starts
  # to target the expectation and keep makeLines()'s single random draw
  # unbiased.
  onEff     <- target == "onEffort"
  totalArea <- sum(vapply(prep, function(pp) pp$areaM2, numeric(1)))
  sumAt <- function(s, off, phase, startSide = TRUE) {
    sum(vapply(prep, function(pp)
      makeLinesPolyTotal(pp, type, angle, s, off, minLenM, phase, startSide,
                         onEff),
      numeric(1)))
  }
  quantAt <- function(s) {
    if (type == "rectangular") {
      # 'target' selects whether targetLength refers to total length (on-effort
      # plus off-effort transit) or on-effort length only. Rectangular on-effort
      # is A/s in closed form (Cauchy-Crofton).
      if (onEff) totalArea / s else sumAt(s, s * 0.5, 0.5)
    } else {
      # makeLines() draws both the phase and the starting side at random, and on
      # a polygon that is not symmetric about its baseline the starting side
      # changes the realized length, so average over both to target the
      # expectation of what makeLines() will draw.
      grd <- expand.grid(phase = c(0.3, 0.7), side = c(TRUE, FALSE))
      mean(vapply(seq_len(nrow(grd)),
                  function(i) sumAt(s, 0, grd$phase[i], grd$side[i]),
                  numeric(1)))
    }
  }
  obj <- function(par) abs(quantAt(par[1]) - targetM)

  # Search bounds for the scalar spacing, anchored on the Cauchy-Crofton
  # spacing A/L: the spacing at which parallel lines cover 'targetLength' of the
  # area. That is exact for rectangular on-effort, and for a zigzag it is a good
  # lower bound on the answer, because oblique legs cover more ground per unit
  # of baseline than perpendicular ones. Anchoring the zigzag search this way
  # matters: its objective steps down as each new leg enters the polygon, and a
  # search started far from the answer can exhaust 'nfmax' before reaching it.
  s0 <- totalArea / targetM
  if (type == "rectangular") {
    lwr <- s0 * 0.15; upr <- s0 * 3; start <- s0
  } else {
    # A spacing wider than the polygon holds no zigzag at all, so never look
    # past it.
    maxSpan <- max(vapply(prep, function(pp) pp$span, numeric(1)))
    upr     <- min(4 * s0, maxSpan)
    lwr     <- min(s0 / 2, upr / 4)
    start   <- min(s0, upr)
  }

  o <- OSCARS::oscars(
      obj, n = 1, lwr = lwr, upr = upr, start = start,
      controls = OSCARS::oscars.control(nfmax = nfmax, infol = 0, fTol = tolerance))
  sBest <- o$par[1]

  if (plot) {
    plot(sf::st_geometry(do.call(c, polys)), col = "grey90", border = "grey40")
    for (pp in prep) if (!is.null(pp$base)) plot(pp$base, add = TRUE, col = "blue", lty = 2)
  }

  uStr <- units::deparse_unit(targetLength)
  units::set_units(units::set_units(sBest, "m"), value = uStr, mode = "standard")
}
