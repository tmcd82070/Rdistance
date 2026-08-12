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
#' transit between legs; `"onEffort"` is the on-effort survey length only. The
#' two differ only for `"rectangular"` transects (zigzags have no off-effort
#' transit, so total equals on-effort). Use `"onEffort"` when `targetLength`
#' comes from a survey-effort calculation such as [calcLineLength()].
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
#' `spacing` is interpreted as the perpendicular distance between parallel
#' lines for `"rectangular"` transects, and as the pivot-to-pivot distance
#' along the baseline for `"zigzag"` transects. Because a smaller spacing packs
#' more transect into each polygon, the total length is a decreasing function
#' of spacing, which [OSCARS::oscars()] searches over a single scalar.
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

  prep <- makeLinesPrep(polys, baseline, type, angle,
                        needStations = type == "zigzag")

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

  # Expected total length across all polygons at spacing s. Rectangular
  # on-effort is exact in closed form (A/s), so a single representative offset
  # suffices. Zigzag length depends on the (random) pivot phase, so the
  # objective averages the realized length over several phases to target the
  # expectation and keep makeLines()'s single random draw unbiased.
  sumAt <- function(s, off, phase) {
    sum(vapply(prep, function(pp)
      makeLinesPolyTotal(pp, type, angle, s, off, minLenM, phase), numeric(1)))
  }
  totalAt <- function(s) {
    if (type == "rectangular") {
      sumAt(s, s * 0.5, 0.5)
    } else {
      mean(vapply(c(0.3, 0.7), function(ph) sumAt(s, 0, ph), numeric(1)))
    }
  }
  # 'target' selects whether targetLength refers to total length (on-effort plus
  # off-effort transit) or on-effort length only. They differ only for
  # rectangular transects; for zigzags total == on-effort. Rectangular on-effort
  # is A/s in closed form (Cauchy-Crofton).
  totalArea <- sum(vapply(prep, function(pp) pp$areaM2, numeric(1)))
  quantAt <- function(s) {
    if (type == "rectangular" && target == "onEffort") totalArea / s else totalAt(s)
  }
  obj <- function(par) abs(quantAt(par[1]) - targetM)

  # Search bounds for the scalar spacing.
  if (type == "rectangular") {
    s0  <- sum(vapply(prep, function(pp) pp$areaM2, numeric(1))) / targetM
    lwr <- s0 * 0.15; upr <- s0 * 3; start <- s0
  } else {
    blen <- vapply(prep, function(pp) pp$baseLen, numeric(1))
    lwr  <- min(blen) / 256; upr <- max(blen); start <- max(blen) / 4
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
