#' @title makeLines - Place random transects at a given spacing
#'
#' @description
#' Place line transects inside one or more polygons at a supplied `spacing`,
#' using a random start. For each polygon a random offset between 0 and
#' `spacing` is drawn, then transects are generated and clipped to the
#' polygon. This routine does the placement only; [findSpacing()] computes a
#' spacing that yields a target survey length, and [drawTransects()] chains
#' the two. Optionally, `R` independent random replicates can be returned.
#'
#' @param sPoly An `sf` or `sfc` object containing the polygon(s) 
#' (`MULTIPOLYGON` or `POLYGON` geometries) within which
#' transects will be placed. `sPoly` must be projected to a planar
#' coordinate system whose linear unit is meters (ideally an equal-area
#' projection so areas are undistorted). Geographic coordinates are rejected.
#'
#' @param type The transect layout, one of:
#' - `"rectangular"` (the default): parallel, equally spaced straight lines.
#' - `"zigzag"`: a single continuous path that zig-zags between opposite edges
#'   of the polygon, crossing a baseline between successive pivots.
#'
#' @param angle Orientation of `"rectangular"` transects, in degrees.
#' `angle = 0` (the default) produces North-South transects; `angle = 90`
#' produces East-West transects. Ignored when `type = "zigzag"`.
#'
#' @param spacing The distance between adjacent transects, as a
#' `units` length object. For `"rectangular"` transects this is the
#' perpendicular distance between the parallel lines. For `"zigzag"` transects
#' it is the distance along the baseline between the points where *adjacent*
#' legs cross it, i.e., between adjacent crossings of the baseline; one
#' complete zig-zag cycle (out and back) therefore covers `2 * spacing` of
#' baseline. Under both layouts, `spacing` is the distance between neighboring
#' transects, so the two are directly comparable. Typically obtained from
#' [findSpacing()].
#'
#' @param baseline Optional `sf`/`sfc` `LINESTRING` giving the reference line
#' for the transects. For `"zigzag"` transects the baseline is the line the
#' route crosses between pivots: pivots are placed where perpendiculars to the
#' baseline, spaced `spacing` apart with a random start, meet the polygon
#' boundary. When `NULL` (the default) a straight baseline is estimated from
#' the polygon (see Details). For `"rectangular"` transects the baseline is
#' only used for reporting and plotting; when `NULL` it is a straight line
#' through the polygon centroid perpendicular to the transects. Supplying a
#' baseline is only allowed when `sPoly` contains a single polygon. A supplied
#' baseline is extended past the polygon's bounding box before use, so that
#' pivots are placed all the way through the polygon, but it is reported back
#' in the `summary` attribute exactly as given.
#'
#' @param combine Logical controlling the shape of the returned object. Both
#' forms carry the same `onEffortLength` and `totalLength` totals; they differ
#' in the geometry returned.
#' - `TRUE` (the default): return the route actually flown, one continuous
#'   `LINESTRING` per polygon (parallel legs joined into a mow-the-grass route;
#'   zigzags are already continuous). The route is *not* clipped, so it includes
#'   the off-effort transit, and the sum of the returned lengths is the total
#'   length. A route through a concave polygon can therefore run outside it.
#' - `FALSE`: return one row per transect leg, clipped to the polygon so that no
#'   geometry falls outside it, and the sum of the returned lengths is the
#'   on-effort length. A leg that a concavity breaks into several pieces is
#'   returned as a single `MULTILINESTRING` row, so there is still exactly one
#'   row per leg. For rectangular transects each leg's `totalLength` still
#'   includes the off-effort distance to the next leg.
#'
#' @param R Number of independent random replicates to generate. Defaults to 1.
#' The returned object stacks `R` complete transect sets, one per replicate,
#' and always carries an `id` column labelling each set `"Replicate0001"`,
#' `"Replicate0002"`, and so on. Replicate numbers are zero-padded to at least
#' four digits (more if `R > 9999`).
#'
#' @param minSolidity For `"zigzag"` transects with an estimated baseline, a
#' warning is issued when a polygon's solidity (area divided by convex-hull
#' area) falls below this value; the warning recommends splitting the polygon
#' with [convexPartition()] and re-running on the pieces. Set to 0 to disable.
#' Defaults to 0.75.
#'
#' @param plot Logical. If `TRUE`, the polygons, baselines, and generated
#' transects are drawn on the current graphics device. When `R > 1`, each
#' replicate is drawn in a different colour (`rainbow(min(R, 50))`, cycling if
#' `R > 50`). Defaults to `FALSE`.
#'
#' @details
#' The returned transects distinguish *on-effort* length (the parts of the
#' lines inside the polygon) from *total* length (on-effort plus off-effort
#' transit). For `"rectangular"` transects the total includes the connectors
#' joining successive parallel legs. A `"zigzag"` route has no connectors: it
#' is one continuous line whose pivots sit on the polygon boundary, so its
#' total equals its on-effort length unless a concavity pushes part of a leg
#' outside the polygon.
#'
#' `combine` chooses which of the two the geometry represents. With
#' `combine = TRUE` the geometry is the route as flown, unclipped, so that it
#' stays connected; across a concave polygon it can leave the polygon, and those
#' excursions are counted as off-effort transit. With `combine = FALSE` each leg
#' is clipped to the polygon, so nothing is drawn outside it and the returned
#' geometry is the surveyed line only. Summing the length of the returned
#' geometry therefore gives the total length when `combine = TRUE` and the
#' on-effort length when `combine = FALSE`; the `onEffortLength` and
#' `totalLength` columns are computed the same way under both, so either form
#' reports both quantities.
#'
#' When `type = "zigzag"` and no `baseline` is supplied, the baseline is
#' estimated in three steps. First the polygon's centerline is traced by
#' sweeping cross sections along its principal axis and keeping the midpoint of
#' each. Those midpoints wander with every bend in the boundary, so, second,
#' they are straightened by regressing their Y coordinates on their X
#' coordinates and keeping only the slope of that regression. Third, the
#' resulting direction is translated to pass through the polygon's centroid
#' (even when the centroid falls outside the polygon) and drawn well past the
#' polygon's bounding box in both directions. The baseline is therefore always
#' straight; strongly bent polygons are better handled by splitting them with
#' [convexPartition()] (see `minSolidity`) than by a curved baseline.
#'
#' @return An `sf` data frame of transect lines (geometries are
#' either `LINESTRING` or `MULTILINESTRING`; a leg that clipping broke into
#' pieces is one `MULTILINESTRING` row) with columns:
#' \item{transectType}{The layout used, `"rectangular"` or `"zigzag"`.}
#' \item{id}{Replicate label, e.g. `"Replicate0001"`.}
#' \item{polygon}{Integer index of the source polygon. Source polygons and 
#' indices are 
#' given in attribute "summary". }
#' \item{leg}{Integer identifier of the leg within its polygon (1 when
#' `combine = TRUE`, in which case the route is one long line with connectors).}
#' \item{onEffortLength}{On-effort (surveyed) length of the transect(s)
#' represented on the row, i.e., the part inside the polygon. With units.
#' Equals the length of the row's geometry when `combine = FALSE`.}
#' \item{totalLength}{Total transect length of the segment(s) represented on
#' the row. Total transect length includes the on-effort segment as well as 
#' the off-effort segment connecting to the next on-effort leg. With units.}
#'
#' A summary of the design is attached as
#' an attribute `attr(x, "summary")`, a list with the layout
#' `type`, the number of polygons `nPolygons`, the number of replicates
#' `nReplicates`, the `spacing` used, `targetLength` (`NA` unless set by
#' [drawTransects()]), and `polygons`.  The `polygons` element is an 
#' `sf` `LINESTRING` data frame whose
#' geometry column, `baseline`, holds each polygon's baseline, with columns
#' `polygon` (integer index), `area` (with units), and `solidity` (measure of 
#' concavity; 1 = concave; <1 = less concave).
#'
#' @author Original version in pronghornLT: Tom Prebyl, 
#' Jason Carlisle, and Garrett Catlin.  
#' Updated and generalized for Rdistance:  Trent McDonald
#'
#' @seealso [findSpacing()], [drawTransects()].
#'
#' @examples
#' # A simple tapered survey polygon (Alaska Albers, meters).
#' poly <- sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(list(rbind(
#'   c(0, 0), c(60000, 0), c(60000, 25000), c(0, 10000), c(0, 0)))),
#'   crs = 3338))
#'
#' # Rectangular transects at a 5 km spacing 
#' set.seed(243579)
#' rec <- makeLines(poly, type = "rectangular", spacing = units::set_units(5, "km"))
#' c(onEffort = sum(rec$onEffortLength), total = sum(rec$totalLength))
#' plot(poly$geometry)
#' plot(rec$geometry, add=T, col="red")
#'
#' # Zigzag transects, adjacent legs crossing the baseline 4 km apart.
#' zz <- makeLines(poly, type = "zigzag", spacing = units::set_units(4, "km"))
#' sum(zz$totalLength)
#' plot(poly$geometry)
#' plot(zz$geometry, add=T, col="red")
#'
#' # combine = FALSE returns one row per leg, clipped to the polygon.
#' zzLegs <- makeLines(poly, type = "zigzag", spacing = units::set_units(4, "km"),
#'                     combine = FALSE)
#' sum(zzLegs$onEffortLength)
#'
#' @export
makeLines <- function(sPoly,
                      type = c("rectangular", "zigzag"),
                      angle = 0,
                      spacing,
                      baseline = NULL,
                      combine = TRUE,
                      R = 1,
                      minSolidity = 0.75,
                      plot = FALSE) {

  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package 'sf' is required. Please install it with install.packages('sf').")
  }
  type <- match.arg(type)
  makeLinesRequireLength(spacing, "spacing")
  R <- as.integer(R)
  if (is.na(R) || R < 1L) stop("'R' must be a positive integer.")

  polys <- makeLinesPolygons(sPoly)
  if (length(polys) == 0) {
    stop("'sPoly' contains no POLYGON or MULTIPOLYGON geometries.")
  }
  if (isTRUE(sf::st_is_longlat(polys[[1]]))) {
    stop("'sPoly' must be projected to a planar CRS whose linear unit is ",
         "meters (ideally an equal-area projection).")
  }
  if (!is.null(baseline) && length(polys) > 1) {
    stop("Supply 'baseline' only when 'sPoly' contains a single polygon.")
  }

  sM      <- makeLinesAsMeters(spacing)
  minLenM <- makeLinesAsMeters(units::set_units(100, "m"))
  uStr    <- units::deparse_unit(spacing)
  toOut   <- function(xm) units::set_units(units::set_units(xm, "m"),
                                           value = uStr, mode = "standard")

  prep <- makeLinesPrep(polys, baseline, type, angle)
  nP   <- length(prep)

  if (type == "zigzag" && is.null(baseline) && minSolidity > 0) {
    for (k in seq_len(nP)) {
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

  nDigits     <- max(4L, floor(log10(R)) + 1L)   # >= 4 digits, e.g. "Replicate0001"
  rowsList    <- list()
  producedAny <- logical(nP)
  warned      <- logical(nP)

  for (rep in seq_len(R)) {
    idVal <- paste0("Replicate", formatC(rep, width = nDigits, flag = "0"))
    for (k in seq_len(nP)) {
      pp   <- prep[[k]]
      geom <- NULL; legCol <- NULL; onECol <- NULL; totCol <- NULL

      if (type == "rectangular") {
        offM <- stats::runif(1, 0, sM)                 # random start
        g    <- makeLinesRectGen(pp$poly, angle, sM, offM, minLenM)
        if (length(g$legs) == 0) {
          if (!warned[k]) { warning("Polygon ", k, " produced no transects."); warned[k] <- TRUE }
          next
        }
        asm <- makeLinesAssemble(g$legs, g$lineId, g$sortDir)
        if (combine) {
          onE  <- sum(asm$onEff)
          tot  <- as.numeric(sf::st_length(asm$route))
          geom <- asm$route; legCol <- 1L; onECol <- onE; totCol <- tot
        } else {
          onEv <- asm$onEff
          totv <- onEv + asm$internal + c(asm$connectorBefore[-1], 0)
          geom <- asm$geom; legCol <- seq_along(asm$geom); onECol <- onEv; totCol <- totv
        }
      } else {
        # The zigzag is one continuous flight line whose pivots sit on the
        # polygon boundary, so it has no connectors. Its on-effort length is the
        # part of it inside the polygon, which is the whole route unless a
        # concavity pushes a leg outside.
        route <- zigzagRoute(pp$poly, pp$baseUse, sM,
                             phase     = stats::runif(1),
                             startSide = sample(c(TRUE, FALSE), 1))
        if (is.null(route)) {
          if (!warned[k]) { warning("Polygon ", k, " produced no transects."); warned[k] <- TRUE }
          next
        }
        if (combine) {
          # The whole flight line, unclipped, so that it stays connected.
          geom   <- route
          legCol <- 1L
          onECol <- makeLinesOnEffort(route, pp$poly)
          totCol <- as.numeric(sf::st_length(route))
        } else {
          # One row per leg, clipped to the polygon so that nothing is drawn
          # outside it. Clipping only removes off-effort geometry, so the
          # on-effort length of a clipped leg is its full length, and the
          # unclipped leg lengths still carry the totals.
          legs   <- zigzagSplitRoute(route)
          geom   <- makeLinesClipLegs(legs, pp$poly)
          legCol <- seq_along(legs)
          onECol <- vapply(seq_along(geom),
                           function(i) makeLinesLength(geom[i]), numeric(1))
          totCol <- as.numeric(sf::st_length(legs))
        }
      }

      n  <- length(geom)
      df <- data.frame(transectType = rep(type, n),
                       id            = rep(idVal, n),
                       polygon       = rep(k, n),
                       leg           = legCol,
                       stringsAsFactors = FALSE)
      df$onEffortLength <- toOut(onECol)
      df$totalLength    <- toOut(totCol)
      rowsList[[length(rowsList) + 1L]] <- sf::st_sf(df, geometry = geom)
      producedAny[k] <- TRUE
    }
  }

  if (length(rowsList) == 0) {
    stop("No transect lines were generated for any polygon.")
  }
  out <- do.call(rbind, rowsList)

  # Per-polygon summary as an sf LINESTRING data frame whose geometry column is
  # the baseline. On-effort and total lengths vary by replicate, so they are not
  # summarized here; the user aggregates the output columns (e.g. by 'id').
  kk  <- which(producedAny)
  pdf <- data.frame(polygon = kk)
  pdf$area     <- units::set_units(
                    units::set_units(vapply(prep[kk], function(p) p$areaM2, numeric(1)), "m^2"),
                    "km^2")
  pdf$solidity <- vapply(prep[kk], function(p) p$solidity, numeric(1))
  baseSfc      <- do.call(c, lapply(prep[kk], function(p) p$base))
  polygonsSf   <- sf::st_sf(pdf, baseline = baseSfc)

  attr(out, "summary") <- list(
      type         = type,
      nPolygons    = nP,
      nReplicates  = R,
      spacing      = toOut(sM),
      targetLength = NA,
      polygons     = polygonsSf)

  if (plot) {
    plot(sf::st_geometry(do.call(c, polys)), col = "grey90", border = "grey40")
    for (p in prep) plot(p$base, add = TRUE, col = "blue", lty = 2)
    if (R > 1L) {
      # One rainbow colour per replicate, cycling if there are more than 50.
      cols   <- grDevices::rainbow(min(R, 50L))
      repIdx <- match(out$id, sort(unique(out$id)))
      colVec <- cols[((repIdx - 1L) %% length(cols)) + 1L]
      plot(sf::st_geometry(out), add = TRUE, col = colVec, lwd = 1.5)
    } else {
      plot(sf::st_geometry(out), add = TRUE, col = "red", lwd = 2)
    }
  }

  out
}
