# Internal helper utilities shared by makeLines() and its transect engines.
# None of these are exported; they carry no roxygen so that roxygen2 does not
# generate .Rd files for them. See makeLines() for the public interface.

# Convert a length 'units' object to a numeric number of meters. All public
# length arguments are required to carry units (see makeLinesRequireLength).
makeLinesAsMeters <- function(x) {
  as.numeric(units::set_units(x, "m"))
}

# Validate that a length argument carries units convertible to meters.
makeLinesRequireLength <- function(x, nm) {
  if (is.null(x)) return(invisible())
  if (!inherits(x, "units")) {
    stop("'", nm, "' must have measurement units attached by the 'units' ",
         "package, e.g. units::set_units(1000, \"km\").", call. = FALSE)
  }
  ok <- tryCatch({ units::set_units(x, "m"); TRUE }, error = function(e) FALSE)
  if (!ok) {
    stop("'", nm, "' must have length units convertible to meters.",
         call. = FALSE)
  }
  invisible()
}

# Extract a list of single POLYGON sfc geometries from an sf/sfc input. Rows are
# kept separate (no dissolving across rows); a MULTIPOLYGON in one row is split
# into its component POLYGONs. Non-polygonal geometries are ignored.
makeLinesPolygons <- function(sPoly) {
  g <- sf::st_geometry(sPoly)
  out <- list()
  for (i in seq_along(g)) {
    gi <- g[i]
    ti <- as.character(sf::st_geometry_type(gi))
    if (ti == "MULTIPOLYGON") {
      parts <- suppressWarnings(sf::st_cast(gi, "POLYGON"))
      for (k in seq_along(parts)) out[[length(out) + 1L]] <- parts[k]
    } else if (ti == "POLYGON") {
      out[[length(out) + 1L]] <- gi
    }
  }
  out
}

# Bounding-box diagonal of a polygon (meters). Used to size lines that must
# span the polygon before clipping.
makeLinesSpan <- function(poly) {
  bb <- sf::st_bbox(poly)
  as.numeric(sqrt((bb["xmax"] - bb["xmin"])^2 + (bb["ymax"] - bb["ymin"])^2))
}

# Unit tangent (direction) vectors at each point of an ordered coordinate
# matrix, using centered differences.
makeLinesLocalDirs <- function(cc) {
  n <- nrow(cc)
  d <- matrix(0, n, 2)
  d[1, ] <- cc[2, ] - cc[1, ]
  d[n, ] <- cc[n, ] - cc[n - 1, ]
  if (n > 2) {
    d[2:(n - 1), ] <- cc[3:n, ] - cc[1:(n - 2), ]
  }
  len <- sqrt(rowSums(d^2))
  len[len == 0] <- 1
  d / len
}

# Keep only the (multi)linestring pieces from an intersection result and split
# them into individual LINESTRINGs.
makeLinesToLinestrings <- function(g) {
  if (length(g) == 0) return(g)
  g <- g[!sf::st_is_empty(g)]
  if (length(g) == 0) return(g)
  gt <- as.character(sf::st_geometry_type(g))
  g <- g[gt %in% c("LINESTRING", "MULTILINESTRING", "GEOMETRYCOLLECTION")]
  if (length(g) == 0) return(g)
  g <- suppressWarnings(sf::st_collection_extract(g, "LINESTRING"))
  suppressWarnings(sf::st_cast(g, "LINESTRING"))
}

# Join clipped rectangular pieces into a single continuous mow-the-grass
# (serpentine / boustrophedon) route. Pieces are first regrouped by their source
# line ('lineId') into whole transects, so a line that a concavity split into
# several pieces is treated as ONE transect. Transects are ordered across the
# polygon along the spacing direction 'nVec' and traversed in alternating
# directions along the transect direction (first low->high, next high->low, ...),
# so successive transects join at the same end and the connectors run across the
# ends of the polygon, never back over its interior. Within a split transect the
# pieces are visited in order and the gaps between them are filled straight along
# the transect line.
#
# Returns a list with:
#   route           length-1 sfc: the continuous LINESTRING (all connectors)
#   geom            sfc, one geometry per transect (LINESTRING, or MULTILINESTRING
#                   when the transect was split), re-ordered and re-oriented
#   onEff           numeric: on-effort length (sum of piece lengths) per transect
#   internal        numeric: off-effort gap length within each transect
#   connectorBefore numeric: connector length entering each transect (0 for first)
makeLinesAssemble <- function(legs, lineId, nVec) {
  n <- length(legs)
  crs <- sf::st_crs(legs)
  if (n == 0) {
    return(list(route = legs, geom = legs, onEff = numeric(0),
                internal = numeric(0), connectorBefore = numeric(0)))
  }
  uVec <- c(-nVec[2], nVec[1])          # transect direction (perp to spacing)
  coordsList <- lapply(seq_len(n), function(i)
    sf::st_coordinates(legs[i])[, c("X", "Y"), drop = FALSE])

  # Group pieces into transects by source line, ordered across the polygon.
  transects <- split(seq_len(n), lineId)
  crossPos  <- vapply(transects, function(idx)
    mean(vapply(idx, function(i) mean(coordsList[[i]] %*% nVec), numeric(1))),
    numeric(1))
  transects <- transects[order(crossPos)]
  nt <- length(transects)

  path     <- NULL
  prevEnd  <- NULL
  geomList <- vector("list", nt)
  onEff    <- numeric(nt)
  internal <- numeric(nt)
  connBefore <- numeric(nt)

  for (t in seq_len(nt)) {
    idx <- transects[[t]]
    # Order this transect's pieces along the transect direction and orient each
    # piece low -> high.
    idx <- idx[order(vapply(idx, function(i) min(coordsList[[i]] %*% uVec), numeric(1)))]
    pieces <- lapply(idx, function(i) {
      cc <- coordsList[[i]]
      pr <- as.numeric(cc %*% uVec)
      if (pr[1] > pr[length(pr)]) cc <- cc[nrow(cc):1, , drop = FALSE]
      cc
    })
    # Serpentine: reverse the whole transect on every other pass.
    if (t %% 2L == 0L) pieces <- lapply(rev(pieces), function(cc) cc[nrow(cc):1, , drop = FALSE])

    onEff[t] <- sum(vapply(pieces, function(cc) sum(sqrt(rowSums(diff(cc)^2))), numeric(1)))
    if (length(pieces) > 1) {
      internal[t] <- sum(vapply(seq_len(length(pieces) - 1L), function(m)
        sqrt(sum((pieces[[m]][nrow(pieces[[m]]), ] - pieces[[m + 1L]][1, ])^2)),
        numeric(1)))
    }
    tStart <- pieces[[1]][1, ]
    tEnd   <- pieces[[length(pieces)]][nrow(pieces[[length(pieces)]]), ]
    if (!is.null(prevEnd)) connBefore[t] <- sqrt(sum((prevEnd - tStart)^2))

    geomList[[t]] <- if (length(pieces) == 1) sf::st_linestring(pieces[[1]])
                     else sf::st_multilinestring(pieces)
    for (cc in pieces) path <- rbind(path, cc)
    prevEnd <- tEnd
  }

  route <- sf::st_sfc(sf::st_linestring(path), crs = crs)
  geom  <- sf::st_sfc(geomList, crs = crs)
  list(route = route, geom = geom, onEff = onEff, internal = internal,
       connectorBefore = connBefore)
}

# A simplified copy of a polygon, used for the many auxiliary geometry
# operations (baseline, cross-section stations, connector/length measurement)
# where full boundary resolution is unnecessary. Final transect clipping still
# uses the original polygon. Larger 'denom' = finer; falls back to the original
# if simplification collapses.
makeLinesSimplify <- function(poly, denom = 1000) {
  tol <- makeLinesSpan(poly) / denom
  ps  <- suppressWarnings(sf::st_simplify(poly, dTolerance = tol,
                                          preserveTopology = TRUE))
  if (length(ps) == 0 || any(sf::st_is_empty(ps)) ||
      !all(as.character(sf::st_geometry_type(ps)) == "POLYGON")) {
    return(poly)
  }
  ps
}

# Per-polygon preparation shared by findSpacing() and makeLines(): the polygon
# geometry (full and simplified), its area and solidity, a baseline, and (for
# zigzags) the stations along the baseline. The baseline is a user-supplied
# line if given; otherwise a curved centerline for zigzags or a straight line
# through the centroid for rectangular transects. Centerline and stations are
# built on the simplified polygon for speed. One element per polygon.
makeLinesPrep <- function(polys, baseline, type, angleDeg, needStations) {
  lapply(seq_along(polys), function(k) {
    poly    <- polys[[k]]
    polyAux <- makeLinesSimplify(poly, 1000)   # baseline / stations
    if (!is.null(baseline)) {
      base <- suppressWarnings(sf::st_cast(sf::st_geometry(baseline), "LINESTRING"))
      base <- base[which.max(as.numeric(sf::st_length(base)))]
    } else if (type == "zigzag") {
      base <- makeLinesCenterline(polyAux)
    } else {
      base <- makeLinesRectBaseline(poly, angleDeg)
    }
    stations <- if (needStations) makeLinesZigzagStations(polyAux, base) else NULL
    list(poly = poly, polyAux = polyAux, base = base,
         baseLen = as.numeric(sf::st_length(base)), stations = stations,
         areaM2 = as.numeric(sf::st_area(poly)),
         solidity = makeLinesSolidity(polyAux))
  })
}

# Total transect length (on-effort plus off-effort connectors/transit) for one
# prepared polygon at a given spacing and offset. Used as the building block of
# the findSpacing() objective and for reporting in makeLines().
makeLinesPolyTotal <- function(pp, type, angleDeg, spacingM, offsetM, minLenM,
                               phase = 0.5) {
  if (type == "rectangular") {
    # Expected on-effort length of a random parallel grid over area A is A/s
    # (Cauchy-Crofton), independent of offset - so on-effort is exact and
    # unbiased. Only the off-effort connectors are measured geometrically.
    onEff <- pp$areaM2 / spacingM
    g <- makeLinesRectGen(pp$polyAux, angleDeg, spacingM, offsetM, minLenM)
    if (length(g$legs) == 0) return(onEff)
    asm <- makeLinesAssemble(g$legs, g$lineId, g$sortDir)
    connectors <- as.numeric(sf::st_length(asm$route)) - sum(asm$onEff)
    onEff + connectors
  } else {
    # The zigzag flight line is not clipped, so its length is simply the sum of
    # chord lengths between successive pivots. 'phase' lets the caller average
    # over pivot positions.
    nz   <- makeLinesNZags(pp$baseLen, spacingM)
    path <- makeLinesZigzagPath(pp$stations, nz, TRUE, phase)
    if (is.null(path)) return(0)
    sum(sqrt(rowSums(diff(path)^2)))
  }
}
