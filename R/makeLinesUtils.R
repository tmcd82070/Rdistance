# Internal helpers shared by makeLines(), findSpacing(), and drawTransects().
# Nothing here is exported and nothing carries roxygen comments, so roxygen2
# generates no .Rd files for these. See makeLines() for the public interface.
#
# Contents, in order:
#   units and input checks  makeLinesAsMeters, makeLinesRequireLength
#   polygon helpers         makeLinesPolygons, makeLinesSpan, makeLinesSimplify,
#                           makeLinesSolidity, makeLinesToLinestrings,
#                           makeLinesLength, makeLinesOnEffort
#   baselines               makeLinesMidPoints, makeLinesZigzagBaseline,
#                           makeLinesRectBaseline, makeLinesExtendLine
#   rectangular transects   makeLinesRectGen, makeLinesAssemble
#   zigzag transects        zigzagRoute, zigzagStations, zigzagCrossSegments,
#                           zigzagSplitRoute, makeLinesClipLegs
#   drivers                 makeLinesPrep, makeLinesPolyTotal
#
# The zigzag routines are ports of zigZagRoute(), placeTransects(), and
# polygonCrossSegments() from the RUtilities package, reduced to the branches
# used here and rewritten to depend only on 'sf' (RUtilities uses 'sfheaders').


# ---- units and input checks ------------------------------------------------

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


# ---- polygon helpers -------------------------------------------------------

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
# span, or reach beyond, the polygon.
makeLinesSpan <- function(poly) {
  bb <- sf::st_bbox(poly)
  as.numeric(sqrt((bb["xmax"] - bb["xmin"])^2 + (bb["ymax"] - bb["ymin"])^2))
}

# A simplified copy of a polygon, used for the auxiliary geometry operations
# (baseline estimation, length measurement inside findSpacing()) where full
# boundary resolution is unnecessary. Final transect placement still uses the
# original polygon. Larger 'denom' = finer; falls back to the original if
# simplification collapses.
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

# Solidity (a convexity measure): polygon area divided by convex-hull area.
# 1 = convex; smaller = more concave. Returns NA if the hull has no area.
makeLinesSolidity <- function(poly) {
  a <- as.numeric(sf::st_area(poly))
  h <- as.numeric(sf::st_area(sf::st_convex_hull(poly)))
  if (!is.finite(h) || h <= 0) return(NA_real_)
  a / h
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

# Total planar length (meters) of an sfc of lines. Transect polygons must be
# projected (makeLines() rejects long-lat), so this equals sf::st_length() but
# without its per-call overhead, which is substantial and would dominate the
# findSpacing() search. Vertices are summed within, never across, parts.
makeLinesLength <- function(line) {
  cc <- sf::st_coordinates(line)
  if (nrow(cc) < 2) return(0)
  idc <- setdiff(colnames(cc), c("X", "Y", "Z", "M"))
  grp <- if (length(idc) > 0) do.call(paste, as.data.frame(cc[, idc, drop = FALSE]))
         else rep("1", nrow(cc))
  d <- sqrt(rowSums(diff(cc[, c("X", "Y"), drop = FALSE])^2))
  sum(d[grp[-1] == grp[-length(grp)]])
}

# On-effort length (meters) of a line: the part of it that lies inside the
# polygon. Zigzag routes are flown as one continuous line whose pivots sit on
# the boundary, so a concave polygon can push part of a leg outside; that part
# is transit, not survey.
makeLinesOnEffort <- function(line, poly) {
  ix <- suppressWarnings(sf::st_intersection(sf::st_geometry(line),
                                             sf::st_geometry(poly)))
  ix <- makeLinesToLinestrings(ix)
  if (length(ix) == 0) return(0)
  makeLinesLength(ix)
}


# ---- baselines -------------------------------------------------------------
#
# The "baseline" is the reference line along which transects are laid out. For
# zigzag transects it is the line whose perpendiculars carry the pivots, and
# the zigzag crosses it once between every pair of pivots. For rectangular
# transects it is the direction across which the parallel lines are spaced.
# Both are straight, and both are drawn well beyond the polygon so that
# transects are placed all the way through it.

# Midpoints of the polygon's cross sections, swept along its principal (long)
# axis: at each station a perpendicular cut is taken and the midpoint of the
# longest interior piece is kept. Returns a two-column matrix of midpoints, or
# NULL when fewer than two cuts land inside the polygon. These midpoints trace
# the polygon's (possibly curved) centerline; makeLinesZigzagBaseline()
# straightens them into a baseline.
makeLinesMidPoints <- function(poly, nStations = 45) {
  crd <- sf::st_coordinates(poly)[, c("X", "Y")]
  O   <- colMeans(crd)
  ev  <- eigen(stats::var(crd), symmetric = TRUE)$vectors
  u   <- ev[, 1]                       # long axis
  v   <- ev[, 2]                       # short axis (cutting direction)

  p  <- (crd[, 1] - O[1]) * u[1] + (crd[, 2] - O[2]) * u[2]
  sr <- range(p)
  R  <- max(sqrt(rowSums(sweep(crd, 2, O)^2))) * 1.2

  sj   <- seq(sr[1], sr[2], length.out = nStations)
  sj   <- sj[-c(1, length(sj))]
  cuts <- lapply(sj, function(s) {
    c0 <- O + s * u
    sf::st_linestring(rbind(c0 - R * v, c0 + R * v))
  })
  cuts <- sf::st_sfc(cuts, crs = sf::st_crs(poly))
  ix   <- suppressWarnings(sf::st_intersection(cuts, sf::st_geometry(poly)))
  gt   <- as.character(sf::st_geometry_type(ix))
  if (any(gt == "GEOMETRYCOLLECTION")) {
    isGC     <- gt == "GEOMETRYCOLLECTION"
    ix[isGC] <- suppressWarnings(sf::st_collection_extract(ix[isGC], "LINESTRING"))
    gt       <- as.character(sf::st_geometry_type(ix))
  }
  ix <- ix[gt %in% c("LINESTRING", "MULTILINESTRING")]
  if (length(ix) < 2) return(NULL)
  ix <- suppressWarnings(sf::st_cast(ix, "MULTILINESTRING"))

  # A concave polygon splits a cut into several pieces; keep the midpoint of the
  # longest piece of each cut. Pieces are straight, so their length-weighted
  # centroid is the midpoint of their two ends.
  cc   <- sf::st_coordinates(ix)
  nc   <- ncol(cc)
  feat <- cc[, nc]                     # which cut
  rows <- split(seq_len(nrow(cc)), paste(feat, cc[, nc - 1L]))   # cut and piece
  pLen <- vapply(rows, function(j)
    sum(sqrt(rowSums(diff(cc[j, 1:2, drop = FALSE])^2))), numeric(1))
  pMid <- t(vapply(rows, function(j)
    colMeans(cc[c(j[1], j[length(j)]), 1:2, drop = FALSE]), numeric(2)))
  pCut <- vapply(rows, function(j) feat[j[1]], numeric(1))
  best <- vapply(split(seq_along(rows), pCut),
                 function(g) g[which.max(pLen[g])], integer(1))

  mids <- pMid[best, , drop = FALSE]
  if (nrow(mids) < 2) return(NULL)
  mids
}

# Straight baseline for zigzag transects. The polygon's centerline midpoints
# (makeLinesMidPoints) wander with every bend of the boundary, which makes an
# erratic zigzag, so the midpoints are straightened by regressing Y on X and
# only the slope of that regression is kept. The resulting direction is then
# translated to pass through the polygon centroid -- even when the centroid
# falls outside the polygon -- and the line is drawn past the bounding box in
# both directions so that zigzagRoute() places pivots all the way through the
# polygon. Falls back to the principal axis when the midpoints are unusable or
# exactly vertical (a vertical centerline has no Y-on-X slope).
makeLinesZigzagBaseline <- function(poly, polyAux = poly) {
  span <- makeLinesSpan(poly)
  mids <- makeLinesMidPoints(polyAux)
  dir  <- NULL
  if (!is.null(mids) && stats::sd(mids[, 1]) > 1e-8 * span) {
    b <- unname(stats::coef(stats::lm(mids[, 2] ~ mids[, 1]))[2])
    if (is.finite(b)) dir <- c(1, b) / sqrt(1 + b^2)
  }
  if (is.null(dir)) {
    crd <- sf::st_coordinates(polyAux)[, c("X", "Y")]
    dir <- eigen(stats::var(crd), symmetric = TRUE)$vectors[, 1]
  }
  cen <- sf::st_coordinates(sf::st_centroid(sf::st_geometry(poly)))[1, 1:2]
  ext <- 1.1 * span                    # reaches outside the bounding box
  sf::st_sfc(sf::st_linestring(rbind(cen - ext * dir, cen + ext * dir)),
             crs = sf::st_crs(poly))
}

# Straight baseline for rectangular transects: a line through the polygon
# centroid, perpendicular to the transects (i.e., along the spacing direction),
# spanning the polygon's extent. The centroid may fall outside the polygon; the
# baseline is not clipped, so it still passes through the centroid.
makeLinesRectBaseline <- function(poly, angleDeg) {
  cen  <- sf::st_coordinates(sf::st_centroid(poly))[1, 1:2]
  a    <- angleDeg * pi / 180
  nVec <- c(cos(a), -sin(a))           # perpendicular to the transect lines
  crd  <- sf::st_coordinates(poly)[, c("X", "Y")]
  proj <- (crd[, 1] - cen[1]) * nVec[1] + (crd[, 2] - cen[2]) * nVec[2]
  r    <- range(proj)
  p1   <- cen + r[1] * nVec
  p2   <- cen + r[2] * nVec
  sf::st_sfc(sf::st_linestring(rbind(p1, p2)), crs = sf::st_crs(poly))
}

# Extend a line by 'ext' meters at both ends, continuing straight along its
# first and last segments. Used on a user-supplied baseline so that pivots are
# placed all the way through the polygon; the line the user gave is still the
# one reported back to them.
makeLinesExtendLine <- function(line, ext) {
  cc <- sf::st_coordinates(line)[, c("X", "Y"), drop = FALSE]
  n  <- nrow(cc)
  if (n < 2) return(line)
  dStart <- cc[1, ] - cc[2, ]
  dEnd   <- cc[n, ] - cc[n - 1, ]
  dStart <- dStart / sqrt(sum(dStart^2))
  dEnd   <- dEnd   / sqrt(sum(dEnd^2))
  cc <- rbind(cc[1, ] + ext * dStart, cc, cc[n, ] + ext * dEnd)
  sf::st_sfc(sf::st_linestring(cc), crs = sf::st_crs(line))
}


# ---- rectangular transects -------------------------------------------------

# Generate the family of equally spaced parallel lines at 'angleDeg', offset by
# 'offsetM' meters, clipped to the polygon and pruned of pieces shorter than
# 'minLenM'. A concave polygon can split one line into several pieces; each
# piece is tagged (lineId) with the source line so the assembler can regroup
# them into one transect. Returns list(legs = sfc of clipped LINESTRINGs,
# lineId = integer source-line index per piece, sortDir = the spacing direction).
makeLinesRectGen <- function(poly, angleDeg, spacingM, offsetM, minLenM) {
  bb   <- sf::st_bbox(poly)
  xmid <- mean(bb[c("xmin", "xmax")])
  ymid <- mean(bb[c("ymin", "ymax")])
  rmax <- makeLinesSpan(poly) / 2

  a    <- angleDeg * pi / 180
  uVec <- c(sin(a),  cos(a))   # direction the transect lines run
  nVec <- c(cos(a), -sin(a))   # direction across which lines are spaced

  empty <- list(legs = sf::st_sfc(crs = sf::st_crs(poly)),
                lineId = integer(0), sortDir = nVec)
  ps <- seq(offsetM - rmax, rmax, by = spacingM)
  if (length(ps) < 1) return(empty)

  segs <- lapply(ps, function(pp) {
    cx <- xmid + pp * nVec[1]
    cy <- ymid + pp * nVec[2]
    sf::st_linestring(matrix(
      c(cx - rmax * uVec[1], cy - rmax * uVec[2],
        cx + rmax * uVec[1], cy + rmax * uVec[2]),
      ncol = 2, byrow = TRUE))
  })
  grid <- sf::st_sfc(segs, crs = sf::st_crs(poly))
  clip <- makeLinesToLinestrings(suppressWarnings(sf::st_intersection(grid, poly)))
  if (length(clip) == 0) return(empty)
  clip <- clip[as.numeric(sf::st_length(clip)) >= minLenM]
  if (length(clip) == 0) return(empty)

  # Recover each piece's source line: every point of a line projects onto nVec
  # to that line's offset pp, so match each piece's projection to the ps grid.
  co         <- sf::st_coordinates(clip)
  firstPts   <- co[!duplicated(co[, "L1"]), c("X", "Y"), drop = FALSE]
  centerProj <- xmid * nVec[1] + ymid * nVec[2]
  pieceProj  <- as.numeric(firstPts %*% nVec) - centerProj
  lineId     <- vapply(pieceProj, function(pv) which.min(abs(ps - pv)), integer(1))

  list(legs = clip, lineId = lineId, sortDir = nVec)
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


# ---- zigzag transects ------------------------------------------------------
#
# Port of RUtilities::zigZagRoute(). Stations are spaced regularly along the
# baseline with a random start. At each station a cross segment perpendicular
# to the baseline is cut across the polygon; the zigzag then connects the ends
# of successive cross segments, alternating sides.
#
# 'spacingM' is the distance between the points where adjacent legs cross the
# baseline, which equals the station spacing: leg i crosses the baseline
# between stations i and i+1, so consecutive crossings are one station apart.
# (RUtilities::zigZagRoute() instead takes the length of a complete zig-zag
# cycle, which is twice this; leg-to-leg is the more intuitive measure and is
# what makeLines() and findSpacing() expose.)

# Stations along the baseline, spacingM apart. 'phase' in [0, 1) is the
# fractional offset of the first station, i.e. the random start; passing a fixed
# value makes the layout reproducible, which findSpacing() relies on. Returns a
# two-column matrix. This is sf::st_line_sample() done by hand: it is called
# once per candidate spacing inside the findSpacing() search, where the sf
# overhead dominates the arithmetic.
zigzagStations <- function(baseline, spacingM, phase, nMax = 2000L) {
  cc  <- sf::st_coordinates(baseline)[, c("X", "Y"), drop = FALSE]
  d   <- c(0, cumsum(sqrt(rowSums(diff(cc)^2))))
  len <- d[length(d)]
  n   <- as.integer(round(len / spacingM))
  n   <- max(2L, min(nMax, n))
  at  <- (seq_len(n) - 1 + phase) / n * len
  i   <- findInterval(at, d, all.inside = TRUE)
  w   <- (at - d[i]) / (d[i + 1L] - d[i])
  cbind(cc[i, 1] + w * (cc[i + 1L, 1] - cc[i, 1]),
        cc[i, 2] + w * (cc[i + 1L, 2] - cc[i, 2]))
}

# Cross sections of the polygon at each station, perpendicular to the baseline.
# Stations outside the polygon simply contribute nothing. When a cross section
# leaves and re-enters a concave polygon it is resolved to its full extent, the
# two extreme crossings, matching multiResolve = "extent" in RUtilities. Returns
# list(pid = station index kept, pos = far-side ends, neg = near-side ends), or
# NULL when fewer than two stations cross the polygon.
zigzagCrossSegments <- function(poly, crd, span, minWidth = 6) {
  n <- nrow(crd)
  # Direction of travel at each station; the last station inherits the previous
  # station's direction. A straight baseline has one direction throughout.
  v <- rbind(crd[-1, , drop = FALSE] - crd[-n, , drop = FALSE], 0)
  v[n, ] <- v[max(1L, n - 1L), ]
  len <- sqrt(rowSums(v^2))
  len[len == 0] <- 1
  v   <- v / len
  nrm <- cbind(v[, 2], -v[, 1])        # perpendicular to the baseline

  # Most stations on an extended baseline sit beyond the polygon. A station's
  # cross segment can only reach the polygon when the station's position along
  # the baseline falls between the extreme positions of the bounding box, so
  # cut the rest before handing any geometry to sf.
  bb  <- sf::st_bbox(poly)
  cnr <- cbind(bb[c("xmin", "xmax", "xmin", "xmax")],
               bb[c("ymin", "ymin", "ymax", "ymax")])
  tC  <- cnr %*% t(v)                                    # 4 corners x stations
  tS  <- rowSums(crd * v)
  ok  <- which(tS >= pmin(tC[1, ], tC[2, ], tC[3, ], tC[4, ]) &
               tS <= pmax(tC[1, ], tC[2, ], tC[3, ], tC[4, ]))
  if (length(ok) < 2) return(NULL)

  # Cut well past the polygon in both directions, so a cross segment spans it
  # even when the baseline runs outside the polygon.
  reach <- 2 * span
  segs <- lapply(ok, function(i) {
    sf::st_linestring(rbind(crd[i, ] - reach * nrm[i, ],
                            crd[i, ] + reach * nrm[i, ]))
  })
  lns <- sf::st_sfc(segs, crs = sf::st_crs(poly))

  ix  <- suppressWarnings(sf::st_intersection(lns, sf::st_geometry(poly)))
  if (length(ix) < 2) return(NULL)
  pid <- ok[attr(ix, "idx")[, 1]]

  # A cross segment can graze a vertex, which intersects in a point rather than
  # a line; keep the linear parts only.
  gt <- as.character(sf::st_geometry_type(ix))
  if (any(gt == "GEOMETRYCOLLECTION")) {
    isGC     <- gt == "GEOMETRYCOLLECTION"
    ix[isGC] <- suppressWarnings(sf::st_collection_extract(ix[isGC], "LINESTRING"))
    gt       <- as.character(sf::st_geometry_type(ix))
  }
  isLine <- gt %in% c("LINESTRING", "MULTILINESTRING")
  if (sum(isLine) < 2) return(NULL)
  ix  <- suppressWarnings(sf::st_cast(ix[isLine], "MULTILINESTRING"))
  pid <- pid[isLine]

  # Signed position of every crossing along its station's perpendicular. The
  # two extremes span the polygon's full cross section at that station.
  cc  <- sf::st_coordinates(ix)
  fid <- cc[, ncol(cc)]                # which cross segment each vertex is on
  np  <- nrm[pid[fid], , drop = FALSE]
  tp  <- cc[, "X"] * np[, 1] + cc[, "Y"] * np[, 2]
  rows <- seq_along(tp)
  iMax <- vapply(split(rows, fid), function(j) j[which.max(tp[j])], integer(1))
  iMin <- vapply(split(rows, fid), function(j) j[which.min(tp[j])], integer(1))

  keep <- (tp[iMax] - tp[iMin]) > minWidth
  if (sum(keep) < 2) return(NULL)
  list(pid = pid[keep],
       pos = cc[iMax[keep], c("X", "Y"), drop = FALSE],
       neg = cc[iMin[keep], c("X", "Y"), drop = FALSE])
}

# A zig-zag route through one polygon: an 'sfc' LINESTRING connecting the ends
# of successive cross segments, alternating sides of the baseline. 'spacingM' is
# the baseline distance between adjacent legs. 'startSide' picks which side the
# route starts on. The route is returned unclipped, so it is one continuous
# flight line; the caller measures how much of it lies inside the polygon (see
# makeLinesOnEffort) and clips it when the legs are returned separately (see
# makeLinesClipLegs). Returns NULL when the polygon cannot hold a zigzag at
# this spacing.
zigzagRoute <- function(poly, baseline, spacingM,
                        phase     = stats::runif(1),
                        startSide = sample(c(TRUE, FALSE), 1)) {
  span <- makeLinesSpan(poly)
  crd  <- zigzagStations(baseline, spacingM, phase)
  cs   <- zigzagCrossSegments(poly, crd, span)
  if (is.null(cs)) return(NULL)
  # Alternate on the station index, not on the position within the kept set, so
  # that stations dropped for missing the polygon do not flip the pattern.
  takePos <- xor(startSide, (cs$pid %% 2L) == 0L)
  path <- cs$neg
  path[takePos, ] <- cs$pos[takePos, , drop = FALSE]
  if (nrow(path) < 2) return(NULL)
  sf::st_sfc(sf::st_linestring(path), crs = sf::st_crs(poly))
}

# Split a zigzag route into its individual legs, one LINESTRING per consecutive
# pair of pivots. Used when combine = FALSE.
zigzagSplitRoute <- function(route) {
  cc <- sf::st_coordinates(route)[, c("X", "Y"), drop = FALSE]
  if (nrow(cc) < 2) return(sf::st_sfc(crs = sf::st_crs(route)))
  segs <- lapply(seq_len(nrow(cc) - 1L), function(i) {
    sf::st_linestring(cc[i:(i + 1L), , drop = FALSE])
  })
  sf::st_sfc(segs, crs = sf::st_crs(route))
}

# Clip legs to the polygon, returning exactly one geometry per input leg. A leg
# that lies wholly inside is returned unchanged; a leg that a concavity breaks
# into several pieces becomes a MULTILINESTRING; a leg lying wholly outside
# becomes an empty MULTILINESTRING. Keeping one geometry per leg (rather than
# one per surviving piece) is what lets makeLines() return one row per leg with
# combine = FALSE, so leg numbering and the per-leg total lengths -- which
# include the off-effort parts just removed -- stay aligned with the route.
makeLinesClipLegs <- function(legs, poly) {
  out <- rep(list(sf::st_multilinestring()), length(legs))
  ix  <- suppressWarnings(sf::st_intersection(legs, sf::st_geometry(poly)))
  idx <- attr(ix, "idx")
  if (length(ix) > 0 && !is.null(idx)) {
    for (i in unique(idx[, 1])) {
      pieces <- makeLinesToLinestrings(ix[idx[, 1] == i])
      if (length(pieces) == 0) next
      cl <- lapply(seq_along(pieces), function(j)
        sf::st_coordinates(pieces[j])[, c("X", "Y"), drop = FALSE])
      out[[i]] <- if (length(cl) == 1) sf::st_linestring(cl[[1]])
                  else                 sf::st_multilinestring(cl)
    }
  }
  sf::st_sfc(out, crs = sf::st_crs(legs))
}


# ---- drivers ---------------------------------------------------------------

# Per-polygon preparation shared by findSpacing() and makeLines(): the polygon
# geometry (full and simplified), its area and solidity, and its baseline. Two
# versions of the baseline are kept: 'base' is the one reported back to the user
# (a user-supplied baseline is reported exactly as given), and 'baseUse' is the
# one transects are built from, extended past the bounding box so that transects
# are placed all the way through the polygon. Baselines are estimated from the
# simplified polygon for speed.
makeLinesPrep <- function(polys, baseline, type, angleDeg) {
  lapply(seq_along(polys), function(k) {
    poly    <- polys[[k]]
    polyAux <- makeLinesSimplify(poly, 1000)
    span    <- makeLinesSpan(poly)
    if (!is.null(baseline)) {
      base <- suppressWarnings(sf::st_cast(sf::st_geometry(baseline), "LINESTRING"))
      base <- base[which.max(as.numeric(sf::st_length(base)))]
      baseUse <- makeLinesExtendLine(base, 1.1 * span)
    } else if (type == "zigzag") {
      base    <- makeLinesZigzagBaseline(poly, polyAux)
      baseUse <- base
    } else {
      base    <- makeLinesRectBaseline(poly, angleDeg)
      baseUse <- base
    }
    list(poly = poly, polyAux = polyAux, base = base, baseUse = baseUse,
         span = span,
         areaM2 = as.numeric(sf::st_area(poly)),
         solidity = makeLinesSolidity(polyAux))
  })
}

# Transect length for one prepared polygon at a given spacing and offset, used
# as the building block of the findSpacing() objective. 'onEffort' selects the
# surveyed length rather than the total length (survey plus off-effort transit).
# Measurements use the simplified polygon; findSpacing() only needs the length,
# not the geometry.
makeLinesPolyTotal <- function(pp, type, angleDeg, spacingM, offsetM, minLenM,
                               phase = 0.5, startSide = TRUE, onEffort = FALSE) {
  if (type == "rectangular") {
    # Expected on-effort length of a random parallel grid over area A is A/s
    # (Cauchy-Crofton), independent of offset - so on-effort is exact and
    # unbiased. Only the off-effort connectors are measured geometrically.
    onEff <- pp$areaM2 / spacingM
    g <- makeLinesRectGen(pp$polyAux, angleDeg, spacingM, offsetM, minLenM)
    if (length(g$legs) == 0) return(onEff)
    asm <- makeLinesAssemble(g$legs, g$lineId, g$sortDir)
    connectors <- makeLinesLength(asm$route) - sum(asm$onEff)
    onEff + connectors
  } else {
    # 'phase' and 'startSide' fix the random start so the objective is a smooth
    # function of spacing; the caller averages over both. Which side the route
    # starts on matters: it decides which stations get the far pivot and which
    # the near one, and on a polygon that is not symmetric about its baseline
    # the two choices give routes of different length.
    route <- zigzagRoute(pp$polyAux, pp$baseUse, spacingM,
                         phase = phase, startSide = startSide)
    if (is.null(route)) return(0)
    if (onEffort) makeLinesOnEffort(route, pp$polyAux)
    else          makeLinesLength(route)
  }
}
