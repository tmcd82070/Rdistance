# Internal generation helpers for "zigzag" transects. Not exported; no roxygen.
# A zigzag sweeps back and forth across the polygon between opposite edges,
# pivoting on points spaced along a central line. The spacing search lives in
# findSpacing() and the random placement in makeLines(); these routines only
# build geometry. The geometry ideas (perpendicular cross-sections along a
# midline, alternating edge points) are ported from the RUtilities package and
# simplified to depend only on sf.

# Precompute, at N stations along the midline, the polygon-edge points on each
# side of the midline. Returns matrices pos[N,2], neg[N,2] and widths[N] (NA
# where the perpendicular does not cross the polygon).
makeLinesZigzagStations <- function(poly, midline, N = 120) {
  span <- makeLinesSpan(poly)
  pos  <- (seq_len(N) - 0.5) / N
  pts  <- sf::st_line_sample(midline, sample = pos)
  cc   <- sf::st_coordinates(pts)[, c("X", "Y"), drop = FALSE]
  N    <- nrow(cc)
  dirs <- makeLinesLocalDirs(cc)
  P <- matrix(NA_real_, N, 2)
  Q <- matrix(NA_real_, N, 2)
  W <- rep(NA_real_, N)
  for (i in seq_len(N)) {
    nd <- c(dirs[i, 2], -dirs[i, 1])
    ce <- makeLinesCrossExtremes(poly, cc[i, 1], cc[i, 2], nd, span)
    if (!is.null(ce)) { P[i, ] <- ce$pos; Q[i, ] <- ce$neg; W[i] <- ce$width }
  }
  list(pos = P, neg = Q, width = W)
}

# Number of pivot points implied by a pivot spacing 'spacingM' (measured along a
# midline of length 'midLenM'), at least 2 and at most the station count.
makeLinesNZags <- function(midLenM, spacingM, nMax = 120L) {
  nz <- as.integer(round(midLenM / spacingM)) + 1L
  max(2L, min(nMax, nz))
}

# Build the ordered zigzag vertices for nz pivots by selecting nz evenly spaced
# stations and alternating between the two edges. 'startPos' mirrors the pattern
# (does not change length); 'phase' in [0, 1) shifts the pivots along the
# midline, giving a random start.
makeLinesZigzagPath <- function(st, nz, startPos, phase = 0.5) {
  N   <- nrow(st$pos)
  positions <- (seq_len(nz) - 1 + phase) / nz         # in [0, 1)
  idx <- pmin(N, pmax(1L, round(positions * N + 0.5)))
  path <- matrix(NA_real_, length(idx), 2)
  ok   <- logical(length(idx))
  for (k in seq_along(idx)) {
    i <- idx[k]
    if (is.na(st$width[i])) next
    takePos   <- xor(startPos, (k %% 2) == 0)
    path[k, ] <- if (takePos) st$pos[i, ] else st$neg[i, ]
    ok[k]     <- TRUE
  }
  path <- path[ok, , drop = FALSE]
  if (nrow(path) < 2) return(NULL)
  path
}

# Split a zigzag path (vertex matrix) into its individual straight legs, one
# LINESTRING per consecutive pair of vertices. Used for combine = FALSE. The
# path is the flight line between boundary waypoints and is not clipped, so the
# legs are exactly the flown segments and carry no off-effort transit.
makeLinesSplitPath <- function(path, crs) {
  if (is.null(path) || nrow(path) < 2) return(sf::st_sfc(crs = crs))
  segs <- lapply(seq_len(nrow(path) - 1), function(i) {
    sf::st_linestring(path[i:(i + 1), , drop = FALSE])
  })
  sf::st_sfc(segs, crs = crs)
}
