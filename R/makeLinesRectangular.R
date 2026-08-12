# Internal generation helpers for "rectangular" (parallel, mow-the-grass)
# transects. Not exported; no roxygen. The spacing search lives in findSpacing()
# and the random placement in makeLines(); these routines only build geometry.

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
