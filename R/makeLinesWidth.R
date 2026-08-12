# Internal helpers for measuring a polygon's cross-section perpendicular to a
# midline. Ported (and simplified to depend only on sf) from the width /
# cross-segment routines in the RUtilities package. Not exported.

# Local cross-section of the polygon along the perpendicular through a point:
# the boundary crossings immediately on each side of the point. On a concave
# polygon the perpendicular can re-enter the polygon far away, so we take the
# crossings that straddle the pivot rather than the most extreme ones. Returns
# list(pos, neg, width) or NULL when the point is not interior to a section.
makeLinesCrossExtremes <- function(poly, px, py, nd, span) {
  seg <- sf::st_sfc(sf::st_linestring(matrix(
    c(px - span * nd[1], py - span * nd[2],
      px + span * nd[1], py + span * nd[2]),
    ncol = 2, byrow = TRUE)), crs = sf::st_crs(poly))
  ix <- suppressWarnings(sf::st_intersection(seg, poly))
  if (length(ix) == 0 || all(sf::st_is_empty(ix))) return(NULL)
  cc <- sf::st_coordinates(ix)
  cc <- cc[, c("X", "Y"), drop = FALSE]
  if (nrow(cc) < 2) return(NULL)
  # Signed distance of each crossing along the normal direction. The pivot
  # sits at tproj = 0; take the nearest crossing on each side.
  tproj  <- (cc[, 1] - px) * nd[1] + (cc[, 2] - py) * nd[2]
  posIdx <- which(tproj > 0)
  negIdx <- which(tproj < 0)
  if (length(posIdx) == 0 || length(negIdx) == 0) return(NULL)
  ip <- posIdx[which.min(tproj[posIdx])]   # nearest crossing above pivot
  im <- negIdx[which.max(tproj[negIdx])]   # nearest crossing below pivot
  list(pos   = cc[ip, ],
       neg   = cc[im, ],
       width = tproj[ip] - tproj[im])
}
