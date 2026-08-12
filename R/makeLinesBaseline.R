# Internal helpers for transect baselines. Not exported; no roxygen.
#
# The "baseline" is the reference line used to lay out transects. For zigzag
# transects it is a curved centerline through the polygon (or one supplied by
# the user); pivots are spaced along it. For rectangular transects it is a
# straight line through the polygon centroid, perpendicular to the transects,
# representing the direction across which the parallel lines are spaced.

# Curved centerline through a polygon, found by sweeping stations along the
# polygon's principal (long) axis, cutting a perpendicular cross-section at
# each station, and connecting the cross-section midpoints. This follows arcs
# and bends ("around the corner") where a single straight line cannot.
makeLinesCenterline <- function(poly, nStations = 45) {
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
  mids <- matrix(NA_real_, length(sj), 2)
  for (j in seq_along(sj)) {
    c0  <- O + sj[j] * u
    cut <- sf::st_sfc(sf::st_linestring(rbind(c0 - R * v, c0 + R * v)),
                      crs = sf::st_crs(poly))
    ix  <- makeLinesToLinestrings(suppressWarnings(sf::st_intersection(cut, poly)))
    if (length(ix) == 0) next
    ix  <- ix[which.max(as.numeric(sf::st_length(ix)))]
    m   <- sf::st_coordinates(sf::st_centroid(ix))
    mids[j, ] <- m[1, 1:2]
  }
  mids <- mids[!is.na(mids[, 1]), , drop = FALSE]

  if (nrow(mids) < 2) {
    return(makeLinesStraightCenterline(poly, O, u, R))
  }
  mids <- makeLinesSmooth(mids, 3)
  sf::st_sfc(sf::st_linestring(mids), crs = sf::st_crs(poly))
}

# Fallback centerline: a single straight line along the long axis through an
# interior point, clipped to the polygon (longest interior piece).
makeLinesStraightCenterline <- function(poly, O, u, R) {
  pos <- sf::st_coordinates(sf::st_point_on_surface(poly))[1, 1:2]
  ln  <- sf::st_sfc(sf::st_linestring(rbind(pos - R * u, pos + R * u)),
                    crs = sf::st_crs(poly))
  ln  <- makeLinesToLinestrings(suppressWarnings(sf::st_intersection(ln, poly)))
  if (length(ln) == 0) {
    stop("Could not build a baseline inside a polygon. Supply a 'baseline'.")
  }
  ln[which.max(as.numeric(sf::st_length(ln)))]
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

# Simple moving-average smoother for an ordered coordinate matrix; endpoints
# are preserved to keep the centerline's full extent.
makeLinesSmooth <- function(m, w = 3) {
  n <- nrow(m)
  if (n <= w) return(m)
  half <- (w - 1) %/% 2
  out  <- m
  for (i in seq_len(n)) {
    lo <- max(1, i - half); hi <- min(n, i + half)
    out[i, ] <- colMeans(m[lo:hi, , drop = FALSE])
  }
  out[1, ] <- m[1, ]; out[n, ] <- m[n, ]
  out
}

# Solidity (a convexity measure): polygon area divided by convex-hull area.
# 1 = convex; smaller = more concave. Returns NA if the hull has no area.
makeLinesSolidity <- function(poly) {
  a <- as.numeric(sf::st_area(poly))
  h <- as.numeric(sf::st_area(sf::st_convex_hull(poly)))
  if (!is.finite(h) || h <= 0) return(NA_real_)
  a / h
}
