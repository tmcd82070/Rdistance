#' @title Partition a concave polygon into more-convex pieces
#'
#' @description
#' Splits a concave (non-convex) polygon into several more-convex sub-polygons
#' using *Approximate Convex Decomposition* (ACD). Survey-design routines such
#' as [makeLines()] and [drawTransects()] place transects most efficiently
#' inside reasonably convex polygons; a strongly concave polygon (e.g., a
#' banana-shaped arc, or a coastline wrapping a bay) is better surveyed as a
#' handful of convex pieces. This function recommends *where* and *how* to cut.
#'
#' @details
#' Convex decomposition of a polygon is a classical problem in computational
#' geometry. Exact minimum-piece methods (Keil & Snoeyink; Chazelle & Dobkin)
#' produce the fewest convex pieces but, on real, densely-digitized boundaries,
#' generate hundreds of sliver polygons because every digitizing wiggle is a
#' true reflex vertex. This routine instead implements *Approximate Convex
#' Decomposition* (Lien & Amato 2006), which is governed by a concavity
#' tolerance and therefore yields a small number of meaningful cuts.
#'
#' The algorithm is recursive. For the current polygon it:
#'
#' 1. computes each vertex's *concavity* --- its straight-line distance to the
#'    polygon's convex hull;
#' 2. finds the vertex of greatest concavity. If that concavity is below
#'    `concavityTol`, the polygon is accepted as "convex enough" and returned
#'    unchanged;
#' 3. otherwise it resolves that vertex with the shortest *interior chord* (a
#'    diagonal that stays inside the polygon and splits it into two pieces of
#'    non-trivial area), then recurses on each piece.
#'
#' For a banana-shaped arc this places a single cut straight across the
#' "waist" at the apex --- exactly where one would cut by hand.
#'
#' Cuts are *decided* on a lightly simplified copy of the boundary (via
#' Douglas-Peucker, [sf::st_simplify()]) so the concavity measure is not fooled
#' by digitizing noise, but they are *applied* to the original full-resolution
#' boundary, so the returned pieces retain the true coastline. Because
#' Douglas-Peucker retains a subset of the original vertices, every cut joins
#' two original vertices and the pieces tile the input exactly (total area is
#' conserved). No triangulation and no dependency beyond `sf` is required.
#'
#' A typical workflow is to run [drawTransects()], see its low-solidity
#' warning, split the offending polygon with `convexPartition()`, and re-run
#' [drawTransects()] on the resulting pieces.
#'
#' Distances (and hence `concavityTol` and `simplifyTol`) are measured in the
#' units of `x`'s coordinate reference system, so `x` should be projected to a
#' planar CRS (ideally equal-area) whose linear unit is meters, the same
#' requirement as [makeLines()].
#'
#' @param x An `sf`, `sfc`, or `sfg` `POLYGON`, or a two-column matrix of
#' coordinates. If `x` contains several polygons only the first is used (with a
#' message); call the function once per feature otherwise. Holes are ignored.
#'
#' @param concavityTol Concavity tolerance, in the linear unit of `x`'s CRS
#' (e.g., meters). A piece is accepted as convex enough when no vertex lies
#' farther than this from its convex hull. Larger values give fewer, less
#' convex pieces; smaller values give more, tighter pieces. When `NA` (the
#' default) it is set to 5% of the hull "width" (`sqrt(hull area)`).
#'
#' @param simplifyTol Douglas-Peucker tolerance (same units as `concavityTol`)
#' used only to decide *where* to cut, not to alter the returned boundary. When
#' `NA` (the default) it is set to `concavityTol / 4`. Use `0` to disable
#' simplification and decide cuts on the full-resolution boundary (slower, and
#' more sensitive to digitizing noise).
#'
#' @param minPieceFrac A candidate cut is rejected if it would create a piece
#' smaller than this fraction of the parent's area. Guards against sliver
#' pieces. Defaults to 0.02.
#'
#' @param maxPieces Safety cap on the number of pieces produced. Defaults
#' to 200.
#'
#' @return An `sf` `POLYGON` data frame with one row per piece and columns:
#' \item{piece}{Integer piece id, ordered largest-area first.}
#' \item{solidity}{Area divided by convex-hull area; `1` for a convex piece,
#' smaller for a concave one.}
#' \item{area}{Piece area, with units (from [sf::st_area()]).}
#'
#' The pieces tile the input polygon (their union equals `x` and their areas
#' sum to the area of `x`). The CRS of `x` is carried through. When `x` is
#' already convex enough, a one-row data frame containing the (unchanged)
#' polygon is returned.
#'
#' @references
#' Lien, J.-M. and Amato, N. M. (2006) Approximate convex decomposition of
#' polygons. *Computational Geometry* 35(1-2):100-123.
#'
#' @author Trent McDonald.
#'
#' @seealso [drawTransects()] and [makeLines()], which warn when an input
#' polygon's solidity is low and recommend this routine.
#'
#' @examples
#' # A banana-shaped (concave) polygon in a planar CRS (meters).
#' arc <- sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(list(rbind(
#'   c(0, 0), c(40, 30), c(80, 40), c(120, 30), c(160, 0),
#'   c(150, 0), c(115, 22), c(80, 30), c(45, 22), c(10, 0), c(0, 0)))),
#'   crs = 3338))
#'
#' sf::st_area(arc) / sf::st_area(sf::st_convex_hull(arc))  # low solidity
#'
#' pieces <- convexPartition(arc, concavityTol = 8)
#' nrow(pieces)          # two more-convex pieces, cut at the apex
#' pieces$solidity
#' plot(pieces["piece"])
#'
#' @export
convexPartition <- function(x,
                            concavityTol = NA,
                            simplifyTol  = NA,
                            minPieceFrac = 0.02,
                            maxPieces    = 200) {

  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package 'sf' is required. Please install it with install.packages('sf').")
  }

  ## --- coerce input to a single sfc POLYGON --------------------------------
  crs <- sf::NA_crs_
  if (inherits(x, "sf"))  { crs <- sf::st_crs(x); x <- sf::st_geometry(x) }
  if (inherits(x, "sfc")) {
    crs <- sf::st_crs(x)
    if (length(x) > 1) message("'x' has ", length(x),
                               " features; using the first.")
    x <- x[[1]]
  }
  if (is.matrix(x)) {
    if (!all(x[1, ] == x[nrow(x), ])) x <- rbind(x, x[1, ])
    x <- sf::st_polygon(list(x))
  }
  if (!inherits(x, "POLYGON")) {
    stop("'x' must be an sf/sfc/sfg POLYGON or a coordinate matrix.")
  }
  poly0 <- sf::st_sfc(x, crs = crs)

  ## --- helpers -------------------------------------------------------------
  # Outer ring of an sfc polygon as an open coordinate matrix (no closing dup).
  ringMat <- function(sfcpoly) {
    m <- sf::st_coordinates(sfcpoly)
    m <- m[m[, "L1"] == 1, c("X", "Y"), drop = FALSE]
    if (all(m[1, ] == m[nrow(m), ])) m <- m[-nrow(m), , drop = FALSE]
    m
  }
  mkPoly <- function(mat) {
    sf::st_sfc(sf::st_polygon(list(rbind(mat, mat[1, , drop = FALSE]))),
               crs = crs)
  }
  # Vertex concavity = distance from each vertex to the convex hull boundary.
  concavity <- function(mat) {
    hb  <- sf::st_cast(sf::st_convex_hull(mkPoly(mat)), "LINESTRING")
    pts <- sf::st_sfc(lapply(seq_len(nrow(mat)),
                             function(i) sf::st_point(mat[i, ])), crs = crs)
    as.numeric(sf::st_distance(pts, hb))
  }
  # Is chord (i, j) an interior diagonal of polygon 'poly'?
  interiorDiag <- function(mat, i, j, poly) {
    seg <- sf::st_sfc(sf::st_linestring(rbind(mat[i, ], mat[j, ])), crs = crs)
    as.logical(sf::st_covered_by(seg, poly, sparse = FALSE)[1, 1])
  }
  # Split a ring into the two chains sharing vertices i and j.
  splitRing <- function(mat, i, j) {
    n <- nrow(mat)
    a <- if (i <= j) i:j else c(i:n, 1:j)
    b <- if (j <= i) j:i else c(j:n, 1:i)
    list(A = mat[a, , drop = FALSE], B = mat[b, , drop = FALSE])
  }
  # Index of the row of 'mat' nearest to coordinate 'pt' (exact when pt is a
  # copied vertex; robust to floating-point noise otherwise).
  nearestIdx <- function(mat, pt) {
    which.min((mat[, 1] - pt[1])^2 + (mat[, 2] - pt[2])^2)
  }
  ringDist <- function(a, b, n) { d <- abs(a - b); min(d, n - d) }

  ## --- size-based defaults -------------------------------------------------
  hullArea <- as.numeric(sf::st_area(sf::st_convex_hull(poly0)))
  scale    <- sqrt(hullArea)
  if (is.na(concavityTol)) concavityTol <- 0.05 * scale
  if (is.na(simplifyTol))  simplifyTol  <- concavityTol / 4

  ## --- simplified ring drives the cut decisions; original ring is split ----
  omat0 <- ringMat(poly0)
  work  <- if (simplifyTol > 0) {
    sf::st_simplify(poly0, dTolerance = simplifyTol, preserveTopology = TRUE)
  } else poly0
  smat0 <- ringMat(work)
  if (nrow(smat0) < 4) smat0 <- omat0            # over-simplified; fall back
  minArea <- minPieceFrac * abs(as.numeric(sf::st_area(poly0)))

  ## --- recursive ACD -------------------------------------------------------
  # Carries the simplified sub-ring (clean concavity, candidate cut vertices)
  # alongside the original sub-ring (validity tests and returned geometry).
  pieces <- list()
  acd <- function(smat, omat, depth = 0) {
    if (length(pieces) + 1 >= maxPieces || depth > 40) {
      pieces[[length(pieces) + 1]] <<- omat; return(invisible())
    }
    cc <- concavity(smat)
    m  <- which.max(cc)
    if (cc[m] <= concavityTol) {                  # convex enough
      pieces[[length(pieces) + 1]] <<- omat; return(invisible())
    }
    ns    <- nrow(smat)
    opoly <- mkPoly(omat)
    im    <- nearestIdx(omat, smat[m, ])
    # candidate partners among simplified vertices, tried shortest chord first
    cand  <- setdiff(seq_len(ns), m)
    cand  <- cand[vapply(cand, function(w) ringDist(w, m, ns) >= 2, logical(1))]
    len   <- sqrt((smat[cand, 1] - smat[m, 1])^2 + (smat[cand, 2] - smat[m, 2])^2)
    cand  <- cand[order(len)]
    chosen <- NA_integer_
    for (w in cand) {
      iw <- nearestIdx(omat, smat[w, ])
      if (ringDist(im, iw, nrow(omat)) < 2) next
      if (!interiorDiag(omat, im, iw, opoly)) next
      pr <- splitRing(omat, im, iw)
      aA <- abs(as.numeric(sf::st_area(mkPoly(pr$A))))
      aB <- abs(as.numeric(sf::st_area(mkPoly(pr$B))))
      if (min(aA, aB) < minArea) next
      chosen <- w; iwSel <- iw; break
    }
    if (is.na(chosen)) {                          # unresolvable; keep as-is
      pieces[[length(pieces) + 1]] <<- omat; return(invisible())
    }
    so <- splitRing(smat, m, chosen)
    oo <- splitRing(omat, im, iwSel)
    acd(so$A, oo$A, depth + 1)
    acd(so$B, oo$B, depth + 1)
  }
  acd(smat0, omat0)

  ## --- assemble sf POLYGON data frame --------------------------------------
  polys <- do.call(c, lapply(pieces, mkPoly))
  polys <- sf::st_make_valid(polys)
  # keep only polygonal parts and ensure single POLYGON rows (make_valid can
  # emit a GEOMETRYCOLLECTION or MULTIPOLYGON at a pinched cut)
  if (any(as.character(sf::st_geometry_type(polys)) != "POLYGON")) {
    polys <- sf::st_collection_extract(polys, "POLYGON")
    polys <- sf::st_cast(polys, "POLYGON")
  }

  ar    <- as.numeric(sf::st_area(polys))
  keep  <- ar >= minArea * 0.25
  polys <- polys[keep]
  ord   <- order(-as.numeric(sf::st_area(polys)))
  polys <- polys[ord]

  sol <- vapply(seq_along(polys), function(k) {
    as.numeric(sf::st_area(polys[k]) / sf::st_area(sf::st_convex_hull(polys[k])))
  }, numeric(1))

  sf::st_sf(piece    = seq_along(polys),
            solidity = round(sol, 4),
            area     = sf::st_area(polys),
            geometry = polys)
}
