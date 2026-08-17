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
#' The number of pieces is either left to a concavity tolerance, fixed by the
#' caller (`nPieces`), or chosen automatically; and the cut locations are found
#' either greedily (`method = "fast"`) or by direct search with
#' [OSCARS::oscars()] (`method = "optimum"`).
#'
#' @details
#' Convex decomposition of a polygon is a classical problem in computational
#' geometry. Exact minimum-piece methods (Keil & Snoeyink; Chazelle & Dobkin
#' 1985) produce the fewest convex pieces but, on real, densely-digitized
#' boundaries, generate hundreds of sliver polygons because every digitizing
#' wiggle is a true reflex vertex. This routine instead implements *Approximate
#' Convex Decomposition* (Lien & Amato 2006), which is governed by a concavity
#' tolerance and therefore yields a small number of meaningful cuts.
#'
#' ## The greedy ("fast") algorithm
#'
#' The fast algorithm is the top-down, one-cut-at-a-time hierarchical
#' decomposition of Lien & Amato (2006), in the fixed-component-count form
#' popularized by Mamou & Ghorbel (2009). Starting from the whole polygon it
#' repeatedly:
#'
#' 1. computes each vertex's *concavity* --- its straight-line distance to its
#'    piece's convex hull;
#' 2. selects the piece holding the single most concave vertex anywhere;
#' 3. resolves that vertex with the shortest *interior chord* (a diagonal that
#'    stays inside the piece and splits it into two pieces of non-trivial
#'    area), replacing the piece with the two halves.
#'
#' For a banana-shaped arc the first cut lands straight across the "waist" at
#' the apex --- exactly where one would cut by hand.
#'
#' Splitting stops when no vertex anywhere is more concave than `concavityTol`
#' (`nPieces = "optimum"`), or as soon as `nPieces` pieces exist (`nPieces` an
#' integer). Because the two rules use the same machinery, a fixed-count run is
#' simply a tolerance run halted early.
#'
#' ## The direct-search ("optimum") algorithm
#'
#' The greedy rule is myopic: the shortest chord at the most concave vertex is
#' locally sensible but need not lead to the best *set* of pieces. With
#' `method = "optimum"` the cut locations are instead searched over. Making
#' \eqn{k} pieces takes \eqn{k - 1} cuts, and each cut is identified by the
#' boundary vertex it starts from, so a candidate decomposition is a vector of
#' \eqn{k - 1} vertex indices. That vector is handed to [OSCARS::oscars()],
#' whose derivative-free pattern search (Hooke & Jeeves 1961) is well suited to
#' the resulting piecewise-constant objective, and the search
#'
#' \deqn{\max_{v_1,\dots,v_{k-1}} \; \min_j \; \mathrm{solidity}_j}
#'
#' maximizes the solidity of the *worst* piece, i.e. it makes the least convex
#' piece as convex as possible. Configurations that fail to produce \eqn{k}
#' pieces (a chosen vertex admits no legal chord) are penalized by the number
#' of pieces they fall short.
#'
#' Each value of \eqn{k} is searched from `nStarts` starting points. The first
#' is the greedy answer for that \eqn{k}, so `method = "optimum"` can never do
#' worse than `method = "fast"`; the rest are random vertices. Randomness means
#' repeated calls can differ --- use [set.seed()] for reproducibility.
#'
#' The best point OSCARS returns is then polished: each cut in turn is moved to
#' every other candidate vertex and any improvement is kept, repeating until a
#' full sweep finds none. The objective is flat between vertices, so a pattern
#' search can stop on a plateau with a strictly better vertex sitting next to
#' it; the polish is cheap, can only improve the answer, and makes the result
#' much less sensitive to where the random starts landed.
#'
#' When `nPieces = "optimum"` and `method = "optimum"`, the number of pieces is
#' searched too, over \eqn{k = 1, 2, \dots, 10} (the uncut polygon competes, so
#' an already-convex polygon is returned whole). Minimum solidity is almost
#' always weakly increasing in \eqn{k} --- enough small pieces are always
#' convex --- so taking the strict maximum would nearly always return 10
#' pieces. Instead the *smallest* \eqn{k} whose best minimum solidity comes
#' within `solidityTol` of the overall best is returned: the fewest pieces that
#' buy essentially all of the achievable convexity. The cap of 10 is
#' deliberate; to obtain more pieces than that, pass an integer `nPieces`.
#'
#' ## Common machinery
#'
#' Cuts are *decided* on a lightly simplified copy of the boundary (via
#' Douglas-Peucker, [sf::st_simplify()]) so the concavity measure is not fooled
#' by digitizing noise, but they are *applied* to the original full-resolution
#' boundary, so the returned pieces retain the true coastline. Because
#' Douglas-Peucker retains a subset of the original vertices, every cut joins
#' two original vertices and the pieces tile the input exactly (total area is
#' conserved). No triangulation is required.
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
#' `method = "optimum"` evaluates the objective up to `nfmax` times per start,
#' `nStarts` times per candidate piece count, and each evaluation re-cuts the
#' polygon, so it is orders of magnitude slower than `method = "fast"`. Once a
#' run has been going for 10 seconds a [progress::progress_bar] appears (the
#' same mechanism [abundEstim()] uses for bootstrap iterations); shorter runs
#' finish silently.
#'
#' @param x An `sf`, `sfc`, or `sfg` `POLYGON`, or a two-column matrix of
#' coordinates. If `x` contains several polygons only the first is used (with a
#' message); call the function once per feature otherwise. Holes are ignored.
#'
#' @param nPieces Number of pieces to produce. Either an integer `>= 2`, in
#' which case exactly that many pieces are returned, or the string `"optimum"`
#' (the default), in which case the count is chosen for you: by `concavityTol`
#' when `method = "fast"`, and by maximizing minimum solidity over
#' `2, 3, ..., 10` pieces when `method = "optimum"`. Values above 10 are
#' reachable only by passing an integer.
#'
#' @param method Character string selecting how the cut locations are chosen:
#' \describe{
#'   \item{`"fast"`}{(default) Greedy, deterministic hierarchical ACD: cut at
#'     the most concave vertex, using the shortest interior chord.}
#'   \item{`"optimum"`}{Search the cut vertices with [OSCARS::oscars()] to
#'     maximize the minimum solidity of the resulting pieces. Much slower.}
#' }
#'
#' @param concavityTol Concavity tolerance, in the linear unit of `x`'s CRS
#' (e.g., meters). A piece is accepted as convex enough when no vertex lies
#' farther than this from its convex hull. Larger values give fewer, less
#' convex pieces; smaller values give more, tighter pieces. When `NA` (the
#' default) it is set to 5% of the hull "width" (`sqrt(hull area)`). Used only
#' when `nPieces = "optimum"` and `method = "fast"`; the other combinations
#' fix or search the piece count directly and ignore it.
#'
#' @param simplifyTol Douglas-Peucker tolerance (same units as `concavityTol`)
#' used only to decide *where* to cut, not to alter the returned boundary. When
#' `NA` (the default) it is set to `concavityTol / 4`. Use `0` to disable
#' simplification and decide cuts on the full-resolution boundary (slower, and
#' more sensitive to digitizing noise). It also sets the resolution of the
#' `method = "optimum"` search, whose candidate cut vertices are the simplified
#' boundary's vertices.
#'
#' @param minPieceFrac A candidate cut is rejected if it would create a piece
#' smaller than this fraction of the parent's area. Guards against sliver
#' pieces. Defaults to 0.02.
#'
#' @param maxPieces Safety cap on the number of pieces produced. Defaults
#' to 200. Raised automatically when `nPieces` is a larger integer.
#'
#' @param nStarts Number of starting points per candidate piece count when
#' `method = "optimum"`. The first start is always the greedy solution; the
#' remainder are random vertices. Defaults to 3. Ignored when
#' `method = "fast"`.
#'
#' @param nfmax Maximum objective evaluations per start, passed to
#' [OSCARS::oscars.control()]. Defaults to 100. Larger values search harder and
#' take proportionally longer. Ignored when `method = "fast"`.
#'
#' @param solidityTol Slack used to break ties when both `nPieces` and `method`
#' are `"optimum"`: the fewest pieces whose best minimum solidity is within
#' `solidityTol` of the overall best wins. Defaults to 0.01. Larger values
#' favor fewer, larger pieces. Ignored otherwise.
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
#' Chazelle, B. and Dobkin, D. P. (1985) Optimal convex decompositions. In
#' G. T. Toussaint (ed.), *Computational Geometry*, pages 63-133. North-Holland,
#' Amsterdam.
#'
#' Hooke, R. and Jeeves, T. A. (1961) "Direct search" solution of numerical and
#' statistical problems. *Journal of the ACM* 8(2):212-229.
#' \doi{10.1145/321062.321069}
#'
#' Keil, J. M. (2000) Polygon decomposition. In J.-R. Sack and J. Urrutia
#' (eds.), *Handbook of Computational Geometry*, pages 491-518. Elsevier,
#' Amsterdam.
#'
#' Lien, J.-M. and Amato, N. M. (2006) Approximate convex decomposition of
#' polygons. *Computational Geometry* 35(1-2):100-123.
#' \doi{10.1016/j.comgeo.2005.10.005}
#'
#' Mamou, K. and Ghorbel, F. (2009) A simple and efficient approach for 3D mesh
#' approximate convex decomposition. In *16th IEEE International Conference on
#' Image Processing (ICIP)*, pages 3501-3504.
#' \doi{10.1109/ICIP.2009.5414068}
#'
#' @author Trent McDonald.
#'
#' @seealso [drawTransects()] and [makeLines()], which warn when an input
#' polygon's solidity is low and recommend this routine; [OSCARS::oscars()],
#' which performs the `method = "optimum"` search.
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
#' # Tolerance-driven: let concavityTol decide how many pieces.
#' pieces <- convexPartition(arc, concavityTol = 8)
#' nrow(pieces)          # two more-convex pieces, cut at the apex
#' pieces$solidity
#' plot(pieces["piece"])
#'
#' # Exactly three pieces, cut greedily.
#' convexPartition(arc, nPieces = 3, method = "fast")$solidity
#'
#' \donttest{
#' # Exactly three pieces, cut where OSCARS says the worst piece is best.
#' set.seed(1)
#' convexPartition(arc, nPieces = 3, method = "optimum")$solidity
#'
#' # Let the search pick the number of pieces too. Budgets are cut down here
#' # to keep the example quick; the defaults search harder.
#' set.seed(1)
#' best <- convexPartition(arc, nPieces = "optimum", method = "optimum",
#'                         nStarts = 1, nfmax = 20)
#' nrow(best)
#' best$solidity
#' }
#'
#' @export
convexPartition <- function(x,
                            nPieces      = "optimum",
                            method       = c("fast", "optimum"),
                            concavityTol = NA,
                            simplifyTol  = NA,
                            minPieceFrac = 0.02,
                            maxPieces    = 200,
                            nStarts      = 3,
                            nfmax        = 100,
                            solidityTol  = 0.01) {

  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package 'sf' is required. Please install it with install.packages('sf').")
  }

  ## --- validate the two mode-setting arguments -----------------------------
  method <- match.arg(method)
  badN <- paste0("'nPieces' must be a single integer >= 2, or the string ",
                 "\"optimum\".")
  if (is.character(nPieces)) {
    if (length(nPieces) != 1L || !identical(nPieces, "optimum")) stop(badN)
    nP <- NA_integer_
  } else {
    if (length(nPieces) != 1L || !is.numeric(nPieces) || is.na(nPieces) ||
        nPieces < 2 || nPieces != round(nPieces)) stop(badN)
    nP <- as.integer(nPieces)
  }
  if (!is.na(nP)) maxPieces <- max(maxPieces, nP)
  if (method == "optimum") {
    if (!requireNamespace("OSCARS", quietly = TRUE)) {
      stop("Package 'OSCARS' is required when method = \"optimum\".")
    }
    if (length(nStarts) != 1L || is.na(nStarts) || nStarts < 1) {
      stop("'nStarts' must be a positive integer.")
    }
    if (length(nfmax) != 1L || is.na(nfmax) || nfmax < 1) {
      stop("'nfmax' must be a positive integer.")
    }
    nStarts <- as.integer(nStarts)
    nfmax   <- as.integer(nfmax)
  }
  # Search over piece counts too, versus a fixed count, versus tolerance-driven.
  runMode <- if (method == "fast") {
               if (is.na(nP)) "tolerance" else "fixedFast"
             } else if (is.na(nP)) "searchAll" else "fixedOptim"

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
  # The two vertex chains of an n-vertex ring that share vertices i and j.
  chainIdx <- function(n, i, j) {
    a <- if (i <= j) i:j else c(i:n, 1:j)
    b <- if (j <= i) j:i else c(j:n, 1:i)
    list(A = a, B = b)
  }
  # Index of the row of 'mat' nearest to coordinate 'pt' (exact when pt is a
  # copied vertex; robust to floating-point noise otherwise).
  nearestIdx <- function(mat, pt) {
    which.min((mat[, 1] - pt[1])^2 + (mat[, 2] - pt[2])^2)
  }
  ringDist <- function(a, b, n) { d <- abs(a - b); min(d, n - d) }
  # Shoelace area of an open ring, and the area of its convex hull. On a
  # projected CRS these equal sf::st_area(); they are used instead because the
  # "optimum" search evaluates them tens of thousands of times and the sf
  # round-trip dominates the run time. Validated against sf below, with a
  # fallback for the geographic-CRS case sf measures geodesically.
  shoelace <- function(mat) {
    j <- c(seq_len(nrow(mat))[-1], 1L)
    abs(sum(mat[, 1] * mat[j, 2] - mat[j, 1] * mat[, 2])) / 2
  }
  shoelaceHull <- function(mat) {
    h <- grDevices::chull(mat)
    if (length(h) < 3L) return(0)
    shoelace(mat[h, , drop = FALSE])
  }

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
  polyArea <- abs(as.numeric(sf::st_area(poly0)))
  minArea  <- minPieceFrac * polyArea
  nSimp    <- nrow(smat0)
  # Trust the planar formulas only where they reproduce sf on the whole
  # polygon; a geographic CRS (or anything else exotic) falls back to sf.
  fastArea <- isTRUE(abs(shoelace(omat0) - polyArea) <=
                     1e-6 * max(polyArea, .Machine$double.eps))

  ## --- piece machinery -----------------------------------------------------
  # A piece is carried as index vectors into the simplified ring (sIdx, which
  # supplies concavity and candidate cut vertices) and into the original ring
  # (oIdx, which supplies validity tests and the returned geometry). Sub-rings
  # are contiguous chains of their parent, so a simplified vertex can always be
  # located in whichever piece still owns it.
  rootPiece <- function() {
    list(sIdx = seq_len(nSimp), oIdx = seq_len(nrow(omat0)),
         depth = 0L, stuck = FALSE)
  }
  pieceArea <- function(oIdx) {
    mat <- omat0[oIdx, , drop = FALSE]
    if (fastArea) return(shoelace(mat))
    abs(as.numeric(sf::st_area(mkPoly(mat))))
  }
  pieceSolidity <- function(oIdx) {
    mat <- omat0[oIdx, , drop = FALSE]
    if (fastArea) {
      h <- shoelaceHull(mat)
      if (!is.finite(h) || h <= 0) return(0)
      return(shoelace(mat) / h)
    }
    g <- mkPoly(mat)
    h <- abs(as.numeric(sf::st_area(sf::st_convex_hull(g))))
    if (!is.finite(h) || h <= 0) return(0)
    abs(as.numeric(sf::st_area(g))) / h
  }
  minSolidity <- function(pieces) {
    min(vapply(pieces, function(p) pieceSolidity(p$oIdx), numeric(1)))
  }

  # Cut piece 'pc' starting from vertex 'mLoc' of its simplified sub-ring,
  # using the shortest interior chord that leaves two pieces of usable area.
  # Returns the two children, or NULL when no legal chord exists.
  cutPiece <- function(pc, mLoc) {
    sIdx <- pc$sIdx; oIdx <- pc$oIdx
    smat <- smat0[sIdx, , drop = FALSE]
    omat <- omat0[oIdx, , drop = FALSE]
    ns   <- nrow(smat); no <- nrow(omat)
    if (ns < 4L || no < 4L) return(NULL)
    opoly <- mkPoly(omat)
    im    <- nearestIdx(omat, smat[mLoc, ])
    # candidate partners among simplified vertices, tried shortest chord first
    cand  <- setdiff(seq_len(ns), mLoc)
    cand  <- cand[vapply(cand, function(w) ringDist(w, mLoc, ns) >= 2, logical(1))]
    if (!length(cand)) return(NULL)
    len   <- sqrt((smat[cand, 1] - smat[mLoc, 1])^2 +
                  (smat[cand, 2] - smat[mLoc, 2])^2)
    cand  <- cand[order(len)]
    for (w in cand) {
      iw <- nearestIdx(omat, smat[w, ])
      if (ringDist(im, iw, no) < 2) next
      if (!interiorDiag(omat, im, iw, opoly)) next
      po <- chainIdx(no, im, iw)
      aA <- pieceArea(oIdx[po$A])
      aB <- pieceArea(oIdx[po$B])
      if (min(aA, aB) < minArea) next
      ps <- chainIdx(ns, mLoc, w)
      d  <- pc$depth + 1L
      return(list(
        list(sIdx = sIdx[ps$A], oIdx = oIdx[po$A], depth = d, stuck = FALSE),
        list(sIdx = sIdx[ps$B], oIdx = oIdx[po$B], depth = d, stuck = FALSE)))
    }
    NULL
  }

  # Greedy hierarchical ACD. Always cuts the most concave vertex anywhere.
  # Stops at 'nStop' pieces (NA = no limit) or once no vertex exceeds
  # 'tolStop' (-Inf = keep cutting regardless of concavity). Returns the piece
  # list and the simplified-ring vertices the cuts started from, which seed the
  # direct search.
  greedy <- function(nStop = NA_integer_, tolStop = -Inf) {
    pieces <- list(rootPiece())
    cuts   <- integer(0)
    concOf <- function(k) {
      if (is.null(pieces[[k]]$cc)) {
        pieces[[k]]$cc <<- concavity(smat0[pieces[[k]]$sIdx, , drop = FALSE])
      }
      pieces[[k]]$cc
    }
    repeat {
      if (length(pieces) >= maxPieces) break
      if (!is.na(nStop) && length(pieces) >= nStop) break
      live <- which(!vapply(pieces, `[[`, logical(1), "stuck") &
                     vapply(pieces, `[[`, integer(1), "depth") <= 40L)
      if (!length(live)) break
      best <- NA_integer_; bestVal <- -Inf; bestLoc <- NA_integer_
      for (k in live) {
        cc <- concOf(k)
        m  <- which.max(cc)
        if (cc[m] > bestVal) { bestVal <- cc[m]; best <- k; bestLoc <- m }
      }
      if (is.na(best) || bestVal <= tolStop) break     # all convex enough
      kids <- cutPiece(pieces[[best]], bestLoc)
      if (is.null(kids)) { pieces[[best]]$stuck <- TRUE; next }
      cuts   <- c(cuts, pieces[[best]]$sIdx[bestLoc])
      pieces <- c(pieces[-best], kids)
    }
    list(pieces = pieces, cuts = cuts)
  }

  # Cut at the given simplified-ring vertices, in order. Each vertex is cut in
  # the largest piece that still owns it; vertices admitting no legal chord in
  # any owning piece are skipped, leaving fewer pieces than cuts requested.
  applyCuts <- function(v) {
    pieces <- list(rootPiece())
    for (vv in v) {
      own <- which(vapply(pieces, function(p) any(p$sIdx == vv), logical(1)))
      if (!length(own)) next
      if (length(own) > 1L) {
        ar  <- vapply(pieces[own], function(p) pieceArea(p$oIdx), numeric(1))
        own <- own[order(-ar)]
      }
      for (k in own) {
        kids <- cutPiece(pieces[[k]], which(pieces[[k]]$sIdx == vv)[1])
        if (!is.null(kids)) { pieces <- c(pieces[-k], kids); break }
      }
    }
    pieces
  }

  ## --- progress bar --------------------------------------------------------
  # One tick per objective evaluation, with the run's unused budget ticked off
  # when a run ends early, so the bar always reaches 100%. show_after = 10
  # keeps the bar hidden unless the run really is a long one.
  pbs <- new.env(parent = emptyenv())
  pbs$pb <- NULL; pbs$done <- 0; pbs$budget <- 0
  addBudget <- function(n) pbs$budget <- pbs$budget + n
  tick <- function() {
    if (!is.null(pbs$pb) && pbs$done < pbs$budget) {
      pbs$done <- pbs$done + 1
      pbs$pb$tick()
    }
  }
  endRun <- function() {
    if (!is.null(pbs$pb) && pbs$done < pbs$budget) {
      pbs$pb$tick(pbs$budget - pbs$done)
      pbs$done <- pbs$budget
    }
  }

  ## --- direct search over cut locations ------------------------------------
  decode    <- function(theta) pmin(nSimp, pmax(1L, as.integer(round(theta))))
  maxSweeps <- 2L

  # Best (k - 1)-cut decomposition into k pieces, by pattern search on the cut
  # vertices. Objective is 1 - min(solidity), plus one unit per missing piece.
  optimK <- function(k) {
    n   <- k - 1L
    fOf <- function(v) {
      tick()
      pcs <- applyCuts(v)
      (1 - minSolidity(pcs)) + (k - length(pcs))
    }
    obj <- function(theta) fOf(decode(theta))

    warm   <- greedy(nStop = k)$cuts
    starts <- vector("list", nStarts)
    starts[[1]] <- if (length(warm) >= n) {
      as.numeric(warm[seq_len(n)])
    } else {
      c(as.numeric(warm), sample.int(nSimp, n - length(warm), replace = TRUE))
    }
    if (nStarts > 1L) {
      for (s in 2:nStarts) {
        starts[[s]] <- as.numeric(sample.int(nSimp, n, replace = TRUE))
      }
    }
    bestVal <- Inf; bestPar <- starts[[1]]
    for (st in starts) {
      addBudget(nfmax)
      o <- OSCARS::oscars(obj, n = n,
                          lwr = rep(0.5, n), upr = rep(nSimp + 0.5, n),
                          start = st,
                          controls = OSCARS::oscars.control(nfmax = nfmax,
                                                            infol = 0,
                                                            fTol  = 1e-4))
      endRun()
      if (o$value < bestVal) { bestVal <- o$value; bestPar <- o$par }
    }

    # Discrete polish. OSCARS searches a continuous box, but the objective only
    # changes as a cut jumps from one vertex to the next, so a pattern search
    # can settle on a plateau while a strictly better vertex sits beside it.
    # Sweeping one cut at a time over every vertex, keeping improvements, is
    # cheap, can only lower the objective, and makes the answer far less
    # sensitive to where the random starts happened to land.
    v <- decode(bestPar)
    addBudget(n * nSimp * maxSweeps + 1L)
    bestVal <- fOf(v)
    for (s in seq_len(maxSweeps)) {
      improved <- FALSE
      for (i in seq_len(n)) {
        for (u in seq_len(nSimp)) {
          if (u == v[i]) next
          w <- v; w[i] <- u
          f <- fOf(w)
          if (f < bestVal - 1e-12) { v <- w; bestVal <- f; improved <- TRUE }
        }
      }
      if (!improved) break
    }
    endRun()

    pcs <- applyCuts(v)
    list(pieces = pcs, minSol = minSolidity(pcs))
  }

  ## --- dispatch ------------------------------------------------------------
  if (method == "optimum") {
    kMax <- if (runMode == "searchAll") min(10L, maxPieces) else nP
    kSeq <- if (runMode == "searchAll") seq_len(kMax)[-1] else nP
    # Upper bound on objective evaluations: the OSCARS budget for every start,
    # plus a full polish, for every candidate piece count.
    nEval <- sum(nStarts * nfmax + (kSeq - 1L) * nSimp * maxSweeps + 1L)
    if (requireNamespace("progress", quietly = TRUE) && nEval > 0) {
      pbs$pb <- progress::progress_bar$new(
          format = "Optimizing partition [:bar] :percent eta: :eta"
        , total  = nEval
        , clear  = FALSE
        , show_after = 10
      )
    }
  }

  pieces <- switch(runMode,

    tolerance  = greedy(tolStop = concavityTol)$pieces,

    fixedFast  = {
      g <- greedy(nStop = nP)
      if (length(g$pieces) < nP) {
        warning("Could only split 'x' into ", length(g$pieces),
                " pieces, not the ", nP, " requested. Lower 'minPieceFrac' ",
                "or 'simplifyTol' to allow finer cuts.")
      }
      g$pieces
    },

    fixedOptim = {
      r <- optimK(nP)
      if (length(r$pieces) < nP) {
        warning("Could only split 'x' into ", length(r$pieces),
                " pieces, not the ", nP, " requested. Lower 'minPieceFrac' ",
                "or 'simplifyTol' to allow finer cuts.")
      }
      r$pieces
    },

    searchAll  = {
      kMax <- min(10L, maxPieces)
      res  <- vector("list", kMax)
      # k = 1 is the uncut polygon: cutting cannot raise its solidity above 1,
      # so an already-convex polygon wins here and is returned whole.
      res[[1]] <- list(pieces = list(rootPiece()),
                       minSol = pieceSolidity(seq_len(nrow(omat0))))
      if (kMax >= 2L) for (k in 2:kMax) res[[k]] <- optimK(k)
      ms    <- vapply(res, function(r) r$minSol, numeric(1))
      # Fewest pieces buying essentially all the achievable convexity.
      kBest <- min(which(ms >= max(ms) - solidityTol))
      res[[kBest]]$pieces
    }
  )

  ## --- assemble sf POLYGON data frame --------------------------------------
  polys <- do.call(c, lapply(pieces, function(p) {
    mkPoly(omat0[p$oIdx, , drop = FALSE])
  }))
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
