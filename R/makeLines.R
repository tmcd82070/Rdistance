#' @title Draw random transect lines in a polygon
#'
#' @description This function optimizes the placement and spacing of
#' line transects in a polygon given a desired total
#' transect length.
#'
#' @param sPoly \code{sf} object, the input polygon within which line transects
#' will be generated. The coordinate system must use meters as the 
#' linear unit (e.g. EPSG=26913)
#' 
#' @param totalLengthKm Total length of transects to generate (in kilometers)
#' 
#' @param angle Orientation of the transects. \code{angle = 0} produces
#' transects oriented North-South (the default), \code{angle = 90} produces
#' East-West transects.
#' 
#' @param minLengthKm Minimum length (km) required for an individual transect
#' line to be surveyed.
#' 
#' @param offset An offset, in km, perpendicular to transect direction 
#' to use when placing transects. By default the central
#' transect is aligned with the center of the polygon. This parameter moves 
#' the central transect one direction or the other. 
#' 
#' @param considerHuntArea Breaks transects as they cross over hunt areas
#' (defaults to TRUE).
#' 
#' @param huntAreaBuffer Buffer by which to break transects when considerHuntArea
#' is TRUE (defaults to 1000 km).
#' 
#' @param breakLongLines Splits longer transects into more digestible pieces
#' (defaults to TRUE).
#' 
#' @param breakLongLineLength Limit for maximum line length (in km) if
#' breakLongLines is TRUE (defaults to 40 km).
#' 
#' @param minSpace Optional minimal line spacing (in km) to consider when
#' optimizing transect placement (default is NULL).
#' 
#' @param maxSpace Optional maximum line spacing (in km) to consider when
#' optimizing transect placement (default is NULL).
#' 
#' @param optimTol Optional tolerance value indicating the maximum allowable
#' deviation from \code{totalLengthKm} expressed as a proportion (default is 0.01).
#' When the function fails to find a solution within the specified tolerance an
#' error is returned.
#' 
#' @param optimQuick if TRUE, the initial optimization is used. Helpful for when
#' calculations need to fail quickly.
#'
#' @return Returns a list object containing the transect lines and
#' a summary dataframe:
#' \describe{
#'   \item{lines}{\code{sf} object, simple feature collection of transect lines}
#'   \item{summary}{\code{data.frame} summary of transect lines}
#' }
#'
#' @author Tom Prebyl, Jason Carlisle, and Garrett Catlin
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' # Read in herd unit polygon
#' hu <- sf::st_read("W:/My Drive/Projects/Pronghorn_ltds/herd_units/Rattlesnake_HU.shp")
#'
#' # Make lines
#' testLines <- makeLines(sPoly = hu,
#'      totalLengthKm = 1000,
#'      angle = 0,
#'      minSpace = NULL,
#'      maxSpace = NULL)
#'
#' # View output summary
#' print(testLines$summary)
#'
#' # Plot output
#' plot(hu["geometry"])
#' plot(testLines$lines['lineID'], add = T, col = 'red')
#' }
makeLines <- function(sPoly,
                      totalLengthKm,
                      angle = 0,
                      minLengthKm = 2.0,
                      offset = 0,
                      considerHuntArea = TRUE,
                      huntAreaBuffer = 1000,
                      breakLongLines = TRUE,
                      breakLongLineLength = 40,
                      minSpace = NULL,
                      maxSpace = NULL,
                      optimTol = 0.01,
                      optimQuick = FALSE) {

  # Functions below interpret angle = 90 to be North-South and 0 East-West
  # Convert so user inputs are more intuitive 0 = N-S, 90 = E-W
  angle <- (angle*-1)-90

  # Convert km to m
  targLenM <- totalLengthKm*1000
  minLength <- minLengthKm*1000

  # extract HUNTNAME for considerHuntArea
  sHUNTNAMES <- unique(sPoly$HUNTNAME)

  # union sPoly with itself
  sPoly <- st_union(sPoly)

  # create spatstat window using owinList
  polyWin <- as.owin(sPoly)

  # Estimate approx line spacing (using area)
  totSqKM <- as.numeric(st_area(sPoly)*1e-06)
  targKmPerSqKm <- totalLengthKm/totSqKM
  approxSpace <- exp(-1*log(targKmPerSqKm))*1000  # approximate line spacing (m)


  # Set spacing optimization bounds (if not supplied by user)
  if(is.null(minSpace)) {
    minSpace <- approxSpace*0.75
  }
  if(is.null(maxSpace)) {
    maxSpace <- approxSpace*1.1
  }

  optimSpace <- optimize(
    f = evalLines,
    lower = minSpace,
    upper = maxSpace,
    # tol = 0.1,
    angle = angle,
    win = polyWin,
    minLength = minLength,
    targLenM = targLenM,
    offset = offset
  )

  # If quick optim failed, try more intensive manual search
  if(optimSpace$objective > targLenM*optimTol && !(optimQuick)) {
    op <- options("warn")
    on.exit(options(op))
    options(warn=1)
    warning("Quick optimization failed, trying intensive search")
    optimSpace <- manualOptim(
      minSpace = minSpace,
      maxSpace = maxSpace,
      angle = angle,
      win = polyWin,
      minLength = minLength,
      targLenM = targLenM,
      offset = offset,
      optimTol = optimTol
    )
  }


  if(optimSpace$objective > targLenM*optimTol && !(optimQuick)) {
    stop(paste0("Optimization failed to generate lines with supplied parameters\n",
                "Consider setting minSpace and maxSpace parameters\n",
                "or increasing optimTol.",
                "The best spacing found was: ", round(optimSpace$minimum/1000), "km\n"))
  }

  # print(approxSpace / optimSpace$minimum)

  # Make lines using the optimization results
  keepLines <- genLines(
    angle = angle,
    spacing = optimSpace$minimum,
    win = polyWin,
    offset = offset,
    minLength = minLength)

  # Convert lines to sf object
  matLines <- as.matrix(keepLines)
  lines <- lapply(1:nrow(matLines), function(x) {
    st_linestring(matrix(matLines[x,],ncol=2, byrow = TRUE))
  })
  sfLines <- st_sfc(lines, crs = st_crs(sPoly))

  # Add attributes
  sfLines <- st_sf(sfLines)
  sfLines <- st_combine(sfLines)
  sfLines <- st_cast(sfLines, "LINESTRING")

  # split transects at hunt areas?
  if (considerHuntArea) {

    # get relevant hunt areas
    huntAreasIn <- huntAreas %>%
      filter(HUNTNAME %in% sHUNTNAMES)

    # If needed, project hunt area object to same crs as sPoly
    if (st_crs(huntAreas) != st_crs(sPoly)) {
      huntAreasIn <- st_transform(huntAreasIn, crs = st_crs(sPoly))
    }

    # create outline of herd unit & cast
    outline <- st_cast(st_union(huntAreasIn), "MULTILINESTRING")

    # buffer this outline (to avoid placing pts on hu border)
    outlineBuffer <- st_buffer(outline, set_units(50, "m"))

    # cast borders to multilinestring
    haBorders <- st_cast(huntAreasIn$geometry, "MULTILINESTRING")

    # remove outer borders
    haBorders <- st_difference(haBorders, outlineBuffer)
    haBorders <- st_cast(haBorders, "MULTILINESTRING")

    # get intersections as points
    intersectPoints <- st_intersection(sfLines, haBorders)
    intersectPoints <- st_combine(intersectPoints)

    # if isn't empty
    if (!st_is_empty(intersectPoints)) {
      # cast points
      intersectPoints <- st_cast(intersectPoints, "POINT")

      # add buffer to points
      intersectPoints <- st_buffer(intersectPoints, set_units(huntAreaBuffer, "m"))

      # add buffer to points
      intersectPoints <- st_buffer(intersectPoints, set_units(1000, "m"))

      # get intersections
      lineCutouts <- st_combine(st_intersection(sfLines, intersectPoints))

      # add buffer (weird sf error handling)
      lineCutouts <- st_buffer(lineCutouts, 50)

      # new lines
      sfLines <- st_difference(sfLines, lineCutouts)
      sfLines <- st_combine(sfLines)
      sfLines <- st_cast(sfLines, "LINESTRING")
    }
  }

  # split long transects?
  if (breakLongLines) {
    # add length to sfLines
    sfLines <- addLength(sfLines)

    # loop
    while (nrow(sfLines[lengthKm > breakLongLineLength]) > 0) {

      # if transect km > X, split at centers
      sfLinesKeep <- sfLines[lengthKm <= breakLongLineLength]
      sfLinesChange <- sfLines[lengthKm > breakLongLineLength]

      # get centroids to change
      changeCentroids <- st_centroid(sfLinesChange$x)

      # add buffer to centers
      changeCentroids <- st_buffer(changeCentroids, set_units(1000, "m"))

      # get intersections
      lineCutouts <- st_combine(st_intersection(sfLinesChange$x, changeCentroids))

      # add buffer (weird sf error handling)
      lineCutouts <- st_buffer(lineCutouts, 50)

      # get new lines
      sfLinesChange <- st_difference(sfLinesChange$x, lineCutouts)
      sfLinesChange <- st_combine(sfLinesChange)
      sfLinesChange <- st_cast(sfLinesChange, "LINESTRING")

      # create sf/data.table object, add length
      sfLinesChange <- addLength(sfLinesChange)

      # rbind
      sfLines <- rbind(sfLinesKeep, sfLinesChange)
    }
  }

  # add length to sfLines
  sfLines <- addLength(sfLines)

  # remove any less than minlengthKm
  sfLines <- sfLines[lengthKm >= minLengthKm]

  # get centroid of all lines (to see which ha they are in)
  lineCenters <- st_centroid(sfLines$x)

  # set agr to remove warning message
  huntAreasAGR <- huntAreas
  st_agr(huntAreasAGR) = "constant"

  # add info to centroids
  lineCenters <- st_intersection(huntAreasAGR, lineCenters)

  # add info to transects
  sfLines[, HuntName := lineCenters$HUNTNAME]
  sfLines[, HerdName := lineCenters$HERDNAME]
  sfLines[, HuntNo := lineCenters$HUNTAREA]
  sfLines[, HerdNo := lineCenters$HERDUNIT]

  # add prefix
  sfLines[, TransectPrefix := paste0(HerdName, "_", HuntName,"_")]
  sfLines[, TransectPrefix := gsub(" ", "", TransectPrefix)]
  sfLines[, TransectPrefix := gsub("-", "", TransectPrefix)]

  # sort from w-e, n-s
  sortCoords <- st_coordinates(st_centroid(sfLines$x))
  sfLines <- sfLines[order(sortCoords[,"X"], -sortCoords[,"Y"]),]

  # make ID based on sort & key
  sfLines[, ID := 1:.N, by = .(HerdName, HuntName)]
  setkey(sfLines, TransectPrefix, ID)

  # add line ID and remove prefix and ID
  sfLines[, LineID := paste0(TransectPrefix, ID)]
  sfLines[, LineShortID := paste0(HuntNo, "_", ID)]
  sfLines[, c("TransectPrefix", "ID") := NULL]

  # finally, transform to sf again
  sfLines <- st_as_sf(sfLines)

  # Make summary info
  outSumm <- list(
    nLines = nrow(sfLines),
    genTotalLengthKm = sum(sfLines$lengthKm),
    spacingKm = optimSpace$minimum/1000
    # approxRatio = optimSpace$minimum/approxSpace
  )


  # Return sf object and summary
  print(outSumm)
  return(list(lines = sfLines, summary = outSumm))
}

# Helper Functions--------------------------------------------------------------
# Define a function to generate lines (modified from spatstat.geom::rlinegrid())
genLines <- function (spacing,
                      win,
                      offset,
                      minLength,
                      angle = 0) {
  # TODO validate inputs
  win <- as.owin(win)
  width <- diff(win$xrange)
  height <- diff(win$yrange)
  rmax <- sqrt(width ^ 2 + height ^ 2) / 2 #length of diagonal divided by 2
  xmid <- mean(win$xrange)
  ymid <- mean(win$yrange)
  # u <- runif(1, min = 0, max = spacing) - rmax
  u <- offset - rmax
  if (u >= rmax)
    return(
      psp(
        numeric(0),
        numeric(0),
        numeric(0),
        numeric(0),
        window = win,
        check = FALSE
      )
    )
  p <- seq(from = u, to = rmax, by = spacing)
  q <- sqrt(rmax ^ 2 - p ^ 2)
  theta <- pi * ((angle - 90) / 180)
  co <- cos(theta)
  si <- sin(theta)
  X <- psp(
    x0 = xmid + p * co + q * si,
    y0 = ymid + p * si - q * co,
    x1 = xmid + p * co - q * si,
    y1 = ymid + p * si + q * co,
    window = owin(xmid + c(-1, 1) * rmax, ymid + c(-1, 1) * rmax),
    check = FALSE
  )
  X <- X[win]$ends
  xlengths <- eval(expression(sqrt((x1 - x0) ^ 2 + (y1 - y0) ^ 2)), envir = X)
  subX <- X[xlengths >= minLength, ]
  return(subX)
}

# Define a function to evaluate generated line length with respect to target length
evalLines <- function(spacing, angle, win, targLenM, offset, minLength){
  tlines <- genLines(
    angle = angle,
    spacing = spacing,
    win = win,
    offset = offset,
    minLength = minLength
  )
  tlengths <- eval(expression(sqrt((x1 - x0)^2 + (y1 - y0)^2)), envir = tlines)
  totLen <- sum(tlengths)
  err <- abs(targLenM-totLen)
  # print(paste0(spacing, ":", err))
  return(err)
}


# Manual optimization function to run if stats::optim fails.
# Identifies local minimums in spacing-error curve and iteratively searches
# each minima for acceptable spacing solution. Explores different offsets
# if necessary.
manualOptim <- function(minSpace, maxSpace,
                        angle, win, targLenM,
                        offset, minLength,
                        optimTol,
                        roughIter = 50,
                        fineIter = 150) {

  # Initialize output
  optimMan <- list(objective = Inf,
                   minimum = mean(c(minSpace, maxSpace)))

  # Run rough optimization
  tryOffs <- c(0, 500, 1000, 1500) #offsets to try if necessary
  minInd <- NA
  for(offInd in 1:length(tryOffs)) {
    if(offInd > 1) {
      warning("Failed to find solution with supplied offset, trying: ",
              "offset = ", tryOffs[offInd])
      offset <- tryOffs[offInd]
      fineIter = fineIter+100
    }
    while(any(c(is.na(minInd),
                minInd==1,
                minInd==roughIter))) {
      roughSpace <- seq(minSpace, maxSpace, length.out = roughIter)
      roughRes <- sapply(roughSpace, function(x) {
        evalLines(spacing = x,
                  angle = angle,
                  win = win,
                  targLenM = targLenM,
                  offset = offset,
                  minLength = minLength)})
      if(any(roughRes < targLenM*optimTol)){
        roughOptim <- list(minimum = roughSpace[which.min(roughRes)],
                           objective = min(roughRes))
        return(roughOptim) # return if satisfies criteria
      }
      # get rough local minima
      roughlocMins <- localmin(roughRes)$ind
      roughlocMins <- sort(unique(c(roughlocMins, which.min(roughRes))))
      minInd <- min(roughlocMins)
      if(minInd == 1) {
        minSpace <- minSpace*0.75
      }
      else if(minInd == length(roughSpace)) {
        maxSpace <- maxSpace*0.25
      }
    }
    # plot(roughSpace, roughRes)
    # points(roughSpace[roughlocMins], roughRes[roughlocMins],
    # pch = 16, col = 'red')

    # run fine optimization on each local minima
    fineOut <- do.call(rbind, lapply(roughlocMins, function(roughInd) {
      roughOptim <- roughSpace[roughInd]
      # roughMin <- max(roughOptim*0.2, roughSpace[max(c(1,(roughInd-2)))])
      # roughMax <- min(roughOptim*2, roughSpace[min(c(length(roughSpace),(roughInd+2)))])
      roughMin <- roughSpace[max(c(1,(roughInd-4)))]
      roughMax <- roughSpace[min(c(length(roughSpace),(roughInd+4)))]

      # Run fine optimization
      fineSpace <- seq(roughMin, roughMax, length.out = fineIter)
      fineRes <- sapply(fineSpace, function(y) {
        evalLines(spacing = y,
                  angle = angle,
                  win = win,
                  targLenM = targLenM,
                  offset = offset,
                  minLength = minLength)
      })
      # plot(fineSpace, fineRes)
      tempOptim <- list(minimum = fineSpace[which.min(fineRes)],
                        objective = min(fineRes))
      return(tempOptim)
    }))

    fineOut <- fineOut[which.min(fineOut[,2]),]
    fineOptim <- list(minimum = fineOut$minimum,
                      objective = fineOut$objective)
    if(fineOptim$objective < targLenM*optimTol) {
      optimMan <- fineOptim
      return(fineOptim) # return if satisfies criteria
    }
    # update best solution
    if(fineOptim$objective < optimMan$objective) {
      optimMan <- fineOptim
    }

  } # end offset loop
  return(optimMan)
}


# Local minima function (modified from R package erpR)
localmin <- function (x, n.points = 2) {
  vet = x
  dat = data.frame(index = 1:length(vet))
  test = function(i, vet, n.points) {
    indices = c((i - n.points):(i - 1), (i + 1):(i + n.points))
    indices = indices[indices > 0 & indices < (length(vet))]
    response = ((!any(vet[indices] <= vet[i])) & length(indices) ==
                  (n.points * 2))
    return(response)
  }
  candidates.indices = apply(dat, 1, function(k) {
    test(i = k, vet, n.points)
  })
  if (any(candidates.indices)) {
    candidates = vet[candidates.indices]
    out <- list(ind = which(candidates.indices),
                vals = candidates)
    # return(min(candidates))
    return(out)
  }
  else {
    return(NA)
  }
}

# helper function for adding length to sfLines
addLength <- function(dt) {
  dt <- st_as_sf(dt)
  dt <- as.data.table(dt)
  dt[, length := st_length(x)]
  dt[, lengthKm := set_units(length, "km")]
  dt[, lengthMi := set_units(length, "mi")]
  dt[, length := NULL]
  dt[, c("lengthKm", "lengthMi") := lapply(.SD, as.numeric), .SDcols = c("lengthKm", "lengthMi")]
}
