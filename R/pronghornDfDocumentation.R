#' @title Pronghorn Line Transect Data
#' 
#' @name pronghornDf 
#' 
#' @description 
#' Pronghorn (*Antilocapra americana*) aerial line transect data collected 
#' by the Wyoming Game and Fish Department in southeast Wyoming, 2012 - 2019.
#' Data contain transect and pronghorn group detection information 
#' collected during five aerial surveys of four herd units during four years. 
#' The general survey protocol is described in Johnson et al. (1991) and 
#' Smyser et al. (2016), modified for these surveys to use 5 distance bins
#' (specified in Details below) and one observer surveying only one side of
#' the aircraft at a time.
#'  
#' @docType data
#' 
#' @format A rowwise tibble containing 300 rows and 6 columns.  Each row 
#' represents one transect.  Column `detections` contains an embedded data frame
#' of detections made on the transect of that row. 
#' 
#' **Transects**:
#' Transect columns are as follows: 
#' \itemize{ 
#'   \item `transectID`: (character) Unique transect identifier. This is 
#'   a concatenation of `herd`, `year`, and `lineLabel`. 
#'   \item `herd`: (character) Abbreviation of the surveyed herd unit. Herds are
#'    considered distinct populations.  
#'   \item `year`: (character) Year of the survey.
#'   \item `lineLabel`: (character) Unique identifier of the transect line 
#'   within herd and year.
#'   \item `lineLength`: (numeric, kilometers) Length of the transect. 
#'   \item `detections`: (nested dataframe) Detections made on the transect
#'   (next section).  NA if no detections were made on the transect.
#' }
#'  
#' **Detections**:
#' The embedded data frame in column `detections` contains the following
#' variables:
#' \itemize{
#'   \item `nominalDistBandLabel`: (character) The nominal distance bin identifier 
#'   assigned by observers in the aircraft at the time of detection based on sighting 
#'   a group between markers on the aircraft's wing strut.
#'   \item `nominalDistBand`: (factor) The nominal distance bin endpoints assuming
#'   an aircraft flight height of 300 \[ft\] above ground level (AGL) at the time of detection.
#'   \item `flightHeight`: (numeric, feet) Height of the aircraft AGL at the time of detection.
#'   \item `clusterSize`: (integer) Number of pronghorn in the detected group.
#'   \item `adjustedMidpoint`: (numeric, meters) The distance bin's midpoint 
#'   after adjustment for AGL at the time of detection.
#'   \item `adjustedBandLower`: (numeric, meters) The lower endpoint of the 
#'   distance bin after adjustment for AGL at the time of detection.
#'   \item `adjustedBandUpper`: (numeric, meters) The upper endpoint of the 
#'   distance bin after adjustment for AGL at the time of detection.
#' }
#' 
#' @details
#' 
#' **Study Areas**:
#' Researchers placed transects systematically over polygons outlining habitable
#' areas within each herd unit's boundaries.
#' Habitable areas within each herd unit had the following sizes:
#' 
#' | Herd    |	Area        |
#' | :------:| ------------:|
#' |---------|--------------|
#' | BA      | 1004 \[mi^2] |
#' | CE      | 1420 \[mi^2] |
#' | CO      |  444 \[mi^2] |
#' | MB      | 3000 \[mi^2] |
#' 
#' **Nominal Distance Bins**:
#' Researchers established the following minimum and maximum distances for each
#' off-transect distance bin after multiple flights calibrating wing strut
#' markers against known-width ground objects (e.g., highways). These lower and
#' upper bin endpoints correspond to a flight altitude of 300 \[ft\] AGL. 
#' 
#' | Bin  | Minimum | Maximum  |
#' | :---:| -------:| --------:|
#' |---|-----------|-----------|
#' | A |  65 \[m\] |  85 \[m\] |
#' | B |  85 \[m\] | 110 \[m\] |
#' | C | 110 \[m\] | 145 \[m\] |
#' | D | 145 \[m\] | 210 \[m\] |
#' | E | 210 \[m\] | 265 \[m\] |
#' 
#' 
#' @source These data come from routine pronghorn population monitoring 
#' conducted by the Wyoming Game and Fish Department.  Included with permission.
#' Contact: Dr. Jason Carlisle (Rdistance author).  
#' 
#' @examples
#' transectInfo <- pronghornDf |> 
#'   dplyr::reframe( herd = herd[1]
#'                 , year = year[1]
#'                 , lineLabel = lineLabel[1]
#'                 , length = lineLength[1]
#'                 , groups = ifelse(is.null(detections), 0, nrow(detections))
#'                 , individuals = ifelse(is.null(detections), 0, sum(detections$clusterSize))
#'                 )
#' surveyInfo <- transectInfo |> 
#'   dplyr::group_by(herd, year) |> 
#'   dplyr::summarise(transects = dplyr::n()
#'                  , transectTotLength = sum(length)
#'                  , totGroups = sum(groups)
#'                  , totIndividuals = sum(individuals)
#'                  , zeroTransects = sum(groups == 0)
#'   )
#' 
#' @references 
#' Johnson, B. K., F. G. Lindzey, and R. J. Guenzel. 1991
#' Use of Aerial Line Transect Surveys to Estimate Pronghorn Populations in
#' Wyoming. *Wildlife Society Bulletin* 19(3):315–321. 
#' http://www.jstor.org/stable/3782522.
#' 
#' Smyser, T. J., R. J. Guenzel, C. N. Jacques, and E. O. Garton. 2016. 
#' Double-observer evaluation of pronghorn aerial line-transect surveys.
#' *Wildlife Research* 43(6):474–481.
#' 
NULL
