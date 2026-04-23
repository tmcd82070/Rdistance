#' @title Sage Thrasher detection data frame in Rdistance >4.0.0 format
#' 
#' @name thrasherDf
#' 
#' @description  
#' Point transect data collected in central Wyoming on 120 points
#' surveyed for Sage Thrashers by the Wyoming Cooperative Fish & Wildlife Research Unit in 2013.
#' 
#' @docType data
#'
#' @format A rowwise tibble containing 120 rows and 8 columns, one of which 
#' (i.e., 'detections') contains nested data frames of detections.  
#' Each row represents
#' one transect of one point.  
#' 
#' @format A data.frame containing 120 rows and 6 columns.  Each row represents
#' a surveyed site.  Each surveyed site is considered one transect of one point.  
#' Column descriptions: 
#' \enumerate{ 
#' \item `siteID`: Factor (120 levels), the site or point surveyed.  
#' \item `detections`: An embedded (nested) data frame containing 
#' detections made at that point.  Columns in the embedded data frame contain:
#'   \enumerate{
#'     \item `groupsize`: The number of individuals in
#'     the detected group.  
#'     \item `dist`: The radial distance (m) from
#'     the transect to the detected group.  
#'   }
#' \item `observer`: Factor (six levels), identity of the observer who surveyed
#' the point.  
#' \item `bare`: Number, the mean bare ground cover (%)
#' within 100 m of each point.  
#' \item `herb`: Number, the mean herbaceous
#' cover (%) within 100 m of each point.  
#' \item `shrub`: Number, the mean
#' shrub cover (%) within 100 m of each point.  
#' \item `height`: Number,
#' the mean shrub height \[cm\] within 100 m of each point.  
#' \item `npoints`: The number of point counts on the transect.
#' }
#'  
#' 
#' @seealso [thrasherSiteData()], [thrasherDetectionData()],
#' [RdistDf()]
#' 
#' @source The sage thrasher data are a subset of data collected 
#' by Jason Carlisle and various field technicians for his Ph.D. from the 
#' Department of Ecology, University of Wyoming, in 2017.  This portion of 
#' Jason's work was funded by the Wyoming Game and Fish Department 
#' through agreements 
#' with the University of Wyoming's  Cooperative Fish & Wildlife
#' Research Unit (2012). 
#' 
#' @examples
#' data(thrasherDf)
#' 
#' is.RdistDf(thrasherDf)
#' 
#' summary(thrasherDf,
#'   formula = dist ~ groupsize(groupsize)
#' )
#' 
#' @references Carlisle, J.D. 2017. The effect of sage-grouse conservation on
#' wildlife species of concern: implications for the umbrella species concept.
#' Dissertation. University of Wyoming, Laramie, Wyoming, USA.
#' 
#' Carlisle, J. D., A. D. Chalfoun, K. T. Smith, and J. L. Beck. 2018. 
#' Nontarget effects on songbirds from habitat manipulation for Greater 
#' Sage-Grouse: Implications for the umbrella species concept. 
#' *The Condor: Ornithological Applications* 120:439–455. 
#' \doi{10.1650/CONDOR-17-200.1}
#' 
NULL
