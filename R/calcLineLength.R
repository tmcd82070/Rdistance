#' @title Calculate target transect length
#'
#' @description Calculate the total length of transects to survey to reach a user-provided
#' target number of detected groups.  All else being equal, areas
#' with higher group densities will require less survey effort than 
#' those with higher densities. 
#'
#' @param saPolygon sf object, spatial polygon of the study area
#' `N`, `avgGroupSize`, and the area of this polygon are used to
#' calculate expected animal density and then expected group density.
#' 
#' @param N Scalar, expected abundance of individuals in the study area.  Estimates
#' from past surveys are likely your best option here.  Used to calculate the
#' expected group density in the survey area.
#' 
#' @param p Scalar, expected probability of detecting a group.  If expected 
#' strip width (ESW) is available, set `p` = `ESW/w`. 
#' 
#' @param w Scalar, nominal width of the survey strip.  Must have measurement 
#' units attached.  
#' 
#' @param targetGroups Scalar, number of desired groups at the end of surveys.  
#' 
#' @param avgGroupSize Scalar, expected average number of individuals in each
#' group. 
#' 
#' @author Jason Carlisle, based on some clever back-of-the-napkin math by Trent
#' McDonald and Greg Hiatt at a WGFD training on pronghorn LT surveys in Laramie
#' in April 2022.
#'
#' @return Scalar, the total length of transects to survey. Measurement units 
#' are the square root of the polygon's area.  E.g., if polygons are in a
#' standard UTM projection, their area is m^2, and transect length units will 
#' be m. 
#' 
#' @export
#'
#'
#' @examples
#' \dontrun{
#' # Read in a sf polygon (here for Rattlesnake herd unit less unoccupied area)
#' occupiedPolygon <- st_read("U:/My Drive/PronghornLT/Rattlesnake/GIS",
#'                                "Rattlesnake_HU")
#'
#' calcLineLength(occupiedPolygon = occupiedPolygon,
#'                N = 12000)
#' }
calcLineLength <- function(occupiedPolygon,
                           N,
                           p = 0.58,
                           w = units::set_units(200, "m"),
                           targetGroups = 300,
                           avgGroupSize = 2.3) {

  # Union in case of multiple features
  occupiedPolygon <- st_union(occupiedPolygon)

  # Calculate area
  area <- st_area(occupiedPolygon)

  # Calculate length of transects in m
  l.m <- (targetGroups*area*avgGroupSize)/(N*p*w)

  # Convert to km
  # l.km <- l.m/1e3

  return(as.numeric(l.m))

}
