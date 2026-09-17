#' @title Calculate target transect length
#'
#' @description Calculate the total length of transects to survey to reach a user-provided
#' target number of detected groups.  All else being equal, areas
#' with higher group densities will require less survey effort than 
#' those with higher densities. 
#'
#' @inheritParams makeLines
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
#' @param targetGroups Scalar, the desired number of detecterd groups 
#' at the end of surveys.  
#' 
#' @param avgGroupSize Scalar, expected average number of individuals in each
#' group. 
#' 
#' @author Jason Carlisle, based on some clever back-of-the-napkin math by Trent
#' McDonald and Greg Hiatt at a WGFD training on pronghorn LT surveys in Laramie
#' in April 2022.
#'
#' @return Scalar, the total length of transects to survey. Units of measurement
#' attached to the return are the square root of the polygon's area.  
#' E.g., if `sPoly` is projected to a
#' standard UTM plane, their area is m^2, and transect length units will 
#' be m. 
#' 
#' @export
#'
#' @examples
#'
#' data(exampleSurveyPoly)
#' calcLineLength(sPoly = exampleSurveyPoly[1, ]
#'                , N = 12000
#'                , p = 0.5
#'                , w = units::set_units(200, "m")
#'                , targetGroups = 300
#'                , avgGroupSize = 2.5
#'                )
#'
calcLineLength <- function(sPoly,
                           N,
                           p,
                           w,
                           targetGroups,
                           avgGroupSize) {

  makeLinesRequireLength(w,"w")
  
  # Union in case of multiple features
  sPoly <- sf::st_union(sPoly)

  # Calculate area
  area <- sf::st_area(sPoly)

  # Calculate length of transects in m
  l.m <- (targetGroups*area*avgGroupSize)/(N*p*w)

  return(l.m)

}
