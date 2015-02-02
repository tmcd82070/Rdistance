perp.dists <- function(obs.dist, obs.angle, digits){
    
    # For line transect surveys where the distance from the observer (obs.dist) and angle (in degrees) from
    # the line (obs.angle) is recorded.
    # Converts degrees to radians, then applies the sine function and rounds off decimal places
    # Reference Buckland et al Intro to Dist Samp book
    
    
    # Need to build in some error handling (e.g., NA values)
    
    
    round(obs.dist * sin(obs.angle*(pi/180)), digits)
    
}