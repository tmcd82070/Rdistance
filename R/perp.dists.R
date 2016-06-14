perp.dists <- function(s.dist, s.angle, data){
  
  # Stop and print error if dist or angle is NA
  if(any(is.na(data[, s.dist])))
    stop("Please remove detections for which s.dist and/or s.angle is NA.")
  if(any(is.na(data[, s.angle])))
    stop("Please remove detections for which s.dist and/or s.angle is NA.")
  
  # Convert degrees to radians, then apply sine function and round off decimal places
  data[, s.dist] * sin(data[, s.angle]*(pi/180))
    
}