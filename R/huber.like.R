huber.like <- function(x, a, range){
  
  # Restrictions : 0 < a <= range <= w.hi
  
  h <- ifelse( abs(x) <= a
              , 0.5 * x^2
              , a*(abs(x) - 0.5*a)
  )
  h <- a*(abs(range) - 0.5*a) - h
  h <- ifelse( h <= 0.0
              , 0.0
              , h
              )
  
  # Integrate under the function and scale
  integral0a <- a^2*range - (2/3)*a^3
  integralar <-  0.5*a*(range^2 + a^2 - 2*range*a)
  areaUnder <- integral0a + integralar
  
  h <- h / areaUnder
  
  h
  
}
