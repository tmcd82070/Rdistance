dE.pt.multi <- function(){
  
  
  # The double-observer method hasn't been checked since updating to version 2.x
  # It's likely that an error would be raised if a user did try the double-observer option,
  # but I'm adding a warning here, just in case
    stop("The double-observer routines have not been tested in Rdistance
          versions >2.x, so they have been disabled for the time being.
          Contact the Rdistance authors if you need double observer analyses
          and can help.")

}