#' @title getModelMatrix
#' 
#' @description Returns the  model frame from a formula and 
#' data set. This routine is intended to only be called from within other Rdistance
#' functions. 
#' 
#' @param formula A dfunc formula object.  See \code{dfuncEstim}.
#' 
#' @param data The data frame from which variables in formula (potentially) come.
#' 
#' @return a model frame containing the response and covariates resulting from 
#' evaluating formula in data.
#' 
#' @details This routine is needed to get the scoping correct in \code{dfuncEstim}. 
#' In \code{dfuncEstim}, we first merge the detection and site data frames, then 
#' call this routine.  Trying to do the model extraction directly in \code{dfuncEstim}
#' did not work because of some vageries of scoping the Trent does not understand. 
#' 
#' @author Trent McDonald, WEST, Inc. \email{tmcdonald@west-inc.com}
#' 
#' 
getDfuncModelFrame <- function(formula, data){

  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data"), names(mf), 0L)
  mf <- mf[c(1L, m)]
  names(mf)[names(mf)=="formula"] <- "formula"
  mf$drop.unused.levels <- TRUE
  mf[[1L]] <- quote(stats::model.frame)
  mf <- eval(mf, parent.frame())
  
  mf
}
