#' @title dfuncModelFrame - Return 'Rdistance' model frame
#' 
#' @description Returns the model frame from a formula and 
#' data set.  
#' 
#' @param formula A dfunc formula object.  See \code{dfuncEstim}.
#' 
#' @param data The data frame from which variables in formula (potentially) come.
#' 
#' @return a model frame containing the response and covariates resulting from 
#' evaluating formula in data.
#' 
#' @details
#' This routine is not intended to be called by users.  It is called 
#' from within \code{Rdistance} estimation functions.
#' 
#' This routine is required for proper scoping in \code{dfuncEstim}. 
#' 
#' @export
#' 
dfuncModelFrame <- function(data, formula) {

  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data"), names(mf), 0L)
  mf <- mf[c(1L, m)]
  names(mf)[names(mf)=="formula"] <- "formula"
  mf$drop.unused.levels <- TRUE
  mf[[1L]] <- quote(stats::model.frame)
  mf <- eval(mf, parent.frame())
  
  mf
}
