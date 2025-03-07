#' @title bcCI - Bias corrected bootstraps
#' 
#' @description
#' Calculate bias-corrected confidence intervals for bootstrap data
#' using methods in Manly textbook.
#' 
#' @param x.bs A vector of bootstrap estimates of some quantity. 
#' 
#' @param x A scaler of the original estimate of the quantity.
#' 
#' @param ci A scaler of the desired confidence interval 
#' coverage. 
#' 
#' @return A named vector containing the lower and upper endpoints of
#' the bias-corrected bootstrap confidence interval. 
#' 
#' @export
#' @importFrom stats qnorm pnorm quantile
#' 
bcCI <- function(x.bs, x, ci = 0.95){
  p <- mean(x.bs > x, na.rm = TRUE)
  z.0 <- stats::qnorm(1 - p)
  z.alpha <- stats::qnorm(1 - ((1 - ci)/2))
  p.L <- stats::pnorm(2 * z.0 - z.alpha)
  p.H <- stats::pnorm(2 * z.0 + z.alpha)
  ans <- stats::quantile(x.bs[!is.na(x.bs)], p = c(p.L, p.H))
  names(ans) <- paste0(100*c( (1 - ci)/2, 1 - (1-ci)/2 ), "%")
  ans
}
