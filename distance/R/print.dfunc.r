print.dfunc <- function( obj, digits = max(3, getOption("digits") - 3), ... ){
#
#   Print a distance function
#

    cat("Call: ", deparse(obj$call), "\n\n", sep = "")
    if (length(coef(obj))) {
        cat("Coefficients:\n")
        print.default(format(coef(obj), digits = digits), print.gap = 2,
            quote = FALSE)
    }
    else cat("No coefficients\n")
    
    cat("\n")
    
    if( obj$convergence == 0 ){
        mess <- "Success"
    } else {
        mess <- "FAILURE"
    }
    cat(paste("Convergence: ", mess, " (Exit code=", obj$convergence, ", ", obj$fit$message, ")", "\n", sep="")) 
    
    
    if( obj$expansions==0 ){
        mess <- ""
    } else {
        mess <- paste( "with", obj$expansions, "expansion(s) of", casefold( obj$series, upper=TRUE ), "series")
    }
    cat(paste("Function:", casefold(obj$like.form, upper=TRUE), mess, "\n") )
    
    cat(paste("Strip:", obj$w.lo, "to", obj$w.hi, "\n"))
    cat(paste("Effective strip width:", ESW(obj), "\n"))
    cat(paste("Scaling: g(", obj$x.scl, ") = ", obj$g.x.scl, "\n", sep=""))
    cat(paste("Log likelihood:", round(obj$loglik, digits), "\n")) 
    cat(paste("AIC:", round(AIC(obj), digits), "\n"))
    
    
    cat("\n")
    invisible(obj)
}
