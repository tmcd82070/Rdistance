F.covars <- function(formla, dist, betas, covars){

# Function to return value of sigma based on covariates.
#
# Input:
# formla = formula object (i.e., ~ x1 + x2).
# dist = distances of detections
# betas = current values of coefficients
# covars = dataframe or matrix of covariates
#
# Output:
# sigma = estimates of sigma based on function of covariates

    
    # -------------  Intermediate function -------
    get.mod.matrix <- function(f, nobs){
    #   
    #   Function to return the model matrix from a formula object.  I.e., convert
    #   ~ x1 + x2 into a usable model matrix.
    #
    #   Input:
    #   f = an R formula object, without a response.  I.e., of the form "~ x1 + x2 + ..."
    #   nobs = number of detections
    #
    #   Output: 
    #   A matrix with variables in formula as columns.  Also included is 
    #   the names of variables in the formula and whether the model has an intercept.

        call <- match.call()
        contrasts <- NULL
        mf <- match.call(expand.dots = FALSE)
        mf$family <- mf$start <- mf$control <- mf$maxit <- NULL
        mf$model <- mf$method <- mf$x <- mf$y <- mf$contrasts <- NULL
        mf$... <- NULL
        mf$f <- mf$nobs <- mf$p.mat <- NULL
        
        mf$drop.unused.levels <- TRUE
        mf[[1]] <- as.name("model.frame")

        mf$formula <- f
        form <- as.character(formula(mf))[2]
 
        mf <- eval(mf, parent.frame())
        mt <- attr(mf, "terms")
        xvars <- as.character(attr(mt, "variables"))[-1]

        if((yvar <- attr(mt, "response")) > 0) xvars <- xvars[-yvar]
           
        xlev <- if(length(xvars) > 0) {
                xlev <- lapply(mf[xvars], levels)
                        xlev[!sapply(xlev, is.null)]
        }
        
        X <- NA
        X <- if( length(attr( mt, "order")) != 0 ) {
            model.matrix(mt, mf, contrasts)
        } else {
            stop("Empty models not allowed")
        }
    
        #   Find out whether intercept is present
        assign.col <- attr(X, "assign")
        if(sum(assign.col == 0 ) == 1){
            intercept <- TRUE
            nx <- sum(unique(assign.col) > 0)
        } else {
            intercept <- FALSE
            nx <- length(unique(assign.col))
        }   
            
        x.names <- xvars

        # Add 1 to param count for intercept, if appropriate
        if(intercept) nx <- nx + 1

        #  Make into a 3-D array if needed
        if(p.mat){
            if(intercept){
                X <- X[,-1]
                X <- cbind( matrix(1, nsites, nvisits), X)
            } 
            dim(X) <- c(nsites, nvisits, nx)
        }

        ans <- list( 
            X=X, 
            n.covars=nx, 
            intercept= intercept, 
            vars = x.names)
        ans
    }  
    
    # -------- Main code ------
    if(missing(betas)) stop("Model for sigma must be specified")
    if(missing(dist)) stop("Detection distances must be specified")

    P.FUN <- match.fun(P.FUN)

    orig.call <- match.call()

    #Total # detections
    nobs  <- length(dist)     

    mod.mat <- get.mod.matrix(formla, nobs)

    X.sigma <- mod.mat$X
    k.sigma <- mod.mat$n.covars   # includes intercept if present

    # evaluate model
    sigma.coefs = beta
    sigma = exp(X.sigma%*%sigma.coefs)          #sigma is nobs X 1

    sigma

}
