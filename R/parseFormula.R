parseModel <- function(formula
                          , likelihood = likelihood
                          , w.lo = w.lo
                          , w.hi = w.hi
                          , expansions = expansions
                          , series = series
                          , x.scl = x.scl
                          , g.x.scl = g.x.scl
                          , outputUnits = outputUnits
                          , control = control
                          ){

    # First convert 'groupsize' to 'offset' ----
    # Much easier to convert "groupsize" to "offset" in formula because
    # model.frame and others treat offset correctly.
    formulaChar <- as.character(formula) # [1] = "~"; [2] = LHS; [3] = RHS
    formulaChar[3] <- gsub( "groupsize\\(", "offset(", formulaChar[3] )
    formula <- formula( paste(formulaChar[c(2,1,3)], collapse = " ") )
    
    # Second, extract model frame, terms etc.
    mf <- getDfuncModelFrame(formula, data)
    mt <- terms(mf)
    if ( attr(mt, "response") == 0 ){
        stop("Formula must have a response on LHS of '~'.")
    }
    dist <- model.response(mf,"any")
    if ( !is.null(attr(mt, "offset")) ){
        # groupsize specified in formula
        groupSize <- stats::model.offset(mf)
    } else {
        groupSize <- rep(1, length(dist))
    }
    covars <- if (!is.empty.model(mt)){
        model.matrix(object = mt,
            data = mf,
            contrasts.arg = control$contrasts )
    }
    
    # Missing values in dist ----
    # Missing distances are okay.  They are observations of 
    # a target for which crew did not get a distance. Happens. 
    # These count toward "n" when computing density, but not 
    # when estimating distance functions.
    #  ml$dist = original distances potentially with NAs
    #  ml$distNoNA = distance without NAs, use for computations
    distNoNA <- na.omit(dist)

    
    # Put everything in list ----    
    ml <- list(mf = mf
        , mt = mt
        , dist = dist
        , distNoNA = distNoNA
        , groupsize = groupsize
        , covars = covars
        , contr = contr
        , assgn = assign
        , likelihood = likelihood
        , w.lo = w.lo
        , w.hi = w.hi
        , expansions = expansions
        , series = series
        , x.scl = x.scl
        , g.x.scl = g.x.scl
        , outputUnits = outputUnits
        , control = control
        )

    # Check units ----
    ml <- checkUnits(ml)
                              
    # Units on x.scl ----
    
    # Covariates ----
    #ncovars <- ncol(ml$covars)

    # Truncate for w.lo and w.hi
    ind <- (ml$w.lo <= ml$dist) & (ml$dist <= ml$w.hi)
    if( any(!ind) ){
        ml$dist <- ml$dist[ind]
        ml$groupSize <- ml$groupSize[ind]
        ml$covars <- ml$covars[ind,,drop = FALSE] 
    }
    
    attr(ml$covars,"assign") <- assgn
    attr(ml$covars,"contrasts") <- contr
            
    # Enforce minimum number of spline basis functions ----
    if (ml$expansions < 2 & ml$series == "bspline"){
        ml$expansions <- 2
        if (warn){
          warning("Minimum spline expansions = 2. Proceeding with 2.")
        }
    }

    # Override x.scl for Gamma likelihood ----
    if ( !is.character(ml$x.scl) ){
        if ( inherits(ml$x.scl, "units") ){ 
            # this case is needed cause drop units does 
            # not work on plain vector
            isZero <- units::drop_units(ml$x.scl) == 0
        } else {
            isZero <- ml$x.scl == 0
        }
        if ( isZero & ml$likelihood == "Gamma" ){
            ml$x.scl <- "max"
            warning("Cannot specify g(0) for Gamma likelihood.  x.scl changed to 'max'.")
        }
    }


    # Find factors. ----
    # This works when covars is NULL and must be called
    # even when ncovars == 1 to cover case like dist ~ -1+x (no intercept)
    #for (i in 1:ncol(mf)){
    #    if (is.factor(mf[,i])){
    #        factor.names <- c(factor.names, names(mf)[i])
    #    }
    #}
    
    strMf <- str(mf)
    factor.names <- strMf |>
      dplyr::filter( type == "factor" ) |>
      dplyr::pull(name)
    
    # vnames has original names, not expanded for factor levels
    vnames<-dimnames(covars)[[2]]
    
    ml
}