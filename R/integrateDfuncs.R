#' @title Integration of distance functions
#' 
#' @description
#' Integrates under distances functions using exact integrals 
#' when possible.  If exact integrals are not known, numerical 
#' integration is used. 
#' 
#' @details
#' Let K be the integral under distance function g(x) (i.e., the output
#' from this routine). In distance analysis, the observation likelihood 
#' being evaluated for mazimization 
#' is the \emph{density}, f(x) = g(x)/K.  K is a key quantity in distance 
#' analysis and is called the "effective sampling distance". 
#' 
#' @inheritParams integrateOneStepPoints 
#' 
#' @inheritParams startLimits 
#' 
#' @inheritSection integrateOneStepPoints Note
#'  
#' @inherit integrateOneStepPoints return
#' 
#' @examples
#' # Faking a model frame
#' ml <- list( likelihood = "halfnorm"
#'           , expansions = 0
#'           , w.lo = 0 %m% .
#'           , w.hi = 100 %m% .
#'           , Units = "m"
#'           )
#' attr(ml, "transType") <- "line"
#' 
#' parms <- matrix(75, nrow = 1) 
#' integrateDfuncs(parms, ml)
#' 
#' # check: Normal, 0 to 100, sd = 75, scaled to mode = 1
#' (pnorm(q = 100, mean = 0, sd = 75) - 0.5) * sqrt(2*pi)*75
#'
#' @export 
integrateDfuncs <- function( object
                           , ml ) {
  
  likExpan <- paste0(ml$likelihood, "_", ml$expansions, "_", transectType(ml))
  
  # The following IF cases were implemented to speed calculations 
  # dramatically when we know the integrals (i.e., avoid numerical 
  # integration when we can). 
  
  if( likExpan == "halfnorm_0_line" ){
    # CASE: Halfnormal, 0 expansions, Lines ----
    intType <- "Exact"
    
    outArea <- integrateHalfnormLines(object
                                      , w.lo = ml$w.lo
                                      , w.hi = ml$w.hi
                                      , Units = ml$outputUnits
    )
    
  } else if( likExpan == "halfnorm_0_point" ){
    # CASE: Halfnormal, 0 expansions, Points ----
    intType = "Exact"
    
    outArea <- integrateHalfnormPoints(object
                                       , w.lo = ml$w.lo
                                       , w.hi = ml$w.hi
                                       , Units = ml$outputUnits
    )
    
  } else if( likExpan == "negexp_0_line" ){
    # CASE: Negative exponential, 0 expansions, Lines ----
    intType = "Exact"
    
    outArea <- integrateNegexpLines(object
                                    , w.lo = ml$w.lo
                                    , w.hi = ml$w.hi
                                    , Units = ml$outputUnits
    )
    
  } else if( likExpan == "negexp_0_point" ){
    # CASE: Negative exponential, 0 expansions, Points ----
    intType = "Exact"
    
    outArea <- integrateNegexpPoints(object
                                     , w.lo = ml$w.lo
                                     , w.hi = ml$w.hi
                                     , Units = ml$outputUnits
    )
    
  } else if( likExpan == "hazrate_0_line" ){
    # CASE: Hazrate, 0 expansions, Lines ----
    intType = "Exact"
    
    outArea <- integrateHazrateLines(object
                                     , w.lo = ml$w.lo
                                     , w.hi = ml$w.hi
                                     , Units = ml$outputUnits
    )
    
  } else if( likExpan == "oneStep_0_line" ){
    # CASE: One step, 0 expansions, lines ----
    # Answer is:Theta <- object[,1];p <- object[,2];outArea <- Theta / p
    intType = "Exact"
    
    outArea <- integrateOneStepLines(object, Units = ml$outputUnits)
    
  } else if( likExpan == "oneStep_0_point"){
    # CASE: One step, 0 expansions, points ----
    intType = "Exact"
    
    outArea <- integrateOneStepPoints(object
                                      , w.lo = ml$w.lo
                                      , w.hi = ml$w.hi
                                      , Units=ml$outputUnits)
    
  } else if( grepl("oneStep", likExpan )){
    # CASE: oneStep (point or line) with expansions ----
    # Numeric integration by Trapazoid Rule
    # Must integrate from 0 to Theta, then Theta+ to w.hi
    # We know ml$expansions > 0 in this case
    # For expansion calculation we need expansion coefficients in 'object'
    # Do NOT exp() parameter b/c raw likelihood is called inside integrateOneStepNumeric
    # and the likelihood applies the link function
    intType = "Trapazoid"
    
    # coefLocs <- (length(ml$par)-(ml$expansions-1)):(length(ml$par))
    # object <- cbind(object
    #                , matrix(ml$par[coefLocs]
    #                         , nrow = nrow(object)
    #                         , ncol=ml$expansions
    #                         , byrow = TRUE
    #                ))  # n X ([#canonical] + nexp)
    object[,1] <- log(object[,1]) 
    
    outArea <- integrateOneStepNumeric(object
                                       , w.lo = ml$w.lo
                                       , w.hi = ml$w.hi
                                       , Units = ml$outputUnits
                                       , expansions = ml$expansions
                                       , series = ml$series
                                       , isPoints = is.points(ml))
    
  } else if( likExpan == "Gamma_0_line"){
    # CASE: Gamma, 0 expansions, lines ----
    intType = "Exact"
    
    outArea <- integrateGammaLines(object
                                   , w.lo = ml$w.lo
                                   , w.hi = ml$w.hi
                                   , Units=ml$outputUnits)
    
  # Trent could not figure out the integral of gamma points, when he does,
  # he should implement it in integrateGammaPoints and uncomment this code:
  # } else if( likExpan == "Gamma_0_point"){
  #   # CASE: Gamma, 0 expansions, lines ----
  #   intType = "Exact"
  # 
  #   outArea <- integrateGammaPoints(object
  #                                  , w.lo = ml$w.lo
  #                                  , w.hi = ml$w.hi
  #                                  , Units=ml$outputUnits)
    
  } else {
    # CASE: All other cases = Numeric integration by Simpson's Rule ----
    # do NOT exp parameters
    intType = "Simpson"
    
    # if( ml$expansions > 0 ){
    #   coefLocs <- (length(ml$par)-(ml$expansions-1)):(length(ml$par))
    #   object <- cbind(object
    #                  , matrix(ml$par[coefLocs]
    #                           , nrow = nrow(object)
    #                           , ncol=ml$expansions
    #                           , byrow = TRUE
    #                  ))  
    # }
    object[,1] <- log(object[,1]) 
    
    outArea <- integrateNumeric(object
                                , w.lo = ml$w.lo
                                , w.hi = ml$w.hi
                                , Units = ml$outputUnits
                                , expansions = ml$expansions
                                , series = ml$series
                                , isPoints = is.points(ml)
                                , likelihood = ml$likelihood
    )
    
  }

  # Done. Final prep of output ----  
  attr(outArea, "interalType") <- intType
  outArea
  
}