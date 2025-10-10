#' @title Parse Rdistance model
#' 
#' @description 
#' Parse an 'Rdistance' formula and produce a list containing all model 
#' parameters.
#' This routine is not normally called directly by the user, but 
#' it might be helpful in simulations.  It is called 
#' internally from the model estimation routines.
#' 
#' @inheritParams dfuncEstim
#' @inheritParams dE.single
#' 
#' 
#' @return An Rdistance model frame, which is an object of class 
#' "dfunc". 
#' Rdistance model frames are lists containing distance 
#' model components but not estimates.  Model frames contain 
#' everything necessary to fit an Rdistance mode, such as covariates, 
#' minimum and maximum distances, the form of the likelihood, 
#' number of expansions, etc.  Rdistance model frames contain a 
#' subset of fitted Rdistance model components. 
#' 
#' @seealso \code{\link{RdistDf}}, which returns an 
#' Rdistance \emph{data} frame;
#' \code{\link{dfuncEstim}}, which returns an 
#' Rdistance \emph{fitted} model.
#' 
#' @examples
#' 
#' data(sparrowDf)
#'    
#' ml <- Rdistance::parseModel(sparrowDf
#'    , formula = dist ~ 1 + observer + groupsize(groupsize)
#'    , likelihood = "halfnorm"
#'    , w.lo = 0
#'    , w.hi = NULL
#'    , series = "cosine"
#'    , x.scl = 0
#'    , g.x.scl = 1
#'    , outputUnits = "m"
#'    )
#' class(ml)  # 'dfunc', but no estimated coefficients
#' print(ml)
#' print.default(ml)
#' 
#' @export
parseModel <- function(data
                          , formula = NULL
                          , likelihood = "halfnorm"
                          , w.lo = 0
                          , w.hi = NULL
                          , expansions = 0
                          , series = "cosine"
                          , x.scl = 0
                          , g.x.scl = 1
                          , outputUnits = NULL
                          ){

  # Check validity of data set ----
  if( !Rdistance::is.RdistDf(data, verbose = TRUE) ){
    stop(paste(deparse(substitute(data)), "is not an RdistDf. See help('RdistDf')"))
  }

  # Control parameters ----
  # if you want, could save control options in output object.
  # control <- options()[grep("Rdist_", names(options()))]
  
  # checkNEvalPts computes and sets Simpson coefficients in options()
  checkNEvalPts(getOption("Rdistance_intEvalPts")) # not exported
  
  # Check that we know the likelihood ----
  if( !( likelihood %in% getOption("Rdistance_knownLikelihoods")) ){
    stop(paste("Unknown likelihood. Likelihood must be one of"
             , paste(getOption("Rdistance_knownLikelihoods"), collapse = ", ")
             , "(case sensitive)."))
  }
  
  # Check for a response ----
  # Otherwise, as.character(formula) is length 2, not 3
  if( is.null(formula) ){
    stop("'formula' is required.")
  }
  if( !inherits(formula, "formula") ){
    stop("'formula' must be a formula object.")
  }
  mt <- stats::terms.formula(formula)
  if ( attr(mt, "response") == 0 ){
    stop("Formula must have a response on LHS of '~'.")
  }

  # Unnest the data frame ----
  # If a name in list column is the same as another in parent frame, 
  # unnest will throw an error; but, this is checked in is.RdistDf.
  detectionData <- tidyr::unnest(data
                                 , cols = attr(data, "detectionColumn"))
  
  # Fix up formula ----
  # I.e., add offset() if not present; change "groupsize" to "offset" if present.
  # Use "offset" because model.offset works with it. 
  # This may change detectionData by adding an offset with random name
  #
  # Much easier to convert "groupsize" to "offset" in formula because
  # model.frame and others treat offset correctly.
  formulaAtCall <- formula # save the original
  formulaChar <- as.character(formula) # [1] = "~"; [2] = LHS; [3] = RHS
  gsFormulaTxt <- "groupsize\\(" # string specifying groupsize in formula
  if( grepl( gsFormulaTxt, formulaChar[3]) ){
    # "groupsize(...)" specified
    formulaChar[3] <- gsub( gsFormulaTxt, "offset(", formulaChar[3] )
  } else {
    # "groupsize" not specified; add it.
    effColumn <- attr(data, "effortColumn")
    offsetVar <- withr::with_preserve_seed({
      set.seed(Sys.time())
      formatC(trunc(stats::runif(1, max = 10000000))
              , format="f"
              , digits=0
              , width = 7
              , flag = "0")
      })
    offsetVar <- paste0("gs_",offsetVar)
    formulaChar[3] <- paste0(formulaChar[3], " + offset(", offsetVar, ")")
    detectionData[[offsetVar]] <- ifelse(is.na(detectionData[[effColumn]]), NA, 1)
  }
  formula <- formula( paste(formulaChar[c(2,1,3)], collapse = " ") )


  # Evaluate model frame ----
  # Will evaluate model frame twice: here, to get response and offset from formula
  # so that we can check units. Later, to exclude observations outside the strip.
  #
  # Use na.exclude here so that predict includes NA for any missing cases 
  # later.  I.e., predict returns vector same size as detectionData always
  # Test whether NA cases were present with is.null(attr())
  mf <- stats::model.frame(
    formula = formula
    , data = detectionData
    , drop.unused.levels = TRUE
    , na.action = stats::na.pass
  )

  # A note on missing values  ----
  # Missing distances are okay.  They are observations of 
  # a target for which crew did not get a distance. Happens. 
  # These count toward "n" when computing density, but not 
  # when estimating distance functions.
  # Using na.pass preserves all missings in the model frame, 
  # this includes missing group sizes (for obs on missing transects). 
  # All of Rdistance must keep in mind that missing responses (distances)
  # could happen.

  # Check units ----    
  # We could check that offset, g.x.scl, and expansions DON'T have units
  mtNames <- as.character(attr(mt, "variables"))
  respName <- mtNames[ attr(mt, "response") + 1 ]
  offsetName <- mtNames[ attr(mt, "offset") + 1 ]
  dataWUnits <- list(data = detectionData
                     , w.lo = w.lo
                     , w.hi = w.hi
                     , respName = respName
                     , offsetName = offsetName
                     , x.scl = x.scl
                     , outputUnits = outputUnits
                     , dataName = deparse(substitute(data))
  )
  
  # CHANGE CHECKUNITS TO ACCEPT ML OBJECTS i.e., accept "RdistMl" object
  # so user can call directly more easier.
  if( getOption("Rdistance_requireUnits") ){
    dataWUnits <- Rdistance::checkUnits(dataWUnits)
  }
  

  # Truncate for w.lo and w.hi ----
  # This physically removes observations outside the strip. 
  # But, it keeps missing values of distances because their group sizes 
  # may count toward abundance. 
  # Use dataWUnits because checkUnits may have changed units, so original mf (and mt) may
  # not be good.
  d <- dplyr::pull(dataWUnits$data, dataWUnits$respName)  
  ind <- (dataWUnits$w.lo <= d) & (d <= dataWUnits$w.hi)  # could be NA's here from missing distances
  # ind <- ind & !is.na(ind)
  if( any(!ind, na.rm = TRUE) ){
    dataWUnits$data <- dataWUnits$data[is.na(ind) | ind, ]  
  }

  mf <- stats::model.frame(
      formula = formula
      , data = dataWUnits$data
      , drop.unused.levels = TRUE
      , na.action = stats::na.pass
    )    

  # Store a reduced data frame for abundance estimation later ----
  allVars <- c(all.vars( stats::terms(mf) ) # for covariates
               , attr(data, "detectionColumn")
               , attr(data, "effortCol")
               , dplyr::group_vars(data))
  modelData <- data |> 
    dplyr::select(dplyr::any_of(allVars))

  # Put everything in list ----    
  ml <- list(mf = mf
             , data = modelData
             , formula = formulaAtCall
             , dataName = dataWUnits$dataName
             , likelihood = likelihood
             , w.lo = dataWUnits$w.lo
             , w.hi = dataWUnits$w.hi
             , expansions = expansions
             , series = series
             , x.scl = dataWUnits$x.scl
             , g.x.scl = g.x.scl
             , outputUnits = dataWUnits$outputUnits
  )
  
  # Enforce minimum number of spline basis functions ----
  if (ml$expansions < 2 & ml$series == "bspline"){
      ml$expansions <- 2
      warning("Minimum spline expansions = 2. Proceeding with 2.")
  }

  # Check x.scl, and override x.scl for Gamma likelihood ----
  if ( length(ml$x.scl) > 1 ){
    # at this point in the process, x.scl can only be a scaler or "max"
    warning(paste0("x.scl cannot have length >1. "
                 , "Found length "
                 , length(ml$x.scl)
                 , ". Only first element will used. "
                 , " x.scl has been set to "
                 , ml$x.scl[1]))
    ml$x.scl <- ml$x.scl[1]
  }
  
  if ( !is.character(ml$x.scl) ){
    isZero <- units::set_units(ml$x.scl, NULL) == 0
    if ( isZero & ml$likelihood == "Gamma" ){
        ml$x.scl <- "max"
        warning("Cannot specify g(0) for Gamma likelihood.  x.scl changed to 'max'.")
    }

    # Check that x.scl >= w.lo ----
    if ( ml$x.scl < ml$w.lo ){
      ml$x.scl <- ml$w.lo
      warning(paste0("x.scl must be >= w.lo. x.scl set to "
                   , format(ml$x.scl)
                   , " i.e., g("
                   , format(ml$x.scl)
                   , ") = "
                   , format(ml$g.x.scl)
                   , " in the model."
                  ))
    }
  } else if( ml$x.scl != "max"){
    stop(paste0("x.scl must either be a number (with units) or string 'max'. Found "
                , ml$x.scl))
  }
  
  class(ml) <- "dfunc"

  ml
}
