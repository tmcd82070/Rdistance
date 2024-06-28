#' @title parseModel - Parse Rdistance model
#' 
#' @description 
#' Parse an 'Rdistance' formula and produce a list containing all model 
#' parameters.
#' 
#' @inheritParams dE.lt.single
#' 
#' @details
#' This routine is not intended to be called by the user.  It is called 
#' from the model estimation routines in \code{Rdistance}.
#' 
#' 
#' @return An Rdistance model frame, which is an object of class 
#' "dfunc". 
#' Rdistance model frames are lists containing distance 
#' model components but not estimates.  Model frames contain 
#' everything necessary to fit an Rdistance mode, such as covariates, 
#' minimum and maximum distances, the form of the likelihood, 
#' number of expansions, etc.  Rdistance model frames are a 
#' subset of an output Rdistance fitted model. 
#' 
#' @examples
#' 
#' sparrowDf <- Rdistance::RdistDf(sparrowSiteData
#'    , sparrowDetectionData
#'    , by = NULL
#'    , pointSurvey = FALSE
#'    , observer = "single"
#'    , .detectionCol = "detections")
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
#'    
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
  # unnest will throw an error.
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
    offsetVar <- withr::with_preserve_seed({
      set.seed(Sys.time())
      formatC(trunc(runif(1, max = 10000000))
              , format="f"
              , digits=0
              , width = 7
              , flag = "0")
      })
    offsetVar <- paste0("gs_",offsetVar)
    formulaChar[3] <- paste0(formulaChar[3], " + offset(", offsetVar, ")")
    detectionData <- detectionData |> 
      dplyr::mutate( !!offsetVar := 1 ) 
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
    , na.action = na.exclude
  )

  # A note on missing values  ----
  # Missing distances are okay.  They are observations of 
  # a target for which crew did not get a distance. Happens. 
  # These count toward "n" when computing density, but not 
  # when estimating distance functions.
  # Using na.exclude in model.frame excludes missing distances 
  # from the fitting frame, but attr(mf, "na.action") stores the 
  # line numbers of rows in the original data set with missing cases. 
  # A missing case has either missing distance or missing covariates. 
  
  # Check units ----    
  # We could check that offset, g.x.scl, and expanstions DON'T have units
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
  if( getOption("Rdistance_requireUnits") ){
    dataWUnits <- Rdistance::checkUnits(dataWUnits)
  }
  

  # Truncate for w.lo and w.hi ----
  # This is second evaluation of model.frame
  # Re-do model.frame so that distances outside strip are set to NA, but 
  # row is kept. Do this here, rather than above, because we've check all units here.
  # Use ml because checkUnits may have changed units, so original mf (and mt) may
  # not be good.
  d <- dplyr::pull(dataWUnits$data, dataWUnits$respName)
  ind <- (dataWUnits$w.lo <= d) & (d <= dataWUnits$w.hi)  # could be NA's here
  ind <- ind & !is.na(ind)
  if( any(!ind) ){
    missDist <- units::set_units(NA_real_, dataWUnits$outputUnits, mode="standard")
    dataWUnits$data[!ind, dataWUnits$respName] <- missDist
  }

  mf <- stats::model.frame(
      formula = formula
      , data = dataWUnits$data
      , drop.unused.levels = TRUE
      , na.action = na.exclude
    )    

  # Put everything in list ----    
  ml <- list(mf = mf
             # , mt = stats::terms(mf)
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
             , transType = attr(data, "transType")
             , obsType = attr(data, "obsType")
             # , control = control
  )
  
  # Enforce minimum number of spline basis functions ----
  if (ml$expansions < 2 & ml$series == "bspline"){
      ml$expansions <- 2
      if (warn) warning("Minimum spline expansions = 2. Proceeding with 2.")
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
  
  class(ml) <- "dfunc"

  ml
}