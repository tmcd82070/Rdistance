dfuncEstim <- function (formula
                        , data
                        , likelihood = "halfnorm"
                        , pointSurvey = FALSE
                        , w.lo = units::set_units(0,"m")
                        , w.hi = NULL
                        , expansions = 0
                        , series = "cosine"
                        , x.scl = units::set_units(0,"m")
                        , g.x.scl = 1
                        , observer = "single"
                        , warn = TRUE
                        , outputUnits = NULL
                        , control = RdistanceControls()){

  # To-do: remove control = RdistanceControls.  Put all options in
  # 'options(<name> = <value>)' in onAttach function.
  # rule: one site per line in data
  #       No point id col: all points on same transect bootstrapped.
  #       do not need transectID because we bootstrap lines. 
  #
  cl <- match.call()
  
  if ( likelihood == "uniform" ){
      .Deprecated(new = "logistic.like"
      , package = "Rdistance"
      , msg = paste("'unform.like' is depricated. Use 'logistic'.\n"
      , "Switching to 'logistic' likelihood.")
      , old = "uniform.like")
      likelihood <- "logistic"
  }
  
  if( pointSurvey ){
    # Point transects ----
    res <- switch( observer,
          "single" = dE.pt.single(formula = formula
                          , data = data
                          , likelihood = likelihood 
                          , w.lo = w.lo
                          , w.hi = w.hi
                          , expansions = expansions
                          , series = series
                          , x.scl = x.scl
                          , g.x.scl = g.x.scl
                          , warn = warn
                          , outputUnits = outputUnits
                          , control = control
      ), 
      "1|2" =,
      "2|1" =,
      "both" = dE.pt.multi(formula = formula
                          , data = data
                          , likelihood = likelihood 
                          , w.lo = w.lo
                          , w.hi = w.hi
                          , expansions = expansions
                          , series = series
                          , x.scl = x.scl
                          , g.x.scl = g.x.scl
                          , observer = observer
                          , warn = warn
                          , outputUnits = outputUnits
                          , control = control
                          )
      )
    } else {
      # Line transects ----
      res <- switch( observer,
          "single" = dE.lt.single(
                            formula = formula
                          , data = data
                          , likelihood = likelihood 
                          , w.lo = w.lo
                          , w.hi = w.hi
                          , expansions = expansions
                          , series = series
                          , x.scl = x.scl
                          , g.x.scl = g.x.scl
                          , warn = warn
                          , outputUnits = outputUnits
                          , control = control
      ) ,
      "1|2" =,
      "2|1" =,
      "both" =  dE.lt.multi(formula = formula
                          , data = data
                          , likelihood = likelihood 
                          , w.lo = w.lo
                          , w.hi = w.hi
                          , expansions = expansions
                          , series = series
                          , x.scl = x.scl
                          , g.x.scl = g.x.scl
                          , observer = observer
                          , warn = warn
                          , outputUnits = outputUnits
                          , control = control
                      )
      )
    }
  
  res 
}
