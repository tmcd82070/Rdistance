# halfnorm-MinimumInputs

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = dataDf, formula = dist ~ groupsize(groupsize),
         likelihood = lhood)
      Coefficients:
                   Estimate  SE       z      p(>|z|)
      (Intercept)  3.959     0.03768  105.1  0      
      
      Message: Success; Asymptotic SE's
      Function: HALFNORM  
      Strip: 0 [m] to 207 [m] 
      Effective strip width (ESW): 65.7 [m] 
      Probability of detection: 0.3174
      Scaling: g(0 [m]) = 1
      Log likelihood: -1668 
      AICc: 3337
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.051 
         Group size range: 1 to 3 
      Density in sampled area: 7.907e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 324578

# halfnorm-NoCovar

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = dataDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, w.lo = w.lo, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = 0, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                   Estimate  SE       z     p(>|z|)
      (Intercept)  3.909     0.04039  96.8  0      
      
      Message: Success; Asymptotic SE's
      Function: HALFNORM  
      Strip: 0 [m] to 150 [m] 
      Effective strip width (ESW): 62.34 [m] 
      Probability of detection: 0.4156
      Scaling: g(0 [m]) = 1
      Log likelihood: -1631 
      AICc: 3263
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 371 in 353 groups 
       Average group size: 1.051 
         Group size range: 1 to 3 
      Density in sampled area: 8.265e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 339286

# halfnorm-ContinuousCovar

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = dataDf, formula = dist ~ bare +
         groupsize(groupsize), likelihood = lhood, w.lo = w.lo, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = 0, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                   Estimate  SE       z       p(>|z|)   
      (Intercept)  3.346184  0.15524  21.555  4.781e-103
      bare         0.009716  0.00272   3.572   3.545e-04
      
      Message: Success; Asymptotic SE's
      Function: HALFNORM  
      Strip: 0 [m] to 150 [m] 
      Average effective strip width (ESW): 62 [m] (range 47.95 [m] to 76.98 [m]) 
      Average probability of detection: 0.4133 (range 0.3197 to 0.5132)
      Scaling: g(0 [m]) = 1
      Log likelihood: -1624 
      AICc: 3253
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 371 in 353 groups 
       Average group size: 1.051 
         Group size range: 1 to 3 
      Density in sampled area: 8.471e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 347722

# halfnorm-FactorCovar

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = dataDf, formula = dist ~ observer +
         groupsize(groupsize), likelihood = lhood, w.lo = w.lo, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = 0, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                    Estimate  SE       z        p(>|z|)
      (Intercept)    3.92801  0.08793  44.6696  0.00000
      observerobs2   0.17073  0.14605   1.1690  0.24239
      observerobs3  -0.01647  0.11924  -0.1381  0.89012
      observerobs4  -0.26740  0.12898  -2.0732  0.03816
      observerobs5  -0.02839  0.12345  -0.2300  0.81810
      
      Message: Success; Asymptotic SE's
      Function: HALFNORM  
      Strip: 0 [m] to 150 [m] 
      Average effective strip width (ESW): 62.19 [m] (range 48.73 [m] to 74.56 [m]) 
      Average probability of detection: 0.4146 (range 0.3249 to 0.4971)
      Scaling: g(0 [m]) = 1
      Log likelihood: -1626 
      AICc: 3262
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 371 in 353 groups 
       Average group size: 1.051 
         Group size range: 1 to 3 
      Density in sampled area: 8.388e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 344317

# halfnorm-NoCovarWloWhi

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = dataDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, w.lo = w.20, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = w.20, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                   Estimate  SE       z      p(>|z|)
      (Intercept)  3.784     0.05013  75.47  0      
      
      Message: Success; Asymptotic SE's
      Function: HALFNORM  
      Strip: 20 [m] to 150 [m] 
      Effective strip width (ESW): 54.94 [m] 
      Probability of detection: 0.4226
      Scaling: g(20 [m]) = 1
      Log likelihood: -1046 
      AICc: 2095
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 241 in 233 groups 
       Average group size: 1.034 
         Group size range: 1 to 2 
      Density in sampled area: 6.093e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 250097

# halfnorm-NoCovarWlo

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = dataDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, w.lo = w.20, expansions =
         0, series = "cosine", x.scl = w.20, g.x.scl = 1, outputUnits = "m")
      Coefficients:
                   Estimate  SE      z      p(>|z|)
      (Intercept)  3.865     0.0463  83.47  0      
      
      Message: Success; Asymptotic SE's
      Function: HALFNORM  
      Strip: 20 [m] to 207 [m] 
      Effective strip width (ESW): 59.76 [m] 
      Probability of detection: 0.3196
      Scaling: g(20 [m]) = 1
      Log likelihood: -1083 
      AICc: 2168
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 244 in 236 groups 
       Average group size: 1.034 
         Group size range: 1 to 2 
      Density in sampled area: 5.67e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 232770

# halfnorm-NoCovarsFt

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = dataDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, outputUnits = "ft")
      Coefficients:
                   Estimate  SE       z      p(>|z|)
      (Intercept)  5.147     0.03768  136.6  0      
      
      Message: Success; Asymptotic SE's
      Function: HALFNORM  
      Strip: 0 [ft] to 679.1 [ft] 
      Effective strip width (ESW): 215.5 [ft] 
      Probability of detection: 0.3174
      Scaling: g(0 [ft]) = 1
      Log likelihood: -2091 
      AICc: 4183
      
           Surveyed Units: 118110 [ft] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.051 
         Group size range: 1 to 3 
      Density in sampled area: 7.346e-06 [1/ft^2]
      Abundance in 4.419e+10 [ft^2] study area: 324578

# halfnorm-NoCovarCosExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = dataDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", series = "cosine")
      Coefficients:
                   Estimate  SE       z       p(>|z|)  
      (Intercept)  4.0756    0.05584  72.987  0.0000000
      a1           0.4587    0.13026   3.521  0.0004298
      a2           0.1450    0.10348   1.401  0.1611601
      
      Message: Success; Asymptotic SE's
      Function: HALFNORM with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 207 [m] 
      Effective strip width (ESW): 54.93 [m] 
      Probability of detection: 0.2654
      Scaling: g(0 [m]) = 1
      Log likelihood: -1662 
      AICc: 3330
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.051 
         Group size range: 1 to 3 
      Density in sampled area: 9.456e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 388182

# halfnorm-NoCovarSinExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = dataDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", series = "sine")
      Coefficients:
                   Estimate  SE       z        p(>|z|)
      (Intercept)   3.94298  0.04185  94.2264  0.00000
      a1           -0.46829  0.24914  -1.8797  0.06015
      a2            0.09273  0.16466   0.5631  0.57335
      
      Message: Success; Asymptotic SE's
      Function: HALFNORM with 2 expansion(s) of SINE series 
      Strip: 0 [m] to 207 [m] 
      Effective strip width (ESW): 56.55 [m] 
      Probability of detection: 0.2732
      Scaling: g(0 [m]) = 1
      Log likelihood: -1666 
      AICc: 3337
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.051 
         Group size range: 1 to 3 
      Density in sampled area: 9.185e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 377057

# halfnorm-NoCovarHermExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = dataDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", series = "hermite")
      Coefficients:
                   Estimate  SE       z        p(>|z|)  
      (Intercept)   5.19333  0.56795    9.144  6.018e-20
      a1           -0.23634  0.01159  -20.399  1.723e-92
      a2            0.09706  0.01059    9.167  4.863e-20
      
      Message: Success; Asymptotic SE's
      Function: HALFNORM with 2 expansion(s) of HERMITE series 
      Strip: 0 [m] to 207 [m] 
      Effective strip width (ESW): 53.2 [m] 
      Probability of detection: 0.257
      Scaling: g(0 [m]) = 1
      Log likelihood: -1655 
      AICc: 3316
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.051 
         Group size range: 1 to 3 
      Density in sampled area: 9.764e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 400823

# halfnorm-NoCovarSimpExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = dataDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", series = "simple")
      Coefficients:
                   Estimate  SE      z       p(>|z|)   
      (Intercept)   3.994    0.1144  34.913  4.677e-267
      a1           -8.537    3.3730  -2.531   1.138e-02
      a2           15.366    8.4166   1.826   6.789e-02
      
      Message: Success; Asymptotic SE's
      Function: HALFNORM with 2 expansion(s) of SIMPLE series 
      Strip: 0 [m] to 207 [m] 
      Effective strip width (ESW): 64.74 [m] 
      Probability of detection: 0.3128
      Scaling: g(0 [m]) = 1
      Log likelihood: -1663 
      AICc: 3332
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.051 
         Group size range: 1 to 3 
      Density in sampled area: 8.024e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 329366

# halfnorm-NoCovarBSplineExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = dataDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", series = "bspline")
      Coefficients:
                   Estimate  SE   z    p(>|z|)
      (Intercept)   5.1966   NaN  NaN  NaN    
      a1           -1.2492   NaN  NaN  NaN    
      a2           -0.9454   NaN  NaN  NaN    
      
      Message: FAILURE (Exit code= 1, false convergence (8))
      Function: HALFNORM with 2 expansion(s) of BSPLINE series 
      Strip: 0 [m] to 207 [m] 
      Effective strip width (ESW): 53.21 [m] 
      Probability of detection: 0.2571
      Scaling: g(0 [m]) = 1
      Log likelihood: -1655 
      AICc: 3316
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.051 
         Group size range: 1 to 3 
      Density in sampled area: NA
      Abundance in 4105 [km^2] study area: NA

# halfnorm-ContCovarExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = dataDf, formula = dist ~ bare +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m")
      Coefficients:
                   Estimate  SE        z       p(>|z|)  
      (Intercept)  3.38599   0.167122  20.261  2.863e-91
      bare         0.01106   0.002704   4.088  4.352e-05
      a1           0.34055   0.165192   2.062  3.925e-02
      a2           0.15481   0.114444   1.353  1.761e-01
      
      Message: Success; Asymptotic SE's
      Function: HALFNORM with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 207 [m] 
      Average effective strip width (ESW): 55.6 [m] (range 43.29 [m] to 69.32 [m]) 
      Average probability of detection: 0.2686 (range 0.2091 to 0.3349)
      Scaling: g(0 [m]) = 1
      Log likelihood: -1654 
      AICc: 3317
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.051 
         Group size range: 1 to 3 
      Density in sampled area: 9.517e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 390690

# halfnorm-ContCovarExpansionsWScaling

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = dataDf, formula = dist ~ bare +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", x.scl = xScl, g.x.scl = gXscl)
      Coefficients:
                   Estimate  SE        z       p(>|z|)  
      (Intercept)  3.38599   0.167122  20.261  2.863e-91
      bare         0.01106   0.002704   4.088  4.352e-05
      a1           0.34055   0.165192   2.062  3.925e-02
      a2           0.15481   0.114444   1.353  1.761e-01
      
      Message: Success; Asymptotic SE's
      Function: HALFNORM with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 207 [m] 
      Average effective strip width (ESW): 41.7 [m] (range 32.47 [m] to 51.99 [m]) 
      Average probability of detection: 0.2015 (range 0.1569 to 0.2512)
      Scaling: g(0 [m]) = 0.75
      Log likelihood: -1654 
      AICc: 3317
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.051 
         Group size range: 1 to 3 
      Density in sampled area: 0.0001269 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 520919

# halfnorm-SerialBootstraps

    Code
      summary(abun)
    Output
      Call: dfuncEstim(data = dataDf, formula = dist ~ groupsize(groupsize),
         likelihood = lhood)
      Coefficients:
                   Estimate  SE       z      p(>|z|)
      (Intercept)  3.959     0.05118  77.35  0      
      
      Message: Success; Bootstrap SE's
      Function: HALFNORM  
      Strip: 0 [m] to 207 [m] 
      Effective strip width (ESW): 65.7 [m] 
      Probability of detection: 0.3174
      Scaling: g(0 [m]) = 1
      Log likelihood: -1668 
      AICc: 3337
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.051 
         Group size range: 1 to 3 
      
      Density in sampled area: 7.907e-05 [1/m^2]
                       95% CI: 6.248e-05 [1/m^2] to 8.923e-05 [1/m^2]
      
      Abundance in 4.105e+09 [m^2] study area: 324578
                                       95% CI: 256462 to 366306

