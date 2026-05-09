# triangle-MinimumInputs

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = dataDf, formula = dist ~ groupsize(groupsize),
         likelihood = lhood)
      Coefficients:
                   Estimate
      (Intercept)  4.81674 
      p            0.01507 
      
      Message: Success; SE's pending bootstrap
      Function: TRIANGLE  
      Strip: 0 [m] to 207 [m] 
      Effective strip width (ESW): 63.97 [m] 
      Probability of detection: 0.309
      Scaling: g(0 [m]) = 1
      Log likelihood: -1665 
      AICc: 3335
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.051 
         Group size range: 1 to 3 
      Density in sampled area: 8.12e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 333327

# triangle-NoCovar

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = dataDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, w.lo = w.lo, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = 0, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                   Estimate
      (Intercept)  4.57936 
      p            0.06236 
      
      Message: Success; SE's pending bootstrap
      Function: TRIANGLE  
      Strip: 0 [m] to 150 [m] 
      Effective strip width (ESW): 55.04 [m] 
      Probability of detection: 0.3669
      Scaling: g(0 [m]) = 1
      Log likelihood: -1632 
      AICc: 3268
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 371 in 353 groups 
       Average group size: 1.051 
         Group size range: 1 to 3 
      Density in sampled area: 9.362e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 384317

# triangle-ContinuousCovar

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = dataDf, formula = dist ~ bare +
         groupsize(groupsize), likelihood = lhood, w.lo = w.lo, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = 0, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                   Estimate
      (Intercept)  4.251129
      bare         0.005926
      p            0.056521
      
      Message: Success; SE's pending bootstrap
      Function: TRIANGLE  
      Strip: 0 [m] to 150 [m] 
      Average effective strip width (ESW): 54.96 [m] (range 48.19 [m] to 62.02 [m]) 
      Average probability of detection: 0.3664 (range 0.3213 to 0.4135)
      Scaling: g(0 [m]) = 1
      Log likelihood: -1629 
      AICc: 3265
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 371 in 353 groups 
       Average group size: 1.051 
         Group size range: 1 to 3 
      Density in sampled area: 9.429e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 387051

# triangle-FactorCovar

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = dataDf, formula = dist ~ observer +
         groupsize(groupsize), likelihood = lhood, w.lo = w.lo, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = 0, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                    Estimate
      (Intercept)    4.68595
      observerobs2   0.08662
      observerobs3   0.12467
      observerobs4  -0.24901
      observerobs5  -0.01679
      p              0.04320
      
      Message: Success; SE's pending bootstrap
      Function: TRIANGLE  
      Strip: 0 [m] to 150 [m] 
      Average effective strip width (ESW): 58.78 [m] (range 46.91 [m] to 65.23 [m]) 
      Average probability of detection: 0.3919 (range 0.3128 to 0.4349)
      Scaling: g(0 [m]) = 1
      Log likelihood: -1631 
      AICc: 3274
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 371 in 353 groups 
       Average group size: 1.051 
         Group size range: 1 to 3 
      Density in sampled area: 8.856e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 363523

# triangle-NoCovarWloWhi

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = dataDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, w.lo = w.20, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = w.20, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                   Estimate
      (Intercept)  4.13690 
      p            0.05055 
      
      Message: Success; SE's pending bootstrap
      Function: TRIANGLE  
      Strip: 20 [m] to 150 [m] 
      Effective strip width (ESW): 36.29 [m] 
      Probability of detection: 0.2792
      Scaling: g(20 [m]) = 1
      Log likelihood: -987.2 
      AICc: 1978
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 241 in 233 groups 
       Average group size: 1.034 
         Group size range: 1 to 2 
      Density in sampled area: 9.222e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 378575

# triangle-NoCovarWlo

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = dataDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, w.lo = w.20, expansions =
         0, series = "cosine", x.scl = w.20, g.x.scl = 1, outputUnits = "m")
      Coefficients:
                   Estimate
      (Intercept)  4.33179 
      p            0.02709 
      
      Message: Success; SE's pending bootstrap
      Function: TRIANGLE  
      Strip: 20 [m] to 207 [m] 
      Effective strip width (ESW): 42.08 [m] 
      Probability of detection: 0.225
      Scaling: g(20 [m]) = 1
      Log likelihood: -1025 
      AICc: 2055
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 244 in 236 groups 
       Average group size: 1.034 
         Group size range: 1 to 2 
      Density in sampled area: 8.054e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 330626

# triangle-NoCovarsFt

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = dataDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, outputUnits = "ft")
      Coefficients:
                   Estimate
      (Intercept)   6.5047 
      p            -0.9686 
      
      Message: FAILURE (Exit code= -1, p parameter at lower boundary.)
      Function: TRIANGLE  
      Strip: 0 [ft] to 679.1 [ft] 
      Effective strip width (ESW): 0.00722 [ft] 
      Probability of detection: 1.063e-05
      Scaling: g(0 [ft]) = 1
      Log likelihood: 10434 
      AICc: -20865
      
           Surveyed Units: 118110 [ft] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.051 
         Group size range: 1 to 3 
      Density in sampled area: NA
      Abundance in 4105 [km^2] study area: NA

# triangle-NoCovarCosExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = dataDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", series = "cosine")
      Coefficients:
                   Estimate
      (Intercept)  4.82610 
      p            0.01171 
      a1           0.34718 
      a2           0.08045 
      
      Message: Success; SE's pending bootstrap
      Function: TRIANGLE with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 207 [m] 
      Effective strip width (ESW): 50.57 [m] 
      Probability of detection: 0.2443
      Scaling: g(0 [m]) = 1
      Log likelihood: -1661 
      AICc: 3330
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.051 
         Group size range: 1 to 3 
      Density in sampled area: 0.0001027 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 421678

# triangle-NoCovarSinExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = dataDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", series = "sine")
      Coefficients:
                   Estimate
      (Intercept)  4.82943 
      p            0.01788 
      a1           0.47574 
      a2           0.19196 
      
      Message: Success; SE's pending bootstrap
      Function: TRIANGLE with 2 expansion(s) of SINE series 
      Strip: 0 [m] to 207 [m] 
      Effective strip width (ESW): 70.75 [m] 
      Probability of detection: 0.3418
      Scaling: g(0 [m]) = 1
      Log likelihood: -1662 
      AICc: 3332
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.051 
         Group size range: 1 to 3 
      Density in sampled area: 7.342e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 301408

# triangle-NoCovarHermExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = dataDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", series = "hermite")
      Coefficients:
                   Estimate
      (Intercept)   4.88121
      p             0.01039
      a1           -0.12553
      a2            0.05106
      
      Message: Success; SE's pending bootstrap
      Function: TRIANGLE with 2 expansion(s) of HERMITE series 
      Strip: 0 [m] to 207 [m] 
      Effective strip width (ESW): 47.04 [m] 
      Probability of detection: 0.2272
      Scaling: g(0 [m]) = 1
      Log likelihood: -1658 
      AICc: 3324
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.051 
         Group size range: 1 to 3 
      Density in sampled area: 0.0001104 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 453310

# triangle-NoCovarSimpExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = dataDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", series = "simple")
      Coefficients:
                   Estimate
      (Intercept)   4.69175
      p             0.02693
      a1           -0.31614
      a2            0.10717
      
      Message: FAILURE (Exit code= 10, )
      Function: TRIANGLE with 2 expansion(s) of SIMPLE series 
      Strip: 0 [m] to 207 [m] 
      Effective strip width (ESW): 57.58 [m] 
      Probability of detection: 0.2781
      Scaling: g(0 [m]) = 1
      Log likelihood: -1667 
      AICc: 3343
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.051 
         Group size range: 1 to 3 
      Density in sampled area: NA
      Abundance in 4105 [km^2] study area: NA

# triangle-NoCovarBSplineExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = dataDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", series = "bspline")
      Coefficients:
                   Estimate
      (Intercept)   4.74100
      p             0.02189
      a1           -0.82798
      a2            0.22595
      
      Message: Success; SE's pending bootstrap
      Function: TRIANGLE with 2 expansion(s) of BSPLINE series 
      Strip: 0 [m] to 207 [m] 
      Effective strip width (ESW): 48.15 [m] 
      Probability of detection: 0.2326
      Scaling: g(0 [m]) = 1
      Log likelihood: -1665 
      AICc: 3338
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.051 
         Group size range: 1 to 3 
      Density in sampled area: 0.0001079 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 442865

# triangle-ContCovarExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = dataDf, formula = dist ~ bare +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m")
      Coefficients:
                   Estimate
      (Intercept)  4.398366
      bare         0.006651
      p            0.012829
      a1           0.289553
      a2           0.055052
      
      Message: Success; SE's pending bootstrap
      Function: TRIANGLE with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 207 [m] 
      Average effective strip width (ESW): 51.07 [m] (range 43.18 [m] to 59.32 [m]) 
      Average probability of detection: 0.2467 (range 0.2086 to 0.2866)
      Scaling: g(0 [m]) = 1
      Log likelihood: -1656 
      AICc: 3322
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.051 
         Group size range: 1 to 3 
      Density in sampled area: 0.0001026 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 421228

# triangle-ContCovarExpansionsWScaling

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = dataDf, formula = dist ~ bare +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", x.scl = xScl, g.x.scl = gXscl)
      Coefficients:
                   Estimate
      (Intercept)  4.398366
      bare         0.006651
      p            0.012829
      a1           0.289553
      a2           0.055052
      
      Message: Success; SE's pending bootstrap
      Function: TRIANGLE with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 207 [m] 
      Average effective strip width (ESW): 38.3 [m] (range 32.39 [m] to 44.49 [m]) 
      Average probability of detection: 0.185 (range 0.1565 to 0.2149)
      Scaling: g(0 [m]) = 0.75
      Log likelihood: -1656 
      AICc: 3322
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.051 
         Group size range: 1 to 3 
      Density in sampled area: 0.0001368 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 561637

# triangle-SerialBootstraps

    Code
      summary(abun)
    Output
      Call: dfuncEstim(data = dataDf, formula = dist ~ groupsize(groupsize),
         likelihood = lhood)
      Coefficients:
                   Estimate  SE      z        p(>|z|)  
      (Intercept)  4.81674   0.2079  23.1728  8.57e-119
      p            0.01507   0.3166   0.0476   9.62e-01
      
      Message: Success; Bootstrap SE's
      Function: TRIANGLE  
      Strip: 0 [m] to 207 [m] 
      Effective strip width (ESW): 63.97 [m] 
      Probability of detection: 0.309
      Scaling: g(0 [m]) = 1
      Log likelihood: -1665 
      AICc: 3335
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.051 
         Group size range: 1 to 3 
      
      Density in sampled area: 8.12e-05 [1/m^2]
                       95% CI: 6.47e-05 [1/m^2] to 0.0001033 [1/m^2]
      
      Abundance in 4.105e+09 [m^2] study area: 333327
                                       95% CI: 265611 to 424034
      CI based on 17 of 20 successful bootstrap iterations

