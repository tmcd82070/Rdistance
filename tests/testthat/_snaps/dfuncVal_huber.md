# huber-MinimumInputs

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = dataDf, formula = dist ~ groupsize(groupsize),
         likelihood = lhood)
      Coefficients:
                   Estimate
      (Intercept)   -7.1054
      theta2       111.2460
      p              0.0262
      
      Message: Success; SE's pending bootstrap
      Function: HUBER  
      Strip: 0 [m] to 207 [m] 
      Effective strip width (ESW): 59.59 [m] 
      Probability of detection: 0.2879
      Scaling: g(0 [m]) = 1
      Log likelihood: -1668 
      AICc: 3341
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.051 
         Group size range: 1 to 3 
      Density in sampled area: 8.717e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 357824

# huber-NoCovar

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = dataDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, w.lo = w.lo, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = 0, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                   Estimate
      (Intercept)  -7.32985
      theta2       97.46737
      p             0.06225
      
      Message: Success; SE's pending bootstrap
      Function: HUBER  
      Strip: 0 [m] to 150 [m] 
      Effective strip width (ESW): 55.03 [m] 
      Probability of detection: 0.3669
      Scaling: g(0 [m]) = 1
      Log likelihood: -1632 
      AICc: 3271
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 371 in 353 groups 
       Average group size: 1.051 
         Group size range: 1 to 3 
      Density in sampled area: 9.363e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 384341

# huber-ContinuousCovar

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = dataDf, formula = dist ~ bare +
         groupsize(groupsize), likelihood = lhood, w.lo = w.lo, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = 0, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                   Estimate
      (Intercept)  -2.59154
      bare          0.08555
      theta2       88.16167
      p             0.04586
      
      Message: Success; SE's pending bootstrap
      Function: HUBER  
      Strip: 0 [m] to 150 [m] 
      Average effective strip width (ESW): 61.08 [m] (range 49.68 [m] to 101.4 [m]) 
      Average probability of detection: 0.4072 (range 0.3312 to 0.6762)
      Scaling: g(0 [m]) = 1
      Log likelihood: -1625 
      AICc: 3259
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 371 in 353 groups 
       Average group size: 1.051 
         Group size range: 1 to 3 
      Density in sampled area: 8.724e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 358125

# huber-FactorCovar

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = dataDf, formula = dist ~ observer +
         groupsize(groupsize), likelihood = lhood, w.lo = w.lo, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = 0, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                    Estimate
      (Intercept)    3.77316
      observerobs2   0.25101
      observerobs3   0.09912
      observerobs4  -0.26486
      observerobs5  -0.28245
      theta2        49.49774
      p              0.07597
      
      Message: Success; SE's pending bootstrap
      Function: HUBER  
      Strip: 0 [m] to 150 [m] 
      Average effective strip width (ESW): 62.82 [m] (range 56.37 [m] to 71.47 [m]) 
      Average probability of detection: 0.4188 (range 0.3758 to 0.4765)
      Scaling: g(0 [m]) = 1
      Log likelihood: -1633 
      AICc: 3280
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 371 in 353 groups 
       Average group size: 1.051 
         Group size range: 1 to 3 
      Density in sampled area: 8.247e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 338533

# huber-NoCovarWloWhi

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = dataDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, w.lo = w.20, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = w.20, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                   Estimate 
      (Intercept)    0.25820
      theta2       102.05264
      p              0.03524
      
      Message: Success; SE's pending bootstrap
      Function: HUBER  
      Strip: 20 [m] to 150 [m] 
      Effective strip width (ESW): 54.75 [m] 
      Probability of detection: 0.4211
      Scaling: g(20 [m]) = 1
      Log likelihood: -1048 
      AICc: 2102
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 241 in 233 groups 
       Average group size: 1.034 
         Group size range: 1 to 2 
      Density in sampled area: 6.114e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 250983

# huber-NoCovarWlo

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = dataDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, w.lo = w.20, expansions =
         0, series = "cosine", x.scl = w.20, g.x.scl = 1, outputUnits = "m")
      Coefficients:
                   Estimate 
      (Intercept)    0.24580
      theta2       106.40302
      p              0.01811
      
      Message: Success; SE's pending bootstrap
      Function: HUBER  
      Strip: 20 [m] to 207 [m] 
      Effective strip width (ESW): 56.57 [m] 
      Probability of detection: 0.3025
      Scaling: g(20 [m]) = 1
      Log likelihood: -1077 
      AICc: 2160
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 244 in 236 groups 
       Average group size: 1.034 
         Group size range: 1 to 2 
      Density in sampled area: 5.991e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 245930

# huber-NoCovarsFt

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = dataDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, outputUnits = "ft")
      Coefficients:
                   Estimate 
      (Intercept)   -7.70199
      theta2       405.32108
      p              0.01506
      
      Message: Success; SE's pending bootstrap
      Function: HUBER  
      Strip: 0 [ft] to 679.1 [ft] 
      Effective strip width (ESW): 209.8 [ft] 
      Probability of detection: 0.309
      Scaling: g(0 [ft]) = 1
      Log likelihood: -2088 
      AICc: 4183
      
           Surveyed Units: 118110 [ft] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.051 
         Group size range: 1 to 3 
      Density in sampled area: 7.545e-06 [1/ft^2]
      Abundance in 4.419e+10 [ft^2] study area: 333384

# huber-NoCovarCosExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = dataDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", series = "cosine")
      Coefficients:
                   Estimate
      (Intercept)   4.09983
      theta2       46.78341
      p             0.02613
      a1            0.39532
      a2            0.03339
      
      Message: Success; SE's pending bootstrap
      Function: HUBER with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 207 [m] 
      Effective strip width (ESW): 55 [m] 
      Probability of detection: 0.2657
      Scaling: g(0 [m]) = 1
      Log likelihood: -1667 
      AICc: 3345
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.051 
         Group size range: 1 to 3 
      Density in sampled area: 9.444e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 387694

# huber-NoCovarSinExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = dataDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", series = "sine")
      Coefficients:
                   Estimate
      (Intercept)   4.12856
      theta2       49.70739
      p             0.03655
      a1            0.41854
      a2            0.27700
      
      Message: Success; SE's pending bootstrap
      Function: HUBER with 2 expansion(s) of SINE series 
      Strip: 0 [m] to 207 [m] 
      Effective strip width (ESW): 79.68 [m] 
      Probability of detection: 0.3849
      Scaling: g(0 [m]) = 1
      Log likelihood: -1670 
      AICc: 3350
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.051 
         Group size range: 1 to 3 
      Density in sampled area: 6.519e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 267618

# huber-NoCovarHermExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = dataDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", series = "hermite")
      Coefficients:
                   Estimate
      (Intercept)   4.5030 
      theta2       49.7797 
      p             0.0103 
      a1           -0.1773 
      a2            0.0702 
      
      Message: Success; SE's pending bootstrap
      Function: HUBER with 2 expansion(s) of HERMITE series 
      Strip: 0 [m] to 207 [m] 
      Effective strip width (ESW): 49.66 [m] 
      Probability of detection: 0.2399
      Scaling: g(0 [m]) = 1
      Log likelihood: -1658 
      AICc: 3326
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.051 
         Group size range: 1 to 3 
      Density in sampled area: 0.0001046 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 429417

# huber-NoCovarSimpExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = dataDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", series = "simple")
      Coefficients:
                   Estimate
      (Intercept)   4.31921
      theta2       46.67089
      p             0.01224
      a1           -4.84390
      a2            6.20639
      
      Message: Success; SE's pending bootstrap
      Function: HUBER with 2 expansion(s) of SIMPLE series 
      Strip: 0 [m] to 207 [m] 
      Effective strip width (ESW): 69.7 [m] 
      Probability of detection: 0.3367
      Scaling: g(0 [m]) = 1
      Log likelihood: -1663 
      AICc: 3337
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.051 
         Group size range: 1 to 3 
      Density in sampled area: 7.452e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 305919

# huber-NoCovarBSplineExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = dataDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", series = "bspline")
      Coefficients:
                   Estimate
      (Intercept)   4.4117 
      theta2       50.7621 
      p             0.0111 
      a1           -1.4146 
      a2            0.1006 
      
      Message: Success; SE's pending bootstrap
      Function: HUBER with 2 expansion(s) of BSPLINE series 
      Strip: 0 [m] to 207 [m] 
      Effective strip width (ESW): 49.01 [m] 
      Probability of detection: 0.2368
      Scaling: g(0 [m]) = 1
      Log likelihood: -1658 
      AICc: 3326
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.051 
         Group size range: 1 to 3 
      Density in sampled area: 0.000106 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 435081

# huber-ContCovarExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = dataDf, formula = dist ~ bare +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m")
      Coefficients:
                   Estimate
      (Intercept)   1.14813
      bare          0.04056
      theta2       70.45831
      p             0.01947
      a1            0.35828
      a2            0.15335
      
      Message: Success; SE's pending bootstrap
      Function: HUBER with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 207 [m] 
      Average effective strip width (ESW): 48.29 [m] (range 35.21 [m] to 72.19 [m]) 
      Average probability of detection: 0.2333 (range 0.1701 to 0.3487)
      Scaling: g(0 [m]) = 1
      Log likelihood: -1656 
      AICc: 3324
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.051 
         Group size range: 1 to 3 
      Density in sampled area: 0.0001119 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 459511

# huber-ContCovarExpansionsWScaling

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = dataDf, formula = dist ~ bare +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", x.scl = xScl, g.x.scl = gXscl)
      Coefficients:
                   Estimate
      (Intercept)   1.14813
      bare          0.04056
      theta2       70.45831
      p             0.01947
      a1            0.35828
      a2            0.15335
      
      Message: Success; SE's pending bootstrap
      Function: HUBER with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 207 [m] 
      Average effective strip width (ESW): 36.22 [m] (range 26.41 [m] to 54.14 [m]) 
      Average probability of detection: 0.175 (range 0.1276 to 0.2615)
      Scaling: g(0 [m]) = 0.75
      Log likelihood: -1656 
      AICc: 3324
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.051 
         Group size range: 1 to 3 
      Density in sampled area: 0.0001493 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 612681

# huber-SerialBootstraps

    Code
      summary(abun)
    Output
      Call: dfuncEstim(data = dataDf, formula = dist ~ groupsize(groupsize),
         likelihood = lhood)
      Coefficients:
                   Estimate  SE        z       p(>|z|)  
      (Intercept)   -7.1054   5.99706  -1.185  2.361e-01
      theta2       111.2460  22.61171   4.920  8.661e-07
      p              0.0262   0.01291   2.029  4.249e-02
      
      Message: Success; Bootstrap SE's
      Function: HUBER  
      Strip: 0 [m] to 207 [m] 
      Effective strip width (ESW): 59.59 [m] 
      Probability of detection: 0.2879
      Scaling: g(0 [m]) = 1
      Log likelihood: -1668 
      AICc: 3341
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.051 
         Group size range: 1 to 3 
      
      Density in sampled area: 8.717e-05 [1/m^2]
                       95% CI: 6.911e-05 [1/m^2] to 0.0001063 [1/m^2]
      
      Abundance in 4.105e+09 [m^2] study area: 357824
                                       95% CI: 283715 to 436446
      CI based on 18 of 20 successful bootstrap iterations

