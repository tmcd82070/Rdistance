# halfnorm-MinimumInputs

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~
         groupsize(groupsize), likelihood = lhood)
      Coefficients:
                   Estimate  SE          z         p(>|z|)
      (Intercept)  3.959313  0.03767961  105.0784  0      
      
      Message: Success; Asymptotic SE's
      Function: HALFNORM  
      Strip: 0 [m] to 207 [m] 
      Effective strip width (ESW): 65.69518 [m] 
      Probability of detection: 0.317368
      Scaling: g(0 [m]) = 1
      Log likelihood: -1667.639 
      AICc: 3337.289
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.050562 
         Group size range: 1 to 3 
      Density in sampled area: 7.906888e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 324577.7

# halfnorm-NoCovar

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, w.lo = w.lo, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = 0, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                   Estimate  SE          z         p(>|z|)
      (Intercept)  3.9095    0.04038592  96.80353  0      
      
      Message: Success; Asymptotic SE's
      Function: HALFNORM  
      Strip: 0 [m] to 150 [m] 
      Effective strip width (ESW): 62.34318 [m] 
      Probability of detection: 0.4156212
      Scaling: g(0 [m]) = 1
      Log likelihood: -1630.716 
      AICc: 3263.443
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 371 in 353 groups 
       Average group size: 1.050992 
         Group size range: 1 to 3 
      Density in sampled area: 8.265183e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 339285.8

# halfnorm-ContinuousCovar

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ bare +
         groupsize(groupsize), likelihood = lhood, w.lo = w.lo, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = 0, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                   Estimate     SE           z          p(>|z|)      
      (Intercept)  3.346183910  0.155241297  21.554728  4.780716e-103
      bare         0.009715925  0.002720183   3.571791   3.545479e-04
      
      Message: Success; Asymptotic SE's
      Function: HALFNORM  
      Strip: 0 [m] to 150 [m] 
      Average effective strip width (ESW): 61.99663 [m] (range 47.95033 [m] to 76.9778 [m]) 
      Average probability of detection: 0.4133109 (range 0.3196689 to 0.5131854)
      Scaling: g(0 [m]) = 1
      Log likelihood: -1624.351 
      AICc: 3252.736
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 371 in 353 groups 
       Average group size: 1.050992 
         Group size range: 1 to 3 
      Density in sampled area: 8.470704e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 347722.4

# halfnorm-FactorCovar

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ observer +
         groupsize(groupsize), likelihood = lhood, w.lo = w.lo, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = 0, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                    Estimate     SE          z           p(>|z|)   
      (Intercept)    3.92800543  0.08793471  44.6695675  0.00000000
      observerobs2   0.17073246  0.14604669   1.1690265  0.24239295
      observerobs3  -0.01647247  0.11923776  -0.1381481  0.89012337
      observerobs4  -0.26739638  0.12897924  -2.0731737  0.03815612
      observerobs5  -0.02839298  0.12345494  -0.2299866  0.81810216
      
      Message: Success; Asymptotic SE's
      Function: HALFNORM  
      Strip: 0 [m] to 150 [m] 
      Average effective strip width (ESW): 62.19478 [m] (range 48.72956 [m] to 74.5625 [m]) 
      Average probability of detection: 0.4146318 (range 0.3248637 to 0.4970833)
      Scaling: g(0 [m]) = 1
      Log likelihood: -1626.142 
      AICc: 3262.456
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 371 in 353 groups 
       Average group size: 1.050992 
         Group size range: 1 to 3 
      Density in sampled area: 8.387749e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 344317.1

# halfnorm-NoCovarWloWhi

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, w.lo = w.20, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = w.20, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                   Estimate  SE          z         p(>|z|)
      (Intercept)  3.783568  0.05013421  75.46878  0      
      
      Message: Success; Asymptotic SE's
      Function: HALFNORM  
      Strip: 20 [m] to 150 [m] 
      Effective strip width (ESW): 54.94 [m] 
      Probability of detection: 0.4226154
      Scaling: g(20 [m]) = 1
      Log likelihood: -1046.467 
      AICc: 2094.952
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 241 in 233 groups 
       Average group size: 1.034335 
         Group size range: 1 to 2 
      Density in sampled area: 6.092505e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 250097.3

# halfnorm-NoCovarWlo

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, w.lo = w.20, expansions =
         0, series = "cosine", x.scl = w.20, g.x.scl = 1, outputUnits = "m")
      Coefficients:
                   Estimate  SE          z         p(>|z|)
      (Intercept)  3.864711  0.04630146  83.46845  0      
      
      Message: Success; Asymptotic SE's
      Function: HALFNORM  
      Strip: 20 [m] to 207 [m] 
      Effective strip width (ESW): 59.76463 [m] 
      Probability of detection: 0.3195969
      Scaling: g(20 [m]) = 1
      Log likelihood: -1083.168 
      AICc: 2168.354
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 244 in 236 groups 
       Average group size: 1.033898 
         Group size range: 1 to 2 
      Density in sampled area: 5.670392e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 232769.6

# halfnorm-NoCovarsFt

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, outputUnits = "ft")
      Coefficients:
                   Estimate  SE         z       p(>|z|)
      (Intercept)  5.147412  0.0376796  136.61  0      
      
      Message: Success; Asymptotic SE's
      Function: HALFNORM  
      Strip: 0 [ft] to 679.1339 [ft] 
      Effective strip width (ESW): 215.5353 [ft] 
      Probability of detection: 0.317368
      Scaling: g(0 [ft]) = 1
      Log likelihood: -2090.602 
      AICc: 4183.216
      
           Surveyed Units: 118110.2 [ft] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.050562 
         Group size range: 1 to 3 
      Density in sampled area: 7.345741e-06 [1/ft^2]
      Abundance in 44185852261 [ft^2] study area: 324577.8

# halfnorm-NoCovarCosExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", series = "cosine")
      Coefficients:
                   Estimate   SE          z          p(>|z|)     
      (Intercept)  4.0755827  0.05583971  72.987181  0.0000000000
      a1           0.4586695  0.13026429   3.521068  0.0004298119
      a2           0.1449905  0.10347736   1.401181  0.1611600646
      
      Message: Success; Asymptotic SE's
      Function: HALFNORM with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 207 [m] 
      Effective strip width (ESW): 54.93086 [m] 
      Probability of detection: 0.2653665
      Scaling: g(0 [m]) = 1
      Log likelihood: -1662.166 
      AICc: 3330.4
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.050562 
         Group size range: 1 to 3 
      Density in sampled area: 9.456332e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 388182.4

# halfnorm-NoCovarSinExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", series = "sine")
      Coefficients:
                   Estimate     SE          z          p(>|z|)   
      (Intercept)   3.94297933  0.04184599  94.225969  0.00000000
      a1           -0.46828903  0.24913668  -1.879647  0.06015619
      a2            0.09272589  0.16466424   0.563121  0.57335248
      
      Message: Success; Asymptotic SE's
      Function: HALFNORM with 2 expansion(s) of SINE series 
      Strip: 0 [m] to 207 [m] 
      Effective strip width (ESW): 56.55192 [m] 
      Probability of detection: 0.2731977
      Scaling: g(0 [m]) = 1
      Log likelihood: -1665.683 
      AICc: 3337.435
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.050562 
         Group size range: 1 to 3 
      Density in sampled area: 9.185266e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 377055.2

# halfnorm-NoCovarHermExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", series = "hermite")
      Coefficients:
                   Estimate     SE         z         p(>|z|)     
      (Intercept)   5.20519141  0.3870045  13.44995  3.080833e-41
      a1           -0.23640284         NA        NA            NA
      a2            0.09711706         NA        NA            NA
      
      Message: VARIANCE FAILURE (singular variance-covariance matrix)
      Function: HALFNORM with 2 expansion(s) of HERMITE series 
      Strip: 0 [m] to 207 [m] 
      Effective strip width (ESW): 53.21236 [m] 
      Probability of detection: 0.2570645
      Scaling: g(0 [m]) = 1
      Log likelihood: -1654.989 
      AICc: 3316.045
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.050562 
         Group size range: 1 to 3 
      Density in sampled area: 9.761725e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 400718.8

# halfnorm-NoCovarSimpExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", series = "simple")
      Coefficients:
                   Estimate   SE        z          p(>|z|)      
      (Intercept)   3.993693  0.114389  34.913247  4.680116e-267
      a1           -8.536728  3.372959  -2.530931   1.137601e-02
      a2           15.366334  8.416604   1.825717   6.789294e-02
      
      Message: Success; Asymptotic SE's
      Function: HALFNORM with 2 expansion(s) of SIMPLE series 
      Strip: 0 [m] to 207 [m] 
      Effective strip width (ESW): 64.74022 [m] 
      Probability of detection: 0.3127547
      Scaling: g(0 [m]) = 1
      Log likelihood: -1662.79 
      AICc: 3331.648
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.050562 
         Group size range: 1 to 3 
      Density in sampled area: 8.02352e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 329365.5

# halfnorm-NoCovarBSplineExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", series = "bspline")
      Coefficients:
                   Estimate    SE          z          p(>|z|)      
      (Intercept)   5.2053945  0.50797762   10.24729   1.217068e-24
      a1           -1.2497467  0.04791432  -26.08295  5.692591e-150
      a2           -0.9456234  0.03279225  -28.83680  7.418013e-183
      
      Message: Success; Asymptotic SE's
      Function: HALFNORM with 2 expansion(s) of BSPLINE series 
      Strip: 0 [m] to 207 [m] 
      Effective strip width (ESW): 53.21257 [m] 
      Probability of detection: 0.2570655
      Scaling: g(0 [m]) = 1
      Log likelihood: -1654.989 
      AICc: 3316.045
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.050562 
         Group size range: 1 to 3 
      Density in sampled area: 9.761688e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 400717.3

# halfnorm-ContCovarExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ bare +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m")
      Coefficients:
                   Estimate    SE           z          p(>|z|)     
      (Intercept)  3.38598814  0.167121543  20.260632  2.862629e-91
      bare         0.01105555  0.002704431   4.087941  4.352188e-05
      a1           0.34055057  0.165192401   2.061539  3.925165e-02
      a2           0.15481272  0.114443570   1.352743  1.761378e-01
      
      Message: Success; Asymptotic SE's
      Function: HALFNORM with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 207 [m] 
      Average effective strip width (ESW): 55.60095 [m] (range 43.29357 [m] to 69.32326 [m]) 
      Average probability of detection: 0.2686036 (range 0.2091477 to 0.334895)
      Scaling: g(0 [m]) = 1
      Log likelihood: -1654.264 
      AICc: 3316.642
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.050562 
         Group size range: 1 to 3 
      Density in sampled area: 9.517406e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 390689.5

---

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ bare +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", x.scl = xScl, g.x.scl = gXscl)
      Coefficients:
                   Estimate    SE           z          p(>|z|)     
      (Intercept)  3.38598814  0.167121543  20.260632  2.862629e-91
      bare         0.01105555  0.002704431   4.087941  4.352188e-05
      a1           0.34055057  0.165192401   2.061539  3.925165e-02
      a2           0.15481272  0.114443570   1.352743  1.761378e-01
      
      Message: Success; Asymptotic SE's
      Function: HALFNORM with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 207 [m] 
      Average effective strip width (ESW): 55.60095 [m] (range 43.29357 [m] to 69.32326 [m]) 
      Average probability of detection: 0.2686036 (range 0.2091477 to 0.334895)
      Scaling: g(0 [m]) = 0.75
      Log likelihood: -1654.264 
      AICc: 3316.642
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.050562 
         Group size range: 1 to 3 
      Density in sampled area: 9.517406e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 390689.5

