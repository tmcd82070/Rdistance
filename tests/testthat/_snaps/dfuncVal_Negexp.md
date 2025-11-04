# negexp-MinimumInputs

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~
         groupsize(groupsize), likelihood = lhood)
      Coefficients:
                   Estimate   SE          z          p(>|z|)
      (Intercept)  -3.712969  0.05801385  -64.00142  0      
      
      Message: Success; Asymptotic SE's
      Function: NEGEXP  
      Strip: 0 [m] to 207 [m] 
      Effective strip width (ESW): 40.71315 [m] 
      Probability of detection: 0.1966819
      Scaling: g(0 [m]) = 1
      Log likelihood: -1663.952 
      AICc: 3329.915
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.050562 
         Group size range: 1 to 3 
      Density in sampled area: 0.0001275864 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 523742.2

# negexp-NoCovar

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, w.lo = w.lo, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = 0, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                   Estimate   SE          z          p(>|z|)
      (Intercept)  -3.760954  0.06845064  -54.94404  0      
      
      Message: Success; Asymptotic SE's
      Function: NEGEXP  
      Strip: 0 [m] to 150 [m] 
      Effective strip width (ESW): 41.67721 [m] 
      Probability of detection: 0.2778481
      Scaling: g(0 [m]) = 1
      Log likelihood: -1630.893 
      AICc: 3263.797
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 371 in 353 groups 
       Average group size: 1.050992 
         Group size range: 1 to 3 
      Density in sampled area: 0.0001236354 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 507523.2

# negexp-ContinuousCovar

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ bare +
         groupsize(groupsize), likelihood = lhood, w.lo = w.lo, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = 0, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                   Estimate     SE           z           p(>|z|)     
      (Intercept)  -2.98953166  0.252782154  -11.826514  2.847227e-32
      bare         -0.01359076  0.004521654   -3.005704  2.649664e-03
      
      Message: Success; Asymptotic SE's
      Function: NEGEXP  
      Strip: 0 [m] to 150 [m] 
      Average effective strip width (ESW): 42.01149 [m] (range 29.95859 [m] to 54.96365 [m]) 
      Average probability of detection: 0.2800766 (range 0.1997239 to 0.3664243)
      Scaling: g(0 [m]) = 1
      Log likelihood: -1626.391 
      AICc: 3256.816
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 371 in 353 groups 
       Average group size: 1.050992 
         Group size range: 1 to 3 
      Density in sampled area: 0.0001264913 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 519246.8

# negexp-FactorCovar

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ observer +
         groupsize(groupsize), likelihood = lhood, w.lo = w.lo, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = 0, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                    Estimate     SE         z             p(>|z|)      
      (Intercept)   -3.81382485  0.1521734  -25.06235837  1.280301e-138
      observerobs2  -0.14929543  0.2485709   -0.60061507   5.480964e-01
      observerobs3   0.04891658  0.2045285    0.23916760   8.109756e-01
      observerobs4   0.36577782  0.2120902    1.72463325   8.459359e-02
      observerobs5   0.01950569  0.2139414    0.09117305   9.273551e-01
      
      Message: Success; Asymptotic SE's
      Function: NEGEXP  
      Strip: 0 [m] to 150 [m] 
      Average effective strip width (ESW): 41.94674 [m] (range 31.17263 [m] to 49.57912 [m]) 
      Average probability of detection: 0.2796449 (range 0.2078175 to 0.3305275)
      Scaling: g(0 [m]) = 1
      Log likelihood: -1628.324 
      AICc: 3266.822
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 371 in 353 groups 
       Average group size: 1.050992 
         Group size range: 1 to 3 
      Density in sampled area: 0.0001249166 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 512782.6

# negexp-NoCovarWloWhi

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, w.lo = w.20, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = w.20, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                   Estimate   SE          z          p(>|z|)
      (Intercept)  -3.792641  0.07264876  -52.20518  0      
      
      Message: Success; Asymptotic SE's
      Function: NEGEXP  
      Strip: 2 [m] to 150 [m] 
      Effective strip width (ESW): 42.79363 [m] 
      Probability of detection: 0.2891462
      Scaling: g(2 [m]) = 1
      Log likelihood: -1528.975 
      AICc: 3059.962
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 347 in 330 groups 
       Average group size: 1.051515 
         Group size range: 1 to 3 
      Density in sampled area: 0.0001126206 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 462307.6

# negexp-NoCovarWlo

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, w.lo = w.20, expansions =
         0, series = "cosine", x.scl = w.20, g.x.scl = 1, outputUnits = "m")
      Coefficients:
                   Estimate  SE          z         p(>|z|)
      (Intercept)  -3.73547  0.06060494  -61.6364  0      
      
      Message: Success; Asymptotic SE's
      Function: NEGEXP  
      Strip: 2 [m] to 207 [m] 
      Effective strip width (ESW): 41.59304 [m] 
      Probability of detection: 0.2028929
      Scaling: g(2 [m]) = 1
      Log likelihood: -1562.078 
      AICc: 3126.168
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 350 in 333 groups 
       Average group size: 1.051051 
         Group size range: 1 to 3 
      Density in sampled area: 0.0001168732 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 479764.4

# negexp-NoCovarsFt

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, outputUnits = "ft")
      Coefficients:
                   Estimate   SE          z        p(>|z|)
      (Intercept)  -4.901069  0.05801386  -84.481  0      
      
      Message: Success; Asymptotic SE's
      Function: NEGEXP  
      Strip: 0 [ft] to 679.1339 [ft] 
      Effective strip width (ESW): 133.5733 [ft] 
      Probability of detection: 0.1966819
      Scaling: g(0 [ft]) = 1
      Log likelihood: -2086.915 
      AICc: 4175.842
      
           Surveyed Units: 118110.2 [ft] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.050562 
         Group size range: 1 to 3 
      Density in sampled area: 1.185316e-05 [1/ft^2]
      Abundance in 44185852261 [ft^2] study area: 523742.1

# negexp-NoCovarCosExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", series = "cosine")
      Coefficients:
                   Estimate    SE         z            p(>|z|)      
      (Intercept)  -3.5110804  0.1107913  -31.6909341  2.071462e-220
      a1           -0.8843878  0.6801919   -1.3002033   1.935313e-01
      a2           -0.2556761  0.2624708   -0.9741127   3.300006e-01
      
      Message: Success; Asymptotic SE's
      Function: NEGEXP with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 207 [m] 
      Effective strip width (ESW): 44.36258 [m] 
      Probability of detection: 0.214312
      Scaling: g(0 [m]) = 1
      Log likelihood: -1661.041 
      AICc: 3328.15
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.050562 
         Group size range: 1 to 3 
      Density in sampled area: 0.0001170907 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 480657.3

# negexp-NoCovarSinExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", series = "sine")
      Coefficients:
                   Estimate     SE         z            p(>|z|)      
      (Intercept)  -3.79591842  0.1004901  -37.7740530  3.031008e-312
      a1            1.04974690  0.4215434    2.4902464   1.276546e-02
      a2            0.06639294  0.2981453    0.2226865   8.237795e-01
      
      Message: Success; Asymptotic SE's
      Function: NEGEXP with 2 expansion(s) of SINE series 
      Strip: 0 [m] to 207 [m] 
      Effective strip width (ESW): 55.63601 [m] 
      Probability of detection: 0.268773
      Scaling: g(0 [m]) = 1
      Log likelihood: -1658.853 
      AICc: 3323.775
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.050562 
         Group size range: 1 to 3 
      Density in sampled area: 9.336479e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 383262.4

# negexp-NoCovarHermExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", series = "hermite")
      Coefficients:
                   Estimate    SE         z            p(>|z|)     
      (Intercept)  -3.3678534  0.3253595  -10.3511750  4.133810e-25
      a1            0.9410717  3.1088212    0.3027101  7.621108e-01
      a2           -0.1473158  0.5758344   -0.2558302  7.980819e-01
      
      Message: Success; Asymptotic SE's
      Function: NEGEXP with 2 expansion(s) of HERMITE series 
      Strip: 0 [m] to 207 [m] 
      Effective strip width (ESW): 51.11335 [m] 
      Probability of detection: 0.2469244
      Scaling: g(0 [m]) = 1
      Log likelihood: -1661.13 
      AICc: 3328.328
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.050562 
         Group size range: 1 to 3 
      Density in sampled area: 0.000101626 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 417174.7

# negexp-NoCovarSimpExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", series = "simple")
      Coefficients:
                   Estimate   SE          z          p(>|z|)     
      (Intercept)  -4.035686  0.09965554  -40.49635  0.000000e+00
      a1           -7.252964  0.72301535  -10.03155  1.107648e-23
      a2            7.263011  1.00782618    7.20661  5.736186e-13
      
      Message: Success; Asymptotic SE's
      Function: NEGEXP with 2 expansion(s) of SIMPLE series 
      Strip: 0 [m] to 207 [m] 
      Effective strip width (ESW): 48.07879 [m] 
      Probability of detection: 0.2322647
      Scaling: g(0 [m]) = 1
      Log likelihood: -1654.35 
      AICc: 3314.768
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.050562 
         Group size range: 1 to 3 
      Density in sampled area: 0.0001080402 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 443505.2

# negexp-ContCovarExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ bare +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m")
      Coefficients:
                   Estimate     SE           z           p(>|z|)     
      (Intercept)  -2.73057366  0.210091008  -12.997099  1.270726e-38
      bare         -0.01277884  0.003442313   -3.712283  2.053981e-04
      a1           -1.21940424  0.830738617   -1.467855  1.421435e-01
      a2           -0.31684593  0.295893853   -1.070809  2.842551e-01
      
      Message: Success; Asymptotic SE's
      Function: NEGEXP with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 207 [m] 
      Average effective strip width (ESW): 45.86588 [m] (range 29.52725 [m] to 65.36621 [m]) 
      Average probability of detection: 0.2215743 (range 0.1426437 to 0.3157788)
      Scaling: g(0 [m]) = 1
      Log likelihood: -1654.15 
      AICc: 3316.413
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.050562 
         Group size range: 1 to 3 
      Density in sampled area: 0.0001192785 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 489638.1

# negexp-ContCovarExpansionsScaling

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ bare +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", x.scl = xScl, g.x.scl = gXscl)
      Coefficients:
                   Estimate     SE           z           p(>|z|)     
      (Intercept)  -2.73057366  0.210091008  -12.997099  1.270726e-38
      bare         -0.01277884  0.003442313   -3.712283  2.053981e-04
      a1           -1.21940424  0.830738617   -1.467855  1.421435e-01
      a2           -0.31684593  0.295893853   -1.070809  2.842551e-01
      
      Message: Success; Asymptotic SE's
      Function: NEGEXP with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 207 [m] 
      Average effective strip width (ESW): 45.86588 [m] (range 29.52725 [m] to 65.36621 [m]) 
      Average probability of detection: 0.2215743 (range 0.1426437 to 0.3157788)
      Scaling: g(0 [m]) = 0.75
      Log likelihood: -1654.15 
      AICc: 3316.413
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.050562 
         Group size range: 1 to 3 
      Density in sampled area: 0.0001192785 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 489638.1

# negexp-Bootstraps

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~
         groupsize(groupsize), likelihood = lhood)
      Coefficients:
                   Estimate   SE          z         p(>|z|)
      (Intercept)  -3.712969  0.07079145  -52.4494  0      
      
      Message: Success; Bootstrap SE's
      Function: NEGEXP  
      Strip: 0 [m] to 207 [m] 
      Effective strip width (ESW): 40.71315 [m] 
      Probability of detection: 0.1966819
      Scaling: g(0 [m]) = 1
      Log likelihood: -1663.952 
      AICc: 3329.915
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.050562 
         Group size range: 1 to 3 
      
      Density in sampled area: 0.0001275864 [1/m^2]
                       95% CI: 9.926176e-05 [1/m^2] to 0.0001486051 [1/m^2]
      
      Abundance in 4.105e+09 [m^2] study area: 523742.2
                                       95% CI: 407469.5 to 610023.8

