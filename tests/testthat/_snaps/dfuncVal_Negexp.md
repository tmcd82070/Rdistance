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
      (Intercept)  -3.5110844  0.1107906  -31.6911642  2.056394e-220
      a1           -0.8843675  0.6801797   -1.3001969   1.935335e-01
      a2           -0.2556691  0.2624684   -0.9740946   3.300096e-01
      
      Message: Success; Asymptotic SE's
      Function: NEGEXP with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 207 [m] 
      Effective strip width (ESW): 44.36253 [m] 
      Probability of detection: 0.2143117
      Scaling: g(0 [m]) = 1
      Log likelihood: -1661.041 
      AICc: 3328.15
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.050562 
         Group size range: 1 to 3 
      Density in sampled area: 0.0001170908 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 480657.8

# negexp-NoCovarSinExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", series = "sine")
      Coefficients:
                   Estimate     SE         z            p(>|z|)      
      (Intercept)  -3.79591767  0.1004898  -37.7741618  3.018574e-312
      a1            1.04974502  0.4215420    2.4902501   1.276532e-02
      a2            0.06638949  0.2981446    0.2226755   8.237881e-01
      
      Message: Success; Asymptotic SE's
      Function: NEGEXP with 2 expansion(s) of SINE series 
      Strip: 0 [m] to 207 [m] 
      Effective strip width (ESW): 55.63593 [m] 
      Probability of detection: 0.2687726
      Scaling: g(0 [m]) = 1
      Log likelihood: -1658.853 
      AICc: 3323.775
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.050562 
         Group size range: 1 to 3 
      Density in sampled area: 9.336492e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 383263

# negexp-NoCovarHermExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", series = "hermite")
      Coefficients:
                   Estimate    SE         z            p(>|z|)     
      (Intercept)  -3.3678413  0.3253899  -10.3501730  4.177305e-25
      a1            0.9411764  3.1093732    0.3026901  7.621261e-01
      a2           -0.1473032  0.5759530   -0.2557555  7.981396e-01
      
      Message: Success; Asymptotic SE's
      Function: NEGEXP with 2 expansion(s) of HERMITE series 
      Strip: 0 [m] to 207 [m] 
      Effective strip width (ESW): 51.11359 [m] 
      Probability of detection: 0.2469256
      Scaling: g(0 [m]) = 1
      Log likelihood: -1661.13 
      AICc: 3328.328
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.050562 
         Group size range: 1 to 3 
      Density in sampled area: 0.0001016255 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 417172.7

# negexp-NoCovarSimpExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", series = "simple")
      Coefficients:
                   Estimate   SE   z    p(>|z|)
      (Intercept)  -4.034305  NaN  NaN  NaN    
      a1           -7.253898  NaN  NaN  NaN    
      a2            7.277163  NaN  NaN  NaN    
      
      Message: FAILURE (Exit code= 1, false convergence (8))
      Function: NEGEXP with 2 expansion(s) of SIMPLE series 
      Strip: 0 [m] to 207 [m] 
      Effective strip width (ESW): 48.04341 [m] 
      Probability of detection: 0.2320938
      Scaling: g(0 [m]) = 1
      Log likelihood: -1654.345 
      AICc: 3314.758
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.050562 
         Group size range: 1 to 3 
      Density in sampled area: NA
      Abundance in 4105 [km^2] study area: NA

# negexp-ContCovarExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ bare +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m")
      Coefficients:
                   Estimate     SE           z           p(>|z|)     
      (Intercept)  -2.73058052  0.210090770  -12.997147  1.269939e-38
      bare         -0.01277882  0.003442318   -3.712271  2.054079e-04
      a1           -1.21936808  0.830716624   -1.467851  1.421447e-01
      a2           -0.31683603  0.295889972   -1.070790  2.842639e-01
      
      Message: Success; Asymptotic SE's
      Function: NEGEXP with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 207 [m] 
      Average effective strip width (ESW): 45.8658 [m] (range 29.52729 [m] to 65.36594 [m]) 
      Average probability of detection: 0.2215739 (range 0.1426439 to 0.3157775)
      Scaling: g(0 [m]) = 1
      Log likelihood: -1654.15 
      AICc: 3316.414
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.050562 
         Group size range: 1 to 3 
      Density in sampled area: 0.0001192786 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 489638.6

# negexp-ContCovarExpansionsScaling

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ bare +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", x.scl = xScl, g.x.scl = gXscl)
      Coefficients:
                   Estimate     SE           z           p(>|z|)     
      (Intercept)  -2.73058052  0.210090770  -12.997147  1.269939e-38
      bare         -0.01277882  0.003442318   -3.712271  2.054079e-04
      a1           -1.21936808  0.830716624   -1.467851  1.421447e-01
      a2           -0.31683603  0.295889972   -1.070790  2.842639e-01
      
      Message: Success; Asymptotic SE's
      Function: NEGEXP with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 207 [m] 
      Average effective strip width (ESW): 45.8658 [m] (range 29.52729 [m] to 65.36594 [m]) 
      Average probability of detection: 0.2215739 (range 0.1426439 to 0.3157775)
      Scaling: g(0 [m]) = 0.75
      Log likelihood: -1654.15 
      AICc: 3316.414
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.050562 
         Group size range: 1 to 3 
      Density in sampled area: 0.0001192786 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 489638.6

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

