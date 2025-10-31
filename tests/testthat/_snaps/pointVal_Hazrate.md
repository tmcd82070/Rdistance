# hazrate-MinimumInputs

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~
         groupsize(groupsize), likelihood = lhood)
      Coefficients:
                   Estimate  SE          z         p(>|z|)    
      (Intercept)  4.540434  0.06265929  72.46226  0.00000e+00
      k            4.199564  0.39721227  10.57259  3.99292e-26
      
      Message: Success; Asymptotic SE's
      Function: HAZRATE  
      Strip: 0 [m] to 265 [m] 
      Effective detection radius (EDR): 118.6254 [m] 
      Probability of detection: 0.2003842
      Scaling: g(0 [m]) = 1
      Log likelihood: -999.0241 
      AICc: 2002.111
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.015544 
         Group size range: 1 to 2 
      Density in sampled area: 3.69462e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 151664.1

# hazrate-NoCovar

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, w.lo = w.lo, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = 0, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                   Estimate  SE          z          p(>|z|)     
      (Intercept)  4.526813  0.06486727  69.785782  0.000000e+00
      k            3.963643  0.47089290   8.417291  3.852886e-17
      
      Message: Success; Asymptotic SE's
      Function: HAZRATE  
      Strip: 0 [m] to 200 [m] 
      Effective detection radius (EDR): 115.7242 [m] 
      Probability of detection: 0.3348022
      Scaling: g(0 [m]) = 1
      Log likelihood: -932.8086 
      AICc: 1869.683
      
           Surveyed Units: 120 
         Individuals seen: 188 in 185 groups 
       Average group size: 1.016216 
         Group size range: 1 to 2 
      Density in sampled area: 3.723732e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 152859.2

# hazrate-ContinuousCovar

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ bare +
         groupsize(groupsize), likelihood = lhood, w.lo = w.lo, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = 0, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                   Estimate    SE           z          p(>|z|)     
      (Intercept)  3.90757233  0.370784273  10.538668  5.730479e-26
      bare         0.01605943  0.009417654   1.705248  8.814822e-02
      k            4.06653714  0.484368842   8.395538  4.637648e-17
      
      Message: Success; Asymptotic SE's
      Function: HAZRATE  
      Strip: 0 [m] to 200 [m] 
      Average effective detection radius (EDR): 116.4435 [m] (range 101.2568 [m] to 136.9467 [m]) 
      Average probability of detection: 0.3406227 (range 0.2563233 to 0.4688598)
      Scaling: g(0 [m]) = 1
      Log likelihood: -931.5143 
      AICc: 1869.161
      
           Surveyed Units: 120 
         Individuals seen: 188 in 185 groups 
       Average group size: 1.016216 
         Group size range: 1 to 2 
      Density in sampled area: 3.72731e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 153006.1

# hazrate-FactorCovar

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ observer +
         groupsize(groupsize), likelihood = lhood, w.lo = w.lo, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = 0, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                    Estimate     SE         z           p(>|z|)     
      (Intercept)    5.05175584  0.1220379  41.3949797  0.000000e+00
      observerobs2  -0.08950050  0.1299713  -0.6886175  4.910640e-01
      observerobs3  -0.39227291  0.1590876  -2.4657672  1.367202e-02
      observerobs4  -0.65859150  0.1242173  -5.3019289  1.145855e-07
      observerobs5  -0.06681955  0.1449113  -0.4611067  6.447221e-01
      observerobs6  -0.48599998  0.1368073  -3.5524411  3.816745e-04
      k              7.29077658  1.6933741   4.3054731  1.666293e-05
      
      Message: Success; Asymptotic SE's
      Function: HAZRATE  
      Strip: 0 [m] to 200 [m] 
      Average effective detection radius (EDR): 131.59 [m] (range 90.6622 [m] to 168.3412 [m]) 
      Average probability of detection: 0.4579888 (range 0.2054909 to 0.7084693)
      Scaling: g(0 [m]) = 1
      Log likelihood: -909.6747 
      AICc: 1833.982
      
           Surveyed Units: 120 
         Individuals seen: 188 in 185 groups 
       Average group size: 1.016216 
         Group size range: 1 to 2 
      Density in sampled area: 3.482606e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 142961

# hazrate-NoCovarWloWhi

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, w.lo = w.20, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = w.20, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                   Estimate  SE          z          p(>|z|)     
      (Intercept)  4.085785  0.08872293  46.051054  0.000000e+00
      k            3.174197  0.32400942   9.796621  1.164154e-22
      
      Message: Success; Asymptotic SE's
      Function: HAZRATE  
      Strip: 20 [m] to 200 [m] 
      Effective detection radius (EDR): 82.89567 [m] 
      Probability of detection: 0.2120892
      Scaling: g(20 [m]) = 1
      Log likelihood: -914.9397 
      AICc: 1833.946
      
           Surveyed Units: 120 
         Individuals seen: 187 in 184 groups 
       Average group size: 1.016304 
         Group size range: 1 to 2 
      Density in sampled area: 7.218498e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 296319.3

# hazrate-NoCovarWlo

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, w.lo = w.20, expansions =
         0, series = "cosine", x.scl = w.20, g.x.scl = 1, outputUnits = "m")
      Coefficients:
                   Estimate  SE          z         p(>|z|)     
      (Intercept)  4.117367  0.08146999  50.53845  0.000000e+00
      k            3.421335  0.28021402  12.20972  2.758274e-34
      
      Message: Success; Asymptotic SE's
      Function: HAZRATE  
      Strip: 20 [m] to 265 [m] 
      Effective detection radius (EDR): 85.45976 [m] 
      Probability of detection: 0.1216721
      Scaling: g(20 [m]) = 1
      Log likelihood: -981.628 
      AICc: 1967.32
      
           Surveyed Units: 120 
         Individuals seen: 195 in 192 groups 
       Average group size: 1.015625 
         Group size range: 1 to 2 
      Density in sampled area: 7.082395e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 290732.3

# hazrate-NoCovarsFt

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, outputUnits = "ft")
      Coefficients:
                   Estimate  SE          z         p(>|z|)    
      (Intercept)  5.728533  0.06267096  91.40651  0.00000e+00
      k            4.199564  0.39728670  10.57061  4.07814e-26
      
      Message: Success; Asymptotic SE's
      Function: HAZRATE  
      Strip: 0 [ft] to 869.4226 [ft] 
      Effective detection radius (EDR): 389.1909 [ft] 
      Probability of detection: 0.2003843
      Scaling: g(0 [ft]) = 1
      Log likelihood: -1228.327 
      AICc: 2460.718
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.015544 
         Group size range: 1 to 2 
      Density in sampled area: 3.432413e-06 [1/ft^2]
      Abundance in 44185852261 [ft^2] study area: 151664.1

# hazrate-NoCovarCosExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", series = "cosine")
      Coefficients:
                   Estimate   SE         z           p(>|z|)      
      (Intercept)   4.326691  0.1131519  38.2379138  6.605658e-320
      k             3.491206  0.3250280  10.7412475   6.515789e-27
      a1           -8.216837  8.0995861  -1.0144761   3.103556e-01
      a2           -3.766622  3.8274935  -0.9840962   3.250682e-01
      
      Message: Success; Asymptotic SE's
      Function: HAZRATE with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 265 [m] 
      Effective detection radius (EDR): 289.2667 [m] 
      Probability of detection: 1.191531 > 1
      Scaling: g(0 [m]) = 1 <- Check scaling
      Log likelihood: -990.9346 
      AICc: 1990.082
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.015544 
         Group size range: 1 to 2 
      Density in sampled area: 6.21338e-06 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 25505.93

# hazrate-NoCovarSinExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", series = "sine")
      Coefficients:
                   Estimate   SE          z           p(>|z|)     
      (Intercept)  4.5975100  0.09410682  48.8541626  0.000000e+00
      k            3.7588446  0.62775698   5.9877384  2.127789e-09
      a1           0.6502632  0.54229313   1.1990991  2.304894e-01
      a2           0.1981850  0.31683705   0.6255109  5.316358e-01
      
      Message: Success; Asymptotic SE's
      Function: HAZRATE with 2 expansion(s) of SINE series 
      Strip: 0 [m] to 265 [m] 
      Effective detection radius (EDR): 136.43 [m] 
      Probability of detection: 0.2650503
      Scaling: g(0 [m]) = 1
      Log likelihood: -998.4344 
      AICc: 2005.082
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.015544 
         Group size range: 1 to 2 
      Density in sampled area: 2.793219e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 114661.6

# hazrate-NoCovarHermExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", series = "hermite")
      Coefficients:
                   Estimate       SE   z    p(>|z|)
      (Intercept)       4.342872  NaN  NaN  NaN    
      k                 3.831493  NaN  NaN  NaN    
      a1             3274.776250  NaN  NaN  NaN    
      a2           -11096.195411  NaN  NaN  NaN    
      
      Message: FAILURE (Exit code= 1, singular convergence (7))
      Function: HAZRATE with 2 expansion(s) of HERMITE series 
      Strip: 0 [m] to 265 [m] 
      Effective detection radius (EDR): 19428.44 [m] 
      Probability of detection: 5375.072 > 1
      Scaling: g(0 [m]) = 1 <- Check scaling
      Log likelihood: -991.9629 
      AICc: 1992.138
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.015544 
         Group size range: 1 to 2 
      Density in sampled area: NA
      Abundance in 4105 [km^2] study area: NA

# hazrate-NoCovarSimpExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", series = "simple")
      Coefficients:
                   Estimate    SE          z            p(>|z|)    
      (Intercept)   4.5329137  0.06834053  66.32833830  0.000000000
      k             3.7422353  1.22375426   3.05799576  0.002228227
      a1           -1.1113175  5.36311571  -0.20721490  0.835842031
      a2            0.4809531  5.08924575   0.09450382  0.924708951
      
      Message: Success; Asymptotic SE's
      Function: HAZRATE with 2 expansion(s) of SIMPLE series 
      Strip: 0 [m] to 265 [m] 
      Effective detection radius (EDR): 118.1196 [m] 
      Probability of detection: 0.1986791
      Scaling: g(0 [m]) = 1
      Log likelihood: -998.6731 
      AICc: 2005.559
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.015544 
         Group size range: 1 to 2 
      Density in sampled area: 3.726328e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 152965.8

# hazrate-ContCovarExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ bare +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m")
      Coefficients:
                   Estimate    SE   z    p(>|z|)
      (Intercept)   4.1962293  NaN  NaN  NaN    
      bare         -0.5659779  NaN  NaN  NaN    
      k             1.3065025  NaN  NaN  NaN    
      a1           -0.9665067  NaN  NaN  NaN    
      a2           -0.9720320  NaN  NaN  NaN    
      
      Message: FAILURE (Exit code= 1, false convergence (8))
      Function: HAZRATE with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 265 [m] 
      Average effective detection radius (EDR): 0.0004564452 [m] (range 3.950899e-06 [m] to 0.004405314 [m]) 
      Average probability of detection: 1.068815e-11 (range 2.222798e-16 to 2.763516e-10)
      Scaling: g(0 [m]) = 1
      Log likelihood: -1045.685 
      AICc: 2101.69
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.015544 
         Group size range: 1 to 2 
      Density in sampled area: NA
      Abundance in 4105 [km^2] study area: NA

---

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ bare +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", x.scl = xScl, g.x.scl = gXscl)
      Coefficients:
                   Estimate    SE   z    p(>|z|)
      (Intercept)   4.1962293  NaN  NaN  NaN    
      bare         -0.5659779  NaN  NaN  NaN    
      k             1.3065025  NaN  NaN  NaN    
      a1           -0.9665067  NaN  NaN  NaN    
      a2           -0.9720320  NaN  NaN  NaN    
      
      Message: FAILURE (Exit code= 1, false convergence (8))
      Function: HAZRATE with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 265 [m] 
      Average effective detection radius (EDR): 0.0004564452 [m] (range 3.950899e-06 [m] to 0.004405314 [m]) 
      Average probability of detection: 1.068815e-11 (range 2.222798e-16 to 2.763516e-10)
      Scaling: g(0 [m]) = 0.75
      Log likelihood: -1045.685 
      AICc: 2101.69
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.015544 
         Group size range: 1 to 2 
      Density in sampled area: NA
      Abundance in 4105 [km^2] study area: NA

# hazrate-Bootstraps

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~
         groupsize(groupsize), likelihood = lhood)
      Coefficients:
                   Estimate  SE          z          p(>|z|)     
      (Intercept)  4.540434  0.06397395  70.973168  0.000000e+00
      k            4.199564  0.49498325   8.484255  2.171045e-17
      
      Message: Success; Bootstrap SE's
      Function: HAZRATE  
      Strip: 0 [m] to 265 [m] 
      Effective detection radius (EDR): 118.6254 [m] 
      Probability of detection: 0.2003842
      Scaling: g(0 [m]) = 1
      Log likelihood: -999.0241 
      AICc: 2002.111
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.015544 
         Group size range: 1 to 2 
      
      Density in sampled area: 3.69462e-05 [1/m^2]
                       95% CI: 3.321246e-05 [1/m^2] to 4.140141e-05 [1/m^2]
      
      Abundance in 4.105e+09 [m^2] study area: 151664.1
                                       95% CI: 136337.1 to 169952.8

