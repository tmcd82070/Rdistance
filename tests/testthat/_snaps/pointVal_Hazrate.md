# hazrate-MinimumInputs

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~
         groupsize(groupsize), likelihood = lhood)
      Coefficients:
                   Estimate  SE       z      p(>|z|)  
      (Intercept)  4.54      0.06266  72.46  0.000e+00
      k            4.20      0.39721  10.57  3.993e-26
      
      Message: Success; Asymptotic SE's
      Function: HAZRATE  
      Strip: 0 [m] to 265 [m] 
      Effective detection radius (EDR): 118.6 [m] 
      Probability of detection: 0.2004
      Scaling: g(0 [m]) = 1
      Log likelihood: -999 
      AICc: 2002
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.016 
         Group size range: 1 to 2 
      Density in sampled area: 3.695e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 151664

# hazrate-NoCovar

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, w.lo = w.lo, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = 0, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                   Estimate  SE       z       p(>|z|)  
      (Intercept)  4.527     0.06487  69.786  0.000e+00
      k            3.964     0.47089   8.417  3.853e-17
      
      Message: Success; Asymptotic SE's
      Function: HAZRATE  
      Strip: 0 [m] to 200 [m] 
      Effective detection radius (EDR): 115.7 [m] 
      Probability of detection: 0.3348
      Scaling: g(0 [m]) = 1
      Log likelihood: -932.8 
      AICc: 1870
      
           Surveyed Units: 120 
         Individuals seen: 188 in 185 groups 
       Average group size: 1.016 
         Group size range: 1 to 2 
      Density in sampled area: 3.724e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 152859

# hazrate-ContinuousCovar

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ bare +
         groupsize(groupsize), likelihood = lhood, w.lo = w.lo, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = 0, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                   Estimate  SE        z       p(>|z|)  
      (Intercept)  3.90756   0.370784  10.539  5.731e-26
      bare         0.01606   0.009418   1.705  8.814e-02
      k            4.06655   0.484370   8.396  4.637e-17
      
      Message: Success; Asymptotic SE's
      Function: HAZRATE  
      Strip: 0 [m] to 200 [m] 
      Average effective detection radius (EDR): 116.4 [m] (range 101.3 [m] to 136.9 [m]) 
      Average probability of detection: 0.3406 (range 0.2563 to 0.4689)
      Scaling: g(0 [m]) = 1
      Log likelihood: -931.5 
      AICc: 1869
      
           Surveyed Units: 120 
         Individuals seen: 188 in 185 groups 
       Average group size: 1.016 
         Group size range: 1 to 2 
      Density in sampled area: 3.727e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 153006

# hazrate-FactorCovar

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ observer +
         groupsize(groupsize), likelihood = lhood, w.lo = w.lo, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = 0, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                    Estimate  SE      z        p(>|z|)  
      (Intercept)    5.05176  0.1220  41.3943  0.000e+00
      observerobs2  -0.08951  0.1300  -0.6887  4.910e-01
      observerobs3  -0.39227  0.1591  -2.4658  1.367e-02
      observerobs4  -0.65859  0.1242  -5.3019  1.146e-07
      observerobs5  -0.06683  0.1449  -0.4612  6.447e-01
      observerobs6  -0.48600  0.1368  -3.5525  3.817e-04
      k              7.29082  1.6935   4.3052  1.668e-05
      
      Message: Success; Asymptotic SE's
      Function: HAZRATE  
      Strip: 0 [m] to 200 [m] 
      Average effective detection radius (EDR): 131.6 [m] (range 90.66 [m] to 168.3 [m]) 
      Average probability of detection: 0.458 (range 0.2055 to 0.7085)
      Scaling: g(0 [m]) = 1
      Log likelihood: -909.7 
      AICc: 1834
      
           Surveyed Units: 120 
         Individuals seen: 188 in 185 groups 
       Average group size: 1.016 
         Group size range: 1 to 2 
      Density in sampled area: 3.483e-05 [1/m^2]
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
                   Estimate  SE       z       p(>|z|)  
      (Intercept)  4.086     0.08872  46.051  0.000e+00
      k            3.174     0.32401   9.797  1.164e-22
      
      Message: Success; Asymptotic SE's
      Function: HAZRATE  
      Strip: 20 [m] to 200 [m] 
      Effective detection radius (EDR): 82.9 [m] 
      Probability of detection: 0.2121
      Scaling: g(20 [m]) = 1
      Log likelihood: -914.9 
      AICc: 1834
      
           Surveyed Units: 120 
         Individuals seen: 187 in 184 groups 
       Average group size: 1.016 
         Group size range: 1 to 2 
      Density in sampled area: 7.218e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 296319

# hazrate-NoCovarWlo

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, w.lo = w.20, expansions =
         0, series = "cosine", x.scl = w.20, g.x.scl = 1, outputUnits = "m")
      Coefficients:
                   Estimate  SE       z      p(>|z|)  
      (Intercept)  4.117     0.08147  50.54  0.000e+00
      k            3.421     0.28021  12.21  2.758e-34
      
      Message: Success; Asymptotic SE's
      Function: HAZRATE  
      Strip: 20 [m] to 265 [m] 
      Effective detection radius (EDR): 85.46 [m] 
      Probability of detection: 0.1217
      Scaling: g(20 [m]) = 1
      Log likelihood: -981.6 
      AICc: 1967
      
           Surveyed Units: 120 
         Individuals seen: 195 in 192 groups 
       Average group size: 1.016 
         Group size range: 1 to 2 
      Density in sampled area: 7.082e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 290732

# hazrate-NoCovarsFt

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, outputUnits = "ft")
      Coefficients:
                   Estimate  SE       z      p(>|z|)  
      (Intercept)  5.729     0.06267  91.41  0.000e+00
      k            4.200     0.39729  10.57  4.078e-26
      
      Message: Success; Asymptotic SE's
      Function: HAZRATE  
      Strip: 0 [ft] to 869.4 [ft] 
      Effective detection radius (EDR): 389.2 [ft] 
      Probability of detection: 0.2004
      Scaling: g(0 [ft]) = 1
      Log likelihood: -1228 
      AICc: 2461
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.016 
         Group size range: 1 to 2 
      Density in sampled area: 3.432e-06 [1/ft^2]
      Abundance in 4.419e+10 [ft^2] study area: 151664

# hazrate-NoCovarCosExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", series = "cosine")
      Coefficients:
                   Estimate  SE      z        p(>|z|)   
      (Intercept)   4.327    0.1132  38.2379  6.612e-320
      k             3.491    0.3250  10.7412   6.516e-27
      a1           -8.217    8.0996  -1.0145   3.104e-01
      a2           -3.767    3.8275  -0.9841   3.251e-01
      
      Message: Success; Asymptotic SE's
      Function: HAZRATE with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 265 [m] 
      Effective detection radius (EDR): 289.3 [m] 
      Probability of detection: 1.192 > 1
      Scaling: g(0 [m]) = 1 <- Check scaling
      Log likelihood: -990.9 
      AICc: 1990
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.016 
         Group size range: 1 to 2 
      Density in sampled area: 6.213e-06 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 25506

# hazrate-NoCovarSinExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", series = "sine")
      Coefficients:
                   Estimate  SE       z        p(>|z|)  
      (Intercept)  4.5975    0.09411  48.8542  0.000e+00
      k            3.7588    0.62776   5.9877  2.128e-09
      a1           0.6503    0.54229   1.1991  2.305e-01
      a2           0.1982    0.31684   0.6255  5.316e-01
      
      Message: Success; Asymptotic SE's
      Function: HAZRATE with 2 expansion(s) of SINE series 
      Strip: 0 [m] to 265 [m] 
      Effective detection radius (EDR): 136.4 [m] 
      Probability of detection: 0.2651
      Scaling: g(0 [m]) = 1
      Log likelihood: -998.4 
      AICc: 2005
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.016 
         Group size range: 1 to 2 
      Density in sampled area: 2.793e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 114662

# hazrate-NoCovarHermExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", series = "hermite")
      Coefficients:
                   Estimate    SE   z    p(>|z|)
      (Intercept)       4.341  NaN  NaN  NaN    
      k                 3.765  NaN  NaN  NaN    
      a1             2955.986  NaN  NaN  NaN    
      a2           -15865.293  NaN  NaN  NaN    
      
      Message: FAILURE (Exit code= 1, singular convergence (7))
      Function: HAZRATE with 2 expansion(s) of HERMITE series 
      Strip: 0 [m] to 265 [m] 
      Effective detection radius (EDR): 22853 [m] 
      Probability of detection: 7437 > 1
      Scaling: g(0 [m]) = 1 <- Check scaling
      Log likelihood: -992.2 
      AICc: 1993
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.016 
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
                   Estimate  SE       z        p(>|z|)
      (Intercept)   4.5329   0.06834  66.3291  0.00000
      k             3.7424   1.22387   3.0578  0.00223
      a1           -1.1108   5.36414  -0.2071  0.83594
      a2            0.4805   5.09018   0.0944  0.92479
      
      Message: Success; Asymptotic SE's
      Function: HAZRATE with 2 expansion(s) of SIMPLE series 
      Strip: 0 [m] to 265 [m] 
      Effective detection radius (EDR): 118.1 [m] 
      Probability of detection: 0.1987
      Scaling: g(0 [m]) = 1
      Log likelihood: -998.7 
      AICc: 2006
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.016 
         Group size range: 1 to 2 
      Density in sampled area: 3.726e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 152966

# hazrate-ContCovarExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ bare +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m")
      Coefficients:
                   Estimate  SE   z    p(>|z|)
      (Intercept)   4.2053   NaN  NaN  NaN    
      bare         -0.4064   NaN  NaN  NaN    
      k             1.4910   NaN  NaN  NaN    
      a1           -3.1577   NaN  NaN  NaN    
      a2           -2.4264   NaN  NaN  NaN    
      
      Message: FAILURE (Exit code= 1, false convergence (8))
      Function: HAZRATE with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 265 [m] 
      Average effective detection radius (EDR): 0.005757 [m] (range 5.606e-05 [m] to 0.04268 [m]) 
      Average probability of detection: 1.338e-09 (range 4.476e-14 to 2.594e-08)
      Scaling: g(0 [m]) = 1
      Log likelihood: -1033 
      AICc: 2076
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.016 
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
                   Estimate  SE   z    p(>|z|)
      (Intercept)   4.2053   NaN  NaN  NaN    
      bare         -0.4064   NaN  NaN  NaN    
      k             1.4910   NaN  NaN  NaN    
      a1           -3.1577   NaN  NaN  NaN    
      a2           -2.4264   NaN  NaN  NaN    
      
      Message: FAILURE (Exit code= 1, false convergence (8))
      Function: HAZRATE with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 265 [m] 
      Average effective detection radius (EDR): 0.005757 [m] (range 5.606e-05 [m] to 0.04268 [m]) 
      Average probability of detection: 1.338e-09 (range 4.476e-14 to 2.594e-08)
      Scaling: g(0 [m]) = 0.75
      Log likelihood: -1033 
      AICc: 2076
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.016 
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
                   Estimate  SE       z      p(>|z|)  
      (Intercept)  4.54      0.04896  92.74  0.000e+00
      k            4.20      0.40236  10.44  1.675e-25
      
      Message: Success; Bootstrap SE's
      Function: HAZRATE  
      Strip: 0 [m] to 265 [m] 
      Effective detection radius (EDR): 118.6 [m] 
      Probability of detection: 0.2004
      Scaling: g(0 [m]) = 1
      Log likelihood: -999 
      AICc: 2002
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.016 
         Group size range: 1 to 2 
      
      Density in sampled area: 3.695e-05 [1/m^2]
                       95% CI: 3.402e-05 [1/m^2] to 4.078e-05 [1/m^2]
      
      Abundance in 4.105e+09 [m^2] study area: 151664
                                       95% CI: 139660 to 167383

