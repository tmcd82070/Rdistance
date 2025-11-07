# hazrate-MinimumInputs

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~
         groupsize(groupsize), likelihood = lhood)
      Coefficients:
                   Estimate  SE          z         p(>|z|)     
      (Intercept)  4.540434  0.06265929  72.46226  0.000000e+00
      k            4.199565  0.39721233  10.57259  3.992942e-26
      
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
      Density in sampled area: 3.694619e-05 [1/m^2]
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
      (Intercept)  4.526813  0.06486727  69.785781  0.000000e+00
      k            3.963643  0.47089290   8.417291  3.852887e-17
      
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
      (Intercept)  3.90756438  0.370783705  10.538663  5.730803e-26
      bare         0.01605971  0.009417635   1.705280  8.814214e-02
      k            4.06654774  0.484369793   8.395544  4.637434e-17
      
      Message: Success; Asymptotic SE's
      Function: HAZRATE  
      Strip: 0 [m] to 200 [m] 
      Average effective detection radius (EDR): 116.4438 [m] (range 101.2567 [m] to 136.9473 [m]) 
      Average probability of detection: 0.3406241 (range 0.2563231 to 0.4688642)
      Scaling: g(0 [m]) = 1
      Log likelihood: -931.5143 
      AICc: 1869.161
      
           Surveyed Units: 120 
         Individuals seen: 188 in 185 groups 
       Average group size: 1.016216 
         Group size range: 1 to 2 
      Density in sampled area: 3.727297e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 153005.5

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
      (Intercept)    5.05175691  0.1220400  41.3942751  0.000000e+00
      observerobs2  -0.08950643  0.1299712  -0.6886639  4.910348e-01
      observerobs3  -0.39227291  0.1590877  -2.4657647  1.367211e-02
      observerobs4  -0.65858963  0.1242175  -5.3019086  1.145982e-07
      observerobs5  -0.06682980  0.1449110  -0.4611782  6.446708e-01
      observerobs6  -0.48600149  0.1368074  -3.5524504  3.816610e-04
      k              7.29081879  1.6934871   4.3052107  1.668269e-05
      
      Message: Success; Asymptotic SE's
      Function: HAZRATE  
      Strip: 0 [m] to 200 [m] 
      Average effective detection radius (EDR): 131.5897 [m] (range 90.66239 [m] to 168.3413 [m]) 
      Average probability of detection: 0.4579863 (range 0.2054917 to 0.7084701)
      Scaling: g(0 [m]) = 1
      Log likelihood: -909.6747 
      AICc: 1833.982
      
           Surveyed Units: 120 
         Individuals seen: 188 in 185 groups 
       Average group size: 1.016216 
         Group size range: 1 to 2 
      Density in sampled area: 3.482608e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 142961.1

# hazrate-NoCovarWloWhi

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, w.lo = w.20, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = w.20, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                   Estimate  SE          z         p(>|z|)     
      (Intercept)  4.085785  0.08872294  46.05105  0.000000e+00
      k            3.174197  0.32400943   9.79662  1.164158e-22
      
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
      k            3.421335  0.28021379  12.20973  2.758045e-34
      
      Message: Success; Asymptotic SE's
      Function: HAZRATE  
      Strip: 20 [m] to 265 [m] 
      Effective detection radius (EDR): 85.45974 [m] 
      Probability of detection: 0.1216721
      Scaling: g(20 [m]) = 1
      Log likelihood: -981.628 
      AICc: 1967.32
      
           Surveyed Units: 120 
         Individuals seen: 195 in 192 groups 
       Average group size: 1.015625 
         Group size range: 1 to 2 
      Density in sampled area: 7.082398e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 290732.4

# hazrate-NoCovarsFt

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, outputUnits = "ft")
      Coefficients:
                   Estimate  SE          z         p(>|z|)     
      (Intercept)  5.728533  0.06267096  91.40651  0.000000e+00
      k            4.199565  0.39728670  10.57061  4.078138e-26
      
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
      (Intercept)   4.326690  0.1131519  38.2378889  6.611586e-320
      k             3.491205  0.3250279  10.7412475   6.515790e-27
      a1           -8.216870  8.0996212  -1.0144758   3.103558e-01
      a2           -3.766640  3.8275114  -0.9840964   3.250681e-01
      
      Message: Success; Asymptotic SE's
      Function: HAZRATE with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 265 [m] 
      Effective detection radius (EDR): 289.2672 [m] 
      Probability of detection: 1.191534 > 1
      Scaling: g(0 [m]) = 1 <- Check scaling
      Log likelihood: -990.9346 
      AICc: 1990.082
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.015544 
         Group size range: 1 to 2 
      Density in sampled area: 6.213361e-06 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 25505.85

# hazrate-NoCovarSinExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", series = "sine")
      Coefficients:
                   Estimate   SE          z           p(>|z|)     
      (Intercept)  4.5975100  0.09410681  48.8541690  0.000000e+00
      k            3.7588444  0.62775706   5.9877373  2.127804e-09
      a1           0.6502634  0.54229310   1.1990996  2.304892e-01
      a2           0.1981852  0.31683701   0.6255116  5.316353e-01
      
      Message: Success; Asymptotic SE's
      Function: HAZRATE with 2 expansion(s) of SINE series 
      Strip: 0 [m] to 265 [m] 
      Effective detection radius (EDR): 136.4301 [m] 
      Probability of detection: 0.2650503
      Scaling: g(0 [m]) = 1
      Log likelihood: -998.4344 
      AICc: 2005.082
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.015544 
         Group size range: 1 to 2 
      Density in sampled area: 2.793218e-05 [1/m^2]
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
      (Intercept)       4.341265  NaN  NaN  NaN    
      k                 3.770231  NaN  NaN  NaN    
      a1             5693.963672  NaN  NaN  NaN    
      a2           -29383.112005  NaN  NaN  NaN    
      
      Message: FAILURE (Exit code= 1, singular convergence (7))
      Function: HAZRATE with 2 expansion(s) of HERMITE series 
      Strip: 0 [m] to 265 [m] 
      Effective detection radius (EDR): 31136.37 [m] 
      Probability of detection: 13805.25 > 1
      Scaling: g(0 [m]) = 1 <- Check scaling
      Log likelihood: -992.1898 
      AICc: 1992.592
      
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
      (Intercept)   4.5329137  0.06834056  66.32830429  0.000000000
      k             3.7422318  1.22375458   3.05799209  0.002228255
      a1           -1.1113297  5.36310673  -0.20721753  0.835839979
      a2            0.4809642  5.08923811   0.09450614  0.924707105
      
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
      Density in sampled area: 3.726327e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 152965.7

# hazrate-ContCovarExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ bare +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m")
      Coefficients:
                   Estimate    SE   z    p(>|z|)
      (Intercept)   4.2190366  NaN  NaN  NaN    
      bare         -0.4934805  NaN  NaN  NaN    
      k             1.5182022  NaN  NaN  NaN    
      a1           -0.6417786  NaN  NaN  NaN    
      a2           -0.6809343  NaN  NaN  NaN    
      
      Message: FAILURE (Exit code= 1, false convergence (8))
      Function: HAZRATE with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 265 [m] 
      Average effective detection radius (EDR): 0.0003595251 [m] (range 3.950473e-06 [m] to 0.003531652 [m]) 
      Average probability of detection: 6.740652e-12 (range 2.222319e-16 to 1.776086e-10)
      Scaling: g(0 [m]) = 1
      Log likelihood: -1057.323 
      AICc: 2124.966
      
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
      (Intercept)   4.2190366  NaN  NaN  NaN    
      bare         -0.4934805  NaN  NaN  NaN    
      k             1.5182022  NaN  NaN  NaN    
      a1           -0.6417786  NaN  NaN  NaN    
      a2           -0.6809343  NaN  NaN  NaN    
      
      Message: FAILURE (Exit code= 1, false convergence (8))
      Function: HAZRATE with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 265 [m] 
      Average effective detection radius (EDR): 0.0003595251 [m] (range 3.950473e-06 [m] to 0.003531652 [m]) 
      Average probability of detection: 6.740652e-12 (range 2.222319e-16 to 1.776086e-10)
      Scaling: g(0 [m]) = 0.75
      Log likelihood: -1057.323 
      AICc: 2124.966
      
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
      (Intercept)  4.540434  0.06397395  70.973169  0.00000e+00
      k            4.199565  0.49498325   8.484256  2.17103e-17
      
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
      
      Density in sampled area: 3.694619e-05 [1/m^2]
                       95% CI: 3.321246e-05 [1/m^2] to 4.140141e-05 [1/m^2]
      
      Abundance in 4.105e+09 [m^2] study area: 151664.1
                                       95% CI: 136337.1 to 169952.8

