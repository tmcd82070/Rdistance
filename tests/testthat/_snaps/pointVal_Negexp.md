# negexp-MinimumInputs

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~
         groupsize(groupsize), likelihood = lhood)
      Coefficients:
                   Estimate   SE          z          p(>|z|)
      (Intercept)  -3.977216  0.06422121  -61.92994  0      
      
      Message: Success; Asymptotic SE's
      Function: NEGEXP  
      Strip: 0 [m] to 265 [m] 
      Effective detection radius (EDR): 73.88733 [m] 
      Probability of detection: 0.07774066
      Scaling: g(0 [m]) = 1
      Log likelihood: -1017.019 
      AICc: 2036.058
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.015544 
         Group size range: 1 to 2 
      Density in sampled area: 9.523245e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 390929.2

# negexp-NoCovar

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, w.lo = w.lo, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = 0, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                   Estimate   SE          z          p(>|z|)
      (Intercept)  -4.090191  0.08843417  -46.25125  0      
      
      Message: Success; Asymptotic SE's
      Function: NEGEXP  
      Strip: 0 [m] to 200 [m] 
      Effective detection radius (EDR): 77.77105 [m] 
      Probability of detection: 0.1512084
      Scaling: g(0 [m]) = 1
      Log likelihood: -948.4692 
      AICc: 1898.96
      
           Surveyed Units: 120 
         Individuals seen: 188 in 185 groups 
       Average group size: 1.016216 
         Group size range: 1 to 2 
      Density in sampled area: 8.245002e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 338457.3

# negexp-ContinuousCovar

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ bare +
         groupsize(groupsize), likelihood = lhood, w.lo = w.lo, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = 0, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                   Estimate     SE          z          p(>|z|)     
      (Intercept)  -3.23977838  0.67689678  -4.786222  1.699499e-06
      bare         -0.02173948  0.01748816  -1.243097  2.138320e-01
      
      Message: Success; Asymptotic SE's
      Function: NEGEXP  
      Strip: 0 [m] to 200 [m] 
      Average effective detection radius (EDR): 78.06683 [m] (range 65.7791 [m] to 94.12602 [m]) 
      Average probability of detection: 0.1534087 (range 0.1081723 to 0.2214927)
      Scaling: g(0 [m]) = 1
      Log likelihood: -947.6442 
      AICc: 1899.354
      
           Surveyed Units: 120 
         Individuals seen: 188 in 185 groups 
       Average group size: 1.016216 
         Group size range: 1 to 2 
      Density in sampled area: 8.341181e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 342405.5

# negexp-FactorCovar

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ observer +
         groupsize(groupsize), likelihood = lhood, w.lo = w.lo, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = 0, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                    Estimate    SE         z           p(>|z|)     
      (Intercept)   -4.8745891  0.4960066  -9.8276698  8.557588e-23
      observerobs2   0.2779590  0.5806509   0.4787025  6.321503e-01
      observerobs3   0.9470300  0.5578669   1.6975912  8.958495e-02
      observerobs4   1.3022314  0.5093425   2.5566909  1.056731e-02
      observerobs5   0.4984739  0.5809473   0.8580363  3.908724e-01
      observerobs6   1.0658945  0.5430211   1.9628969  4.965816e-02
      
      Message: Success; Asymptotic SE's
      Function: NEGEXP  
      Strip: 0 [m] to 200 [m] 
      Average effective detection radius (EDR): 83.93275 [m] (range 49.73782 [m] to 124.3929 [m]) 
      Average probability of detection: 0.1962442 (range 0.06184627 to 0.3868398)
      Scaling: g(0 [m]) = 1
      Log likelihood: -934.7697 
      AICc: 1882.011
      
           Surveyed Units: 120 
         Individuals seen: 188 in 185 groups 
       Average group size: 1.016216 
         Group size range: 1 to 2 
      Density in sampled area: 0.0001020692 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 418994.2

# negexp-NoCovarWloWhi

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, w.lo = w.20, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = w.20, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                   Estimate   SE          z          p(>|z|)
      (Intercept)  -3.728134  0.07214294  -51.67705  0      
      
      Message: Success; Asymptotic SE's
      Function: NEGEXP  
      Strip: 20 [m] to 200 [m] 
      Effective detection radius (EDR): 56.72558 [m] 
      Probability of detection: 0.09931455
      Scaling: g(20 [m]) = 1
      Log likelihood: -920.3167 
      AICc: 1842.655
      
           Surveyed Units: 120 
         Individuals seen: 187 in 184 groups 
       Average group size: 1.016304 
         Group size range: 1 to 2 
      Density in sampled area: 0.0001541532 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 632799

# negexp-NoCovarWlo

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, w.lo = w.20, expansions =
         0, series = "cosine", x.scl = w.20, g.x.scl = 1, outputUnits = "m")
      Coefficients:
                   Estimate   SE          z          p(>|z|)
      (Intercept)  -3.703936  0.05805447  -63.80104  0      
      
      Message: Success; Asymptotic SE's
      Function: NEGEXP  
      Strip: 20 [m] to 265 [m] 
      Effective detection radius (EDR): 56.94053 [m] 
      Probability of detection: 0.05401456
      Scaling: g(20 [m]) = 1
      Log likelihood: -986.2452 
      AICc: 1974.511
      
           Surveyed Units: 120 
         Individuals seen: 195 in 192 groups 
       Average group size: 1.015625 
         Group size range: 1 to 2 
      Density in sampled area: 0.0001595367 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 654898

# negexp-NoCovarsFt

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, outputUnits = "ft")
      Coefficients:
                   Estimate   SE          z          p(>|z|)
      (Intercept)  -5.165315  0.06422114  -80.43013  0      
      
      Message: Success; Asymptotic SE's
      Function: NEGEXP  
      Strip: 0 [ft] to 869.4226 [ft] 
      Effective detection radius (EDR): 242.4123 [ft] 
      Probability of detection: 0.07774052
      Scaling: g(0 [ft]) = 1
      Log likelihood: -1246.322 
      AICc: 2494.665
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.015544 
         Group size range: 1 to 2 
      Density in sampled area: 8.8474e-06 [1/ft^2]
      Abundance in 44185852261 [ft^2] study area: 390929.9

# negexp-NoCovarCosExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", series = "cosine")
      Coefficients:
                   Estimate    SE           z           p(>|z|)  
      (Intercept)   -3.829968   0.08105552  -47.251168  0.0000000
      a1           -17.701639  17.21474109   -1.028284  0.3038164
      a2           -11.394880  11.25883620   -1.012083  0.3114982
      
      Message: Success; Asymptotic SE's
      Function: NEGEXP with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 265 [m] 
      Effective detection radius (EDR): 263.0207 [m] 
      Probability of detection: 0.9851175
      Scaling: g(0 [m]) = 1
      Log likelihood: -995.4031 
      AICc: 1996.933
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.015544 
         Group size range: 1 to 2 
      Density in sampled area: 7.51528e-06 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 30850.22

# negexp-NoCovarSinExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", series = "sine")
      Coefficients:
                   Estimate    SE         z           p(>|z|)      
      (Intercept)  -4.3935759  0.1614894  -27.206588  5.427883e-163
      a1            1.3703649  0.2544945    5.384654   7.258409e-08
      a2            0.1118909  0.2339864    0.478194   6.325121e-01
      
      Message: Success; Asymptotic SE's
      Function: NEGEXP with 2 expansion(s) of SINE series 
      Strip: 0 [m] to 265 [m] 
      Effective detection radius (EDR): 109.7104 [m] 
      Probability of detection: 0.1713973
      Scaling: g(0 [m]) = 1
      Log likelihood: -1007.448 
      AICc: 2021.023
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.015544 
         Group size range: 1 to 2 
      Density in sampled area: 4.319458e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 177313.8

# negexp-NoCovarHermExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", series = "hermite")
      Coefficients:
                   Estimate       SE   z    p(>|z|)
      (Intercept)  -3.768615e+00  NaN  NaN  NaN    
      a1            8.390403e+04  NaN  NaN  NaN    
      a2           -3.676360e+05  NaN  NaN  NaN    
      
      Message: FAILURE (Exit code= 1, singular convergence (7))
      Function: NEGEXP with 2 expansion(s) of HERMITE series 
      Strip: 0 [m] to 265 [m] 
      Effective detection radius (EDR): 64458.34 [m] 
      Probability of detection: 59165.22 > 1
      Scaling: g(0 [m]) = 1 <- Check scaling
      Log likelihood: -1000.307 
      AICc: 2006.742
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.015544 
         Group size range: 1 to 2 
      Density in sampled area: NA
      Abundance in 4105 [km^2] study area: NA

# negexp-NoCovarSimpExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", series = "simple")
      Coefficients:
                   Estimate   SE         z          p(>|z|)      
      (Intercept)  -4.428895  0.1326999  -33.37528  3.132056e-244
      a1           -4.009018         NA         NA             NA
      a2            3.201568         NA         NA             NA
      
      Message: VARIANCE FAILURE (singular variance-covariance matrix)
      Function: NEGEXP with 2 expansion(s) of SIMPLE series 
      Strip: 0 [m] to 265 [m] 
      Effective detection radius (EDR): 91.22457 [m] 
      Probability of detection: 0.1185037
      Scaling: g(0 [m]) = 1
      Log likelihood: -1008.631 
      AICc: 2023.389
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.015544 
         Group size range: 1 to 2 
      Density in sampled area: 6.247429e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 256457

# negexp-ContCovarExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ bare +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m")
      Coefficients:
                   Estimate      SE           z          p(>|z|)     
      (Intercept)   -2.65180902   0.50053083  -5.297993  1.170822e-07
      bare          -0.02967609   0.01275898  -2.325899  2.002394e-02
      a1           -19.67268543  19.21123318  -1.024020  3.058259e-01
      a2           -11.75122395  11.69919424  -1.004447  3.151631e-01
      
      Message: Success; Asymptotic SE's
      Function: NEGEXP with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 265 [m] 
      Average effective detection radius (EDR): 272.2755 [m] (range 194.051 [m] to 382.3216 [m]) 
      Average probability of detection: 1.083001 97 of 193 P(detect) > 1
      Scaling: g(0 [m]) = 1 <- Check scaling
      Log likelihood: -992.5591 
      AICc: 1993.331
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.015544 
         Group size range: 1 to 2 
      Density in sampled area: 7.554681e-06 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 31011.97

---

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ bare +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", x.scl = xScl, g.x.scl = gXscl)
      Coefficients:
                   Estimate      SE           z          p(>|z|)     
      (Intercept)   -2.65180902   0.50053083  -5.297993  1.170822e-07
      bare          -0.02967609   0.01275898  -2.325899  2.002394e-02
      a1           -19.67268543  19.21123318  -1.024020  3.058259e-01
      a2           -11.75122395  11.69919424  -1.004447  3.151631e-01
      
      Message: Success; Asymptotic SE's
      Function: NEGEXP with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 265 [m] 
      Average effective detection radius (EDR): 272.2755 [m] (range 194.051 [m] to 382.3216 [m]) 
      Average probability of detection: 1.083001 97 of 193 P(detect) > 1
      Scaling: g(0 [m]) = 0.75 <- Check scaling
      Log likelihood: -992.5591 
      AICc: 1993.331
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.015544 
         Group size range: 1 to 2 
      Density in sampled area: 7.554681e-06 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 31011.97

# negexp-Bootstraps

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~
         groupsize(groupsize), likelihood = lhood)
      Coefficients:
                   Estimate   SE          z          p(>|z|)
      (Intercept)  -3.977216  0.05639685  -70.52196  0      
      
      Message: Success; Bootstrap SE's
      Function: NEGEXP  
      Strip: 0 [m] to 265 [m] 
      Effective detection radius (EDR): 73.88733 [m] 
      Probability of detection: 0.07774066
      Scaling: g(0 [m]) = 1
      Log likelihood: -1017.019 
      AICc: 2036.058
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.015544 
         Group size range: 1 to 2 
      
      Density in sampled area: 9.523245e-05 [1/m^2]
                       95% CI: 7.196309e-05 [1/m^2] to 0.0001110873 [1/m^2]
      
      Abundance in 4.105e+09 [m^2] study area: 390929.2
                                       95% CI: 295408.5 to 456013.3

