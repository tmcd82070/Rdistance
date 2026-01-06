# oneStep-MinimumInputs

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~
         groupsize(groupsize), likelihood = lhood)
      Coefficients:
                   Estimate 
      (Intercept)  4.6443909
      p            0.8696644
      
      Message: Success; SE's pending bootstrap
      Function: ONESTEP  
      Strip: 0 [m] to 265 [m] 
      Effective detection radius (EDR): 128.7143 [m] 
      Probability of detection: 0.2359183
      Scaling: g(0 [m]) = 1
      Log likelihood: -1036.345 
      AICc: 2076.753
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.015544 
         Group size range: 1 to 2 
      Density in sampled area: 3.138134e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 128820.4

# oneStep-NoCovar

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, w.lo = w.lo, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = 0, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                   Estimate    
      (Intercept)  2.397895e+00
      p            2.220446e-16
      
      Message: FAILURE (Exit code= -1, p parameter at lower boundary.)
      Function: ONESTEP  
      Strip: 0 [m] to 200 [m] 
      Effective detection radius (EDR): 3233087486 [m] 
      Probability of detection: 2.613214e+14 > 1
      Scaling: g(0 [m]) = 1 <- Check scaling
      Log likelihood: -1013.481 
      AICc: 2031.029
      
           Surveyed Units: 120 
         Individuals seen: 188 in 185 groups 
       Average group size: 1.016216 
         Group size range: 1 to 2 
      Density in sampled area: NA
      Abundance in 4105 [km^2] study area: NA

# oneStep-ContinuousCovar

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ bare +
         groupsize(groupsize), likelihood = lhood, w.lo = w.lo, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = 0, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                   Estimate    
      (Intercept)  2.397895e+00
      bare         2.220446e-16
      p            2.220446e-16
      
      Message: FAILURE (Exit code= -1, p parameter at lower boundary.)
      Function: ONESTEP  
      Strip: 0 [m] to 200 [m] 
      Average effective detection radius (EDR): 3233087486 [m] (range 3233087486 [m] to 3233087486 [m]) 
      Average probability of detection: 2.613214e+14 185 of 185 P(detect) > 1
      Scaling: g(0 [m]) = 1 <- Check scaling
      Log likelihood: -1013.481 
      AICc: 2033.095
      
           Surveyed Units: 120 
         Individuals seen: 188 in 185 groups 
       Average group size: 1.016216 
         Group size range: 1 to 2 
      Density in sampled area: NA
      Abundance in 4105 [km^2] study area: NA

# oneStep-FactorCovar

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ observer +
         groupsize(groupsize), likelihood = lhood, w.lo = w.lo, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = 0, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                    Estimate    
      (Intercept)   2.393989e+00
      observerobs2  1.216929e+00
      observerobs3  9.733068e-01
      observerobs4  9.018478e-01
      observerobs5  3.906246e-03
      observerobs6  1.319583e+00
      p             2.220446e-16
      
      Message: FAILURE (Exit code= -1, p parameter at lower boundary.)
      Function: ONESTEP  
      Strip: 0 [m] to 200 [m] 
      Average effective detection radius (EDR): 5051287583 [m] (range 3226451099 [m] to 6670837121 [m]) 
      Average probability of detection: 6.786015e+14 185 of 185 P(detect) > 1
      Scaling: g(0 [m]) = 1 <- Check scaling
      Log likelihood: -1010.28 
      AICc: 2035.192
      
           Surveyed Units: 120 
         Individuals seen: 188 in 185 groups 
       Average group size: 1.016216 
         Group size range: 1 to 2 
      Density in sampled area: NA
      Abundance in 4105 [km^2] study area: NA

# oneStep-NoCovarWloWhi

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, w.lo = w.20, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = w.20, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                   Estimate 
      (Intercept)  4.3694478
      p            0.8600841
      
      Message: Success; SE's pending bootstrap
      Function: ONESTEP  
      Strip: 20 [m] to 200 [m] 
      Effective detection radius (EDR): 97.82399 [m] 
      Probability of detection: 0.295356
      Scaling: g(20 [m]) = 1
      Log likelihood: -933.751 
      AICc: 1871.568
      
           Surveyed Units: 120 
         Individuals seen: 187 in 184 groups 
       Average group size: 1.016304 
         Group size range: 1 to 2 
      Density in sampled area: 5.18346e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 212781

# oneStep-NoCovarWlo

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, w.lo = w.20, expansions =
         0, series = "cosine", x.scl = w.20, g.x.scl = 1, outputUnits = "m")
      Coefficients:
                   Estimate 
      (Intercept)  4.4067192
      p            0.8766756
      
      Message: Success; SE's pending bootstrap
      Function: ONESTEP  
      Strip: 20 [m] to 265 [m] 
      Effective detection radius (EDR): 102.45 [m] 
      Probability of detection: 0.1748605
      Scaling: g(20 [m]) = 1
      Log likelihood: -1027.572 
      AICc: 2059.208
      
           Surveyed Units: 120 
         Individuals seen: 195 in 192 groups 
       Average group size: 1.015625 
         Group size range: 1 to 2 
      Density in sampled area: 4.928102e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 202298.6

# oneStep-NoCovarsFt

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, outputUnits = "ft")
      Coefficients:
                   Estimate 
      (Intercept)  5.8324904
      p            0.8696644
      
      Message: Success; SE's pending bootstrap
      Function: ONESTEP  
      Strip: 0 [ft] to 869.4226 [ft] 
      Effective detection radius (EDR): 422.2909 [ft] 
      Probability of detection: 0.2359183
      Scaling: g(0 [ft]) = 1
      Log likelihood: -1265.648 
      AICc: 2535.359
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.015544 
         Group size range: 1 to 2 
      Density in sampled area: 2.915422e-06 [1/ft^2]
      Abundance in 44185852261 [ft^2] study area: 128820.4

# oneStep-NoCovarCosExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", series = "cosine")
      Coefficients:
                   Estimate  
      (Intercept)   5.0039463
      p             0.8899748
      a1           -3.6473018
      a2            1.0306444
      
      Message: Success; SE's pending bootstrap
      Function: ONESTEP with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 265 [m] 
      Effective detection radius (EDR): 242.6382 [m] 
      Probability of detection: 0.8383521
      Scaling: g(0 [m]) = 1
      Log likelihood: -1002.367 
      AICc: 2012.946
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.015544 
         Group size range: 1 to 2 
      Density in sampled area: 8.830936e-06 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 36250.99

# oneStep-NoCovarSinExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", series = "sine")
      Coefficients:
                   Estimate 
      (Intercept)  5.0039463
      p            0.9588960
      a1           1.1306992
      a2           0.2472177
      
      Message: Success; SE's pending bootstrap
      Function: ONESTEP with 2 expansion(s) of SINE series 
      Strip: 0 [m] to 265 [m] 
      Effective detection radius (EDR): 142.8757 [m] 
      Probability of detection: 0.2906867
      Scaling: g(0 [m]) = 1
      Log likelihood: -1015.083 
      AICc: 2038.378
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.015544 
         Group size range: 1 to 2 
      Density in sampled area: 2.546878e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 104549.3

# oneStep-NoCovarHermExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", series = "hermite")
      Coefficients:
                   Estimate   
      (Intercept)   5.00394631
      p             0.96598983
      a1           -0.21441975
      a2           -0.08594117
      
      Message: Success; SE's pending bootstrap
      Function: ONESTEP with 2 expansion(s) of HERMITE series 
      Strip: 0 [m] to 265 [m] 
      Effective detection radius (EDR): 129.4852 [m] 
      Probability of detection: 0.2387528
      Scaling: g(0 [m]) = 1
      Log likelihood: -1009.136 
      AICc: 2026.485
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.015544 
         Group size range: 1 to 2 
      Density in sampled area: 3.100879e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 127291.1

# oneStep-NoCovarSimpExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", series = "simple")
      Coefficients:
                   Estimate  
      (Intercept)   5.0039463
      p             0.9720374
      a1           -3.5092186
      a2            2.8274578
      
      Message: Success; SE's pending bootstrap
      Function: ONESTEP with 2 expansion(s) of SIMPLE series 
      Strip: 0 [m] to 265 [m] 
      Effective detection radius (EDR): 117.0441 [m] 
      Probability of detection: 0.1950776
      Scaling: g(0 [m]) = 1
      Log likelihood: -1004.885 
      AICc: 2017.983
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.015544 
         Group size range: 1 to 2 
      Density in sampled area: 3.795122e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 155789.8

# oneStep-ContCovarExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ bare +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m")
      Coefficients:
                   Estimate     
      (Intercept)   5.003946e+00
      bare          2.220446e-16
      p             8.899748e-01
      a1           -3.647303e+00
      a2            1.030644e+00
      
      Message: Success; SE's pending bootstrap
      Function: ONESTEP with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 265 [m] 
      Average effective detection radius (EDR): 242.6382 [m] (range 242.6382 [m] to 242.6382 [m]) 
      Average probability of detection: 0.8383524 (range 0.8383524 to 0.8383524)
      Scaling: g(0 [m]) = 1
      Log likelihood: -1002.367 
      AICc: 2015.054
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.015544 
         Group size range: 1 to 2 
      Density in sampled area: 8.830933e-06 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 36250.98

---

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ bare +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", x.scl = xScl, g.x.scl = gXscl)
      Coefficients:
                   Estimate     
      (Intercept)   5.003946e+00
      bare          2.220446e-16
      p             8.899748e-01
      a1           -3.647303e+00
      a2            1.030644e+00
      
      Message: Success; SE's pending bootstrap
      Function: ONESTEP with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 265 [m] 
      Average effective detection radius (EDR): 242.6382 [m] (range 242.6382 [m] to 242.6382 [m]) 
      Average probability of detection: 0.8383524 (range 0.8383524 to 0.8383524)
      Scaling: g(0 [m]) = 0.75
      Log likelihood: -1002.367 
      AICc: 2015.054
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.015544 
         Group size range: 1 to 2 
      Density in sampled area: 8.830933e-06 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 36250.98

# oneStep-Bootstraps

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~
         groupsize(groupsize), likelihood = lhood)
      Coefficients:
                   Estimate   SE         z         p(>|z|)
      (Intercept)  4.6443909  0.1008867  46.03569  0      
      p            0.8696644  0.0223626  38.88923  0      
      
      Message: Success; Bootstrap SE's
      Function: ONESTEP  
      Strip: 0 [m] to 265 [m] 
      Effective detection radius (EDR): 128.7143 [m] 
      Probability of detection: 0.2359183
      Scaling: g(0 [m]) = 1
      Log likelihood: -1036.345 
      AICc: 2076.753
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.015544 
         Group size range: 1 to 2 
      
      Density in sampled area: 3.138134e-05 [1/m^2]
                       95% CI: 2.804612e-05 [1/m^2] to 3.344265e-05 [1/m^2]
      
      Abundance in 4.105e+09 [m^2] study area: 128820.4
                                       95% CI: 115129.3 to 137282.1

