# triangle-MinimumInputs

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~
         groupsize(groupsize), likelihood = lhood)
      Coefficients:
                   Estimate
      (Intercept)  5.07113 
      p            0.02693 
      
      Message: Success; SE's pending bootstrap
      Function: TRIANGLE  
      Strip: 0 [m] to 265 [m] 
      Effective detection radius (EDR): 100.6 [m] 
      Probability of detection: 0.1442
      Scaling: g(0 [m]) = 1
      Log likelihood: -1011 
      AICc: 2026
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.016 
         Group size range: 1 to 2 
      Density in sampled area: 5.133e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 210724

# triangle-NoCovar

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, w.lo = w.lo, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = 0, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                   Estimate
      (Intercept)  3.434   
      p            4.296   
      
      Message: FAILURE (Exit code= -1, p parameter at upper boundary.)
      Function: TRIANGLE  
      Strip: 0 [m] to 200 [m] 
      Effective detection radius (EDR): 413.2 [m] 
      Probability of detection: 4.269 > 1
      Scaling: g(0 [m]) = 1 <- Check scaling
      Log likelihood: -1014 
      AICc: 2032
      
           Surveyed Units: 120 
         Individuals seen: 188 in 185 groups 
       Average group size: 1.016 
         Group size range: 1 to 2 
      Density in sampled area: NA
      Abundance in 4105 [km^2] study area: NA

# triangle-ContinuousCovar

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ bare +
         groupsize(groupsize), likelihood = lhood, w.lo = w.lo, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = 0, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                   Estimate
      (Intercept)  4.911120
      bare         0.003643
      p            0.051566
      
      Message: Success; SE's pending bootstrap
      Function: TRIANGLE  
      Strip: 0 [m] to 200 [m] 
      Average effective detection radius (EDR): 99.13 [m] (range 96.41 [m] to 102.7 [m]) 
      Average probability of detection: 0.2457 (range 0.2324 to 0.2637)
      Scaling: g(0 [m]) = 1
      Log likelihood: -938.6 
      AICc: 1883
      
           Surveyed Units: 120 
         Individuals seen: 188 in 185 groups 
       Average group size: 1.016 
         Group size range: 1 to 2 
      Density in sampled area: 5.077e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 208399

# triangle-FactorCovar

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ observer +
         groupsize(groupsize), likelihood = lhood, w.lo = w.lo, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = 0, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                    Estimate
      (Intercept)   -3.90738
      observerobs2  -3.15011
      observerobs3  -4.51443
      observerobs4   0.53759
      observerobs5   6.72052
      observerobs6   3.00600
      p              0.06822
      
      Message: Success; SE's pending bootstrap
      Function: TRIANGLE  
      Strip: 0 [m] to 200 [m] 
      Average effective detection radius (EDR): 52.36 [m] (range 52.24 [m] to 53.06 [m]) 
      Average probability of detection: 0.06853 (range 0.06822 to 0.07038)
      Scaling: g(0 [m]) = 1
      Log likelihood: -1013 
      AICc: 2041
      
           Surveyed Units: 120 
         Individuals seen: 188 in 185 groups 
       Average group size: 1.016 
         Group size range: 1 to 2 
      Density in sampled area: 0.0001819 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 746759

# triangle-NoCovarWloWhi

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, w.lo = w.20, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = w.20, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                   Estimate
      (Intercept)  4.87368 
      p            0.04122 
      
      Message: Success; SE's pending bootstrap
      Function: TRIANGLE  
      Strip: 20 [m] to 200 [m] 
      Effective detection radius (EDR): 82.48 [m] 
      Probability of detection: 0.21
      Scaling: g(20 [m]) = 1
      Log likelihood: -921.7 
      AICc: 1847
      
           Surveyed Units: 120 
         Individuals seen: 187 in 184 groups 
       Average group size: 1.016 
         Group size range: 1 to 2 
      Density in sampled area: 7.291e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 299296

# triangle-NoCovarWlo

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, w.lo = w.20, expansions =
         0, series = "cosine", x.scl = w.20, g.x.scl = 1, outputUnits = "m")
      Coefficients:
                   Estimate
      (Intercept)   5.4643 
      p            -0.4484 
      
      Message: FAILURE (Exit code= 10, )
      Function: TRIANGLE  
      Strip: 20 [m] to 265 [m] 
      Effective detection radius (EDR): 1.07 [m] 
      Probability of detection: 1.907e-05
      Scaling: g(20 [m]) = 1
      Log likelihood: 5997 
      AICc: -11991
      
           Surveyed Units: 120 
         Individuals seen: 195 in 192 groups 
       Average group size: 1.016 
         Group size range: 1 to 2 
      Density in sampled area: NA
      Abundance in 4105 [km^2] study area: NA

# triangle-NoCovarsFt

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, outputUnits = "ft")
      Coefficients:
                   Estimate
      (Intercept)  6.43337 
      p            0.01765 
      
      Message: Success; SE's pending bootstrap
      Function: TRIANGLE  
      Strip: 0 [ft] to 869.4 [ft] 
      Effective detection radius (EDR): 374.4 [ft] 
      Probability of detection: 0.1854
      Scaling: g(0 [ft]) = 1
      Log likelihood: -1240 
      AICc: 2485
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.016 
         Group size range: 1 to 2 
      Density in sampled area: 3.71e-06 [1/ft^2]
      Abundance in 4.419e+10 [ft^2] study area: 163918

# triangle-NoCovarCosExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", series = "cosine")
      Coefficients:
                   Estimate
      (Intercept)   5.11159
      p             0.09139
      a1           -3.00411
      a2           -0.12052
      
      Message: Success; SE's pending bootstrap
      Function: TRIANGLE with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 265 [m] 
      Effective detection radius (EDR): 188.6 [m] 
      Probability of detection: 0.5064
      Scaling: g(0 [m]) = 1
      Log likelihood: -1004 
      AICc: 2016
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.016 
         Group size range: 1 to 2 
      Density in sampled area: 1.462e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 60013

# triangle-NoCovarSinExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", series = "sine")
      Coefficients:
                   Estimate
      (Intercept)   5.27897
      p             0.01715
      a1            0.66759
      a2           -0.43614
      
      Message: Success; SE's pending bootstrap
      Function: TRIANGLE with 2 expansion(s) of SINE series 
      Strip: 0 [m] to 265 [m] 
      Effective detection radius (EDR): 117.3 [m] 
      Probability of detection: 0.196
      Scaling: g(0 [m]) = 1
      Log likelihood: -999.6 
      AICc: 2007
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.016 
         Group size range: 1 to 2 
      Density in sampled area: 3.777e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 155048

# triangle-NoCovarHermExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", series = "hermite")
      Coefficients:
                   Estimate
      (Intercept)   5.0058 
      p             0.1920 
      a1            0.4589 
      a2           -1.3288 
      
      Message: Success; SE's pending bootstrap
      Function: TRIANGLE with 2 expansion(s) of HERMITE series 
      Strip: 0 [m] to 265 [m] 
      Effective detection radius (EDR): 266.3 [m] 
      Probability of detection: 1.01 > 1
      Scaling: g(0 [m]) = 1 <- Check scaling
      Log likelihood: -1004 
      AICc: 2017
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.016 
         Group size range: 1 to 2 
      Density in sampled area: 7.331e-06 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 30093

# triangle-NoCovarSimpExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", series = "simple")
      Coefficients:
                   Estimate
      (Intercept)   5.08544
      p             0.02714
      a1           -0.11586
      a2           -0.22096
      
      Message: Success; SE's pending bootstrap
      Function: TRIANGLE with 2 expansion(s) of SIMPLE series 
      Strip: 0 [m] to 265 [m] 
      Effective detection radius (EDR): 100.1 [m] 
      Probability of detection: 0.1425
      Scaling: g(0 [m]) = 1
      Log likelihood: -1011 
      AICc: 2030
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.016 
         Group size range: 1 to 2 
      Density in sampled area: 5.194e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 213206

# triangle-ContCovarExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ bare +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m")
      Coefficients:
                   Estimate
      (Intercept)   5.56510
      bare         -0.01307
      p             0.01906
      a1           -0.05885
      a2            0.62259
      
      Message: FAILURE (Exit code= 1, )
      Function: TRIANGLE with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 265 [m] 
      Average effective detection radius (EDR): 82.66 [m] (range 72.99 [m] to 91.68 [m]) 
      Average probability of detection: 0.09758 (range 0.07586 to 0.1197)
      Scaling: g(0 [m]) = 1
      Log likelihood: -1004 
      AICc: 2019
      
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
                   Estimate
      (Intercept)   5.56510
      bare         -0.01307
      p             0.01906
      a1           -0.05885
      a2            0.62259
      
      Message: FAILURE (Exit code= 1, )
      Function: TRIANGLE with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 265 [m] 
      Average effective detection radius (EDR): 82.66 [m] (range 72.99 [m] to 91.68 [m]) 
      Average probability of detection: 0.09758 (range 0.07586 to 0.1197)
      Scaling: g(0 [m]) = 0.75
      Log likelihood: -1004 
      AICc: 2019
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.016 
         Group size range: 1 to 2 
      Density in sampled area: NA
      Abundance in 4105 [km^2] study area: NA

# triangle-Bootstraps

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~
         groupsize(groupsize), likelihood = lhood)
      Coefficients:
                   Estimate  SE      z        p(>|z|)  
      (Intercept)  5.07113   0.2790  18.1731  8.426e-74
      p            0.02693   0.2267   0.1188  9.054e-01
      
      Message: Success; Bootstrap SE's
      Function: TRIANGLE  
      Strip: 0 [m] to 265 [m] 
      Effective detection radius (EDR): 100.6 [m] 
      Probability of detection: 0.1442
      Scaling: g(0 [m]) = 1
      Log likelihood: -1011 
      AICc: 2026
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.016 
         Group size range: 1 to 2 
      
      Density in sampled area: 5.133e-05 [1/m^2]
                       95% CI: 4.676e-05 [1/m^2] to 5.899e-05 [1/m^2]
      
      Abundance in 4.105e+09 [m^2] study area: 210724
                                       95% CI: 191970 to 242134
      CI based on 11 of 20 successful bootstrap iterations
    Condition
      Warning in `summary.abund()`:
      The proportion of non-convergent bootstrap iterations is high.
    Output
      The proportion of non-convergent bootstrap iterations exceeds 0.2.
      It would be good to figure out why this happened (low detections, unstable dfunc form, etc.),
      inspect the $B component of the abundance object (e.g., hist(object$B$density)), and decide whether the bootstrap CI is valid.

