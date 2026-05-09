# negexp-MinimumInputs

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~
         groupsize(groupsize), likelihood = lhood)
      Coefficients:
                   Estimate  SE       z       p(>|z|)
      (Intercept)  -3.977    0.06422  -61.93  0      
      
      Message: Success; Asymptotic SE's
      Function: NEGEXP  
      Strip: 0 [m] to 265 [m] 
      Effective detection radius (EDR): 73.89 [m] 
      Probability of detection: 0.07774
      Scaling: g(0 [m]) = 1
      Log likelihood: -1017 
      AICc: 2036
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.016 
         Group size range: 1 to 2 
      Density in sampled area: 9.523e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 390929

# negexp-NoCovar

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, w.lo = w.lo, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = 0, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                   Estimate  SE       z       p(>|z|)
      (Intercept)  -4.09     0.08843  -46.25  0      
      
      Message: Success; Asymptotic SE's
      Function: NEGEXP  
      Strip: 0 [m] to 200 [m] 
      Effective detection radius (EDR): 77.77 [m] 
      Probability of detection: 0.1512
      Scaling: g(0 [m]) = 1
      Log likelihood: -948.5 
      AICc: 1899
      
           Surveyed Units: 120 
         Individuals seen: 188 in 185 groups 
       Average group size: 1.016 
         Group size range: 1 to 2 
      Density in sampled area: 8.245e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 338457

# negexp-ContinuousCovar

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ bare +
         groupsize(groupsize), likelihood = lhood, w.lo = w.lo, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = 0, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                   Estimate  SE       z       p(>|z|)  
      (Intercept)  -3.23978  0.67690  -4.786  1.699e-06
      bare         -0.02174  0.01749  -1.243  2.138e-01
      
      Message: Success; Asymptotic SE's
      Function: NEGEXP  
      Strip: 0 [m] to 200 [m] 
      Average effective detection radius (EDR): 78.07 [m] (range 65.78 [m] to 94.13 [m]) 
      Average probability of detection: 0.1534 (range 0.1082 to 0.2215)
      Scaling: g(0 [m]) = 1
      Log likelihood: -947.6 
      AICc: 1899
      
           Surveyed Units: 120 
         Individuals seen: 188 in 185 groups 
       Average group size: 1.016 
         Group size range: 1 to 2 
      Density in sampled area: 8.341e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 342405

# negexp-FactorCovar

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ observer +
         groupsize(groupsize), likelihood = lhood, w.lo = w.lo, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = 0, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                    Estimate  SE      z        p(>|z|)  
      (Intercept)   -4.8746   0.4960  -9.8277  8.558e-23
      observerobs2   0.2780   0.5807   0.4787  6.322e-01
      observerobs3   0.9470   0.5579   1.6976  8.958e-02
      observerobs4   1.3022   0.5093   2.5567  1.057e-02
      observerobs5   0.4985   0.5809   0.8580  3.909e-01
      observerobs6   1.0659   0.5430   1.9629  4.966e-02
      
      Message: Success; Asymptotic SE's
      Function: NEGEXP  
      Strip: 0 [m] to 200 [m] 
      Average effective detection radius (EDR): 83.93 [m] (range 49.74 [m] to 124.4 [m]) 
      Average probability of detection: 0.1962 (range 0.06185 to 0.3868)
      Scaling: g(0 [m]) = 1
      Log likelihood: -934.8 
      AICc: 1882
      
           Surveyed Units: 120 
         Individuals seen: 188 in 185 groups 
       Average group size: 1.016 
         Group size range: 1 to 2 
      Density in sampled area: 0.0001021 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 418994

# negexp-NoCovarWloWhi

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, w.lo = w.20, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = w.20, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                   Estimate  SE       z       p(>|z|)
      (Intercept)  -3.728    0.07214  -51.68  0      
      
      Message: Success; Asymptotic SE's
      Function: NEGEXP  
      Strip: 20 [m] to 200 [m] 
      Effective detection radius (EDR): 56.73 [m] 
      Probability of detection: 0.09931
      Scaling: g(20 [m]) = 1
      Log likelihood: -920.3 
      AICc: 1843
      
           Surveyed Units: 120 
         Individuals seen: 187 in 184 groups 
       Average group size: 1.016 
         Group size range: 1 to 2 
      Density in sampled area: 0.0001542 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 632799

# negexp-NoCovarWlo

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, w.lo = w.20, expansions =
         0, series = "cosine", x.scl = w.20, g.x.scl = 1, outputUnits = "m")
      Coefficients:
                   Estimate  SE       z      p(>|z|)
      (Intercept)  -3.704    0.05805  -63.8  0      
      
      Message: Success; Asymptotic SE's
      Function: NEGEXP  
      Strip: 20 [m] to 265 [m] 
      Effective detection radius (EDR): 56.94 [m] 
      Probability of detection: 0.05401
      Scaling: g(20 [m]) = 1
      Log likelihood: -986.2 
      AICc: 1975
      
           Surveyed Units: 120 
         Individuals seen: 195 in 192 groups 
       Average group size: 1.016 
         Group size range: 1 to 2 
      Density in sampled area: 0.0001595 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 654898

# negexp-NoCovarsFt

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, outputUnits = "ft")
      Coefficients:
                   Estimate  SE       z       p(>|z|)
      (Intercept)  -5.165    0.06422  -80.43  0      
      
      Message: Success; Asymptotic SE's
      Function: NEGEXP  
      Strip: 0 [ft] to 869.4 [ft] 
      Effective detection radius (EDR): 242.4 [ft] 
      Probability of detection: 0.07774
      Scaling: g(0 [ft]) = 1
      Log likelihood: -1246 
      AICc: 2495
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.016 
         Group size range: 1 to 2 
      Density in sampled area: 8.847e-06 [1/ft^2]
      Abundance in 4.419e+10 [ft^2] study area: 390930

# negexp-NoCovarCosExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", series = "cosine")
      Coefficients:
                   Estimate  SE        z        p(>|z|)
      (Intercept)   -3.83     0.08106  -47.251  0.0000 
      a1           -17.70    17.21482   -1.028  0.3038 
      a2           -11.39    11.25890   -1.012  0.3115 
      
      Message: Success; Asymptotic SE's
      Function: NEGEXP with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 265 [m] 
      Effective detection radius (EDR): 263 [m] 
      Probability of detection: 0.9851
      Scaling: g(0 [m]) = 1
      Log likelihood: -995.4 
      AICc: 1997
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.016 
         Group size range: 1 to 2 
      Density in sampled area: 7.515e-06 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 30850

# negexp-NoCovarSinExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", series = "sine")
      Coefficients:
                   Estimate  SE      z         p(>|z|)   
      (Intercept)  -4.3936   0.1615  -27.2065  5.436e-163
      a1            1.3704   0.2545    5.3846   7.259e-08
      a2            0.1119   0.2340    0.4782   6.325e-01
      
      Message: Success; Asymptotic SE's
      Function: NEGEXP with 2 expansion(s) of SINE series 
      Strip: 0 [m] to 265 [m] 
      Effective detection radius (EDR): 109.7 [m] 
      Probability of detection: 0.1714
      Scaling: g(0 [m]) = 1
      Log likelihood: -1007 
      AICc: 2021
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.016 
         Group size range: 1 to 2 
      Density in sampled area: 4.319e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 177314

# negexp-NoCovarHermExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", series = "hermite")
      Coefficients:
                   Estimate    SE   z    p(>|z|)
      (Intercept)  -3.768e+00  NaN  NaN  NaN    
      a1            2.852e+04  NaN  NaN  NaN    
      a2           -1.250e+05  NaN  NaN  NaN    
      
      Message: FAILURE (Exit code= 1, singular convergence (7))
      Function: NEGEXP with 2 expansion(s) of HERMITE series 
      Strip: 0 [m] to 265 [m] 
      Effective detection radius (EDR): 37569 [m] 
      Probability of detection: 20099 > 1
      Scaling: g(0 [m]) = 1 <- Check scaling
      Log likelihood: -1000 
      AICc: 2007
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.016 
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
                   Estimate  SE      z       p(>|z|)   
      (Intercept)  -4.429    0.1327  -33.38  3.132e-244
      a1           -4.009        NA      NA          NA
      a2            3.202        NA      NA          NA
      
      Message: VARIANCE FAILURE (singular variance-covariance matrix)
      Function: NEGEXP with 2 expansion(s) of SIMPLE series 
      Strip: 0 [m] to 265 [m] 
      Effective detection radius (EDR): 91.22 [m] 
      Probability of detection: 0.1185
      Scaling: g(0 [m]) = 1
      Log likelihood: -1009 
      AICc: 2023
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.016 
         Group size range: 1 to 2 
      Density in sampled area: 6.247e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 256457

# negexp-ContCovarExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ bare +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m")
      Coefficients:
                   Estimate   SE        z       p(>|z|)  
      (Intercept)   -2.65181   0.50053  -5.298  1.171e-07
      bare          -0.02968   0.01276  -2.326  2.002e-02
      a1           -19.67333  19.21244  -1.024  3.058e-01
      a2           -11.75180  11.70022  -1.004  3.152e-01
      
      Message: Success; Asymptotic SE's
      Function: NEGEXP with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 265 [m] 
      Average effective detection radius (EDR): 272.3 [m] (range 194.1 [m] to 382.3 [m]) 
      Average probability of detection: 1.083 97 of 193 P(detect) > 1
      Scaling: g(0 [m]) = 1 <- Check scaling
      Log likelihood: -992.6 
      AICc: 1993
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.016 
         Group size range: 1 to 2 
      Density in sampled area: 7.554e-06 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 31011

---

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ bare +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", x.scl = xScl, g.x.scl = gXscl)
      Coefficients:
                   Estimate   SE        z       p(>|z|)  
      (Intercept)   -2.65181   0.50053  -5.298  1.171e-07
      bare          -0.02968   0.01276  -2.326  2.002e-02
      a1           -19.67333  19.21244  -1.024  3.058e-01
      a2           -11.75180  11.70022  -1.004  3.152e-01
      
      Message: Success; Asymptotic SE's
      Function: NEGEXP with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 265 [m] 
      Average effective detection radius (EDR): 272.3 [m] (range 194.1 [m] to 382.3 [m]) 
      Average probability of detection: 1.083 97 of 193 P(detect) > 1
      Scaling: g(0 [m]) = 0.75 <- Check scaling
      Log likelihood: -992.6 
      AICc: 1993
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.016 
         Group size range: 1 to 2 
      Density in sampled area: 7.554e-06 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 31011

# negexp-Bootstraps

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~
         groupsize(groupsize), likelihood = lhood)
      Coefficients:
                   Estimate  SE       z       p(>|z|)
      (Intercept)  -3.977    0.05513  -72.15  0      
      
      Message: Success; Bootstrap SE's
      Function: NEGEXP  
      Strip: 0 [m] to 265 [m] 
      Effective detection radius (EDR): 73.89 [m] 
      Probability of detection: 0.07774
      Scaling: g(0 [m]) = 1
      Log likelihood: -1017 
      AICc: 2036
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.016 
         Group size range: 1 to 2 
      
      Density in sampled area: 9.523e-05 [1/m^2]
                       95% CI: 8.232e-05 [1/m^2] to 0.0001258 [1/m^2]
      
      Abundance in 4.105e+09 [m^2] study area: 390929
                                       95% CI: 337925 to 516498

