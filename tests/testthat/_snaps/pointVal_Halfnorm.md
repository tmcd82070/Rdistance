# halfnorm-MinimumInputs

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~
         groupsize(groupsize), likelihood = lhood)
      Coefficients:
                   Estimate  SE          z         p(>|z|)
      (Intercept)  4.755443  0.06790817  70.02756  0      
      
      Message: Success; Asymptotic SE's
      Function: HALFNORM  
      Strip: 0 [m] to 265 [m] 
      Effective detection radius (EDR): 158.1303 [m] 
      Probability of detection: 0.3560727
      Scaling: g(0 [m]) = 1
      Log likelihood: -178.7684 
      AICc: 359.5577
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.015544 
         Group size range: 1 to 2 
      Density in sampled area: 2.079192e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 85350.82

# halfnorm-NoCovar

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, w.lo = w.lo, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = 0, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                   Estimate  SE        z         p(>|z|)      
      (Intercept)  4.920192  0.131766  37.34039  3.631363e-305
      
      Message: Success; Asymptotic SE's
      Function: HALFNORM  
      Strip: 0 [m] to 200 [m] 
      Effective detection radius (EDR): 156.875 [m] 
      Probability of detection: 0.6152441
      Scaling: g(0 [m]) = 1
      Log likelihood: -154.1527 
      AICc: 310.3273
      
           Surveyed Units: 120 
         Individuals seen: 188 in 185 groups 
       Average group size: 1.016216 
         Group size range: 1 to 2 
      Density in sampled area: 2.026372e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 83182.59

# halfnorm-ContinuousCovar

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ bare +
         groupsize(groupsize), likelihood = lhood, w.lo = w.lo, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = 0, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                   Estimate    SE          z         p(>|z|)     
      (Intercept)  3.89328312  0.98459540  3.954196  7.679246e-05
      bare         0.02627412  0.02617529  1.003776  3.154868e-01
      
      Message: Success; Asymptotic SE's
      Function: HALFNORM  
      Strip: 0 [m] to 200 [m] 
      Average effective detection radius (EDR): 156.5702 [m] (range 137.3417 [m] to 175.2186 [m]) 
      Average probability of detection: 0.614839 (range 0.4715689 to 0.767539)
      Scaling: g(0 [m]) = 1
      Log likelihood: -153.4828 
      AICc: 311.0315
      
           Surveyed Units: 120 
         Individuals seen: 188 in 185 groups 
       Average group size: 1.016216 
         Group size range: 1 to 2 
      Density in sampled area: 2.052462e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 84253.56

# halfnorm-FactorCovar

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ observer +
         groupsize(groupsize), likelihood = lhood, w.lo = w.lo, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = 0, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                    Estimate   SE        z          p(>|z|)     
      (Intercept)   11.696652        NA         NA            NA
      observerobs2  -1.903726        NA         NA            NA
      observerobs3  -7.049380        NA         NA            NA
      observerobs4  -7.412532        NA         NA            NA
      observerobs5  -6.033658  1.089084  -5.540123  3.022601e-08
      observerobs6  -7.216953        NA         NA            NA
      
      Message: VARIANCE FAILURE (singular variance-covariance matrix)
      Function: HALFNORM  
      Strip: 0 [m] to 200 [m] 
      Average effective detection radius (EDR): 156.4422 [m] (range 101.4324 [m] to 199.9999 [m]) 
      Average probability of detection: 0.6592837 (range 0.2572131 to 0.9999993)
      Scaling: g(0 [m]) = 1
      Log likelihood: -139.9882 
      AICc: 292.4483
      
           Surveyed Units: 120 
         Individuals seen: 188 in 185 groups 
       Average group size: 1.016216 
         Group size range: 1 to 2 
      Density in sampled area: 2.636091e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 108211.5

# halfnorm-NoCovarWloWhi

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, w.lo = w.20, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = w.20, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                   Estimate  SE          z         p(>|z|)
      (Intercept)  4.572452  0.09015253  50.71906  0      
      
      Message: Success; Asymptotic SE's
      Function: HALFNORM  
      Strip: 20 [m] to 200 [m] 
      Effective detection radius (EDR): 141.2702 [m] 
      Probability of detection: 0.6159649
      Scaling: g(20 [m]) = 1
      Log likelihood: -179.8197 
      AICc: 361.6614
      
           Surveyed Units: 120 
         Individuals seen: 187 in 184 groups 
       Average group size: 1.016304 
         Group size range: 1 to 2 
      Density in sampled area: 2.485476e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 102028.8

# halfnorm-NoCovarWlo

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, w.lo = w.20, expansions =
         0, series = "cosine", x.scl = w.20, g.x.scl = 1, outputUnits = "m")
      Coefficients:
                   Estimate  SE          z         p(>|z|)
      (Intercept)  4.541644  0.05990895  75.80911  0      
      
      Message: Success; Asymptotic SE's
      Function: HALFNORM  
      Strip: 20 [m] to 265 [m] 
      Effective detection radius (EDR): 147.2844 [m] 
      Probability of detection: 0.3613943
      Scaling: g(20 [m]) = 1
      Log likelihood: -203.1634 
      AICc: 408.3478
      
           Surveyed Units: 120 
         Individuals seen: 195 in 192 groups 
       Average group size: 1.015625 
         Group size range: 1 to 2 
      Density in sampled area: 2.38446e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 97882.08

# halfnorm-NoCovarsFt

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, outputUnits = "ft")
      Coefficients:
                   Estimate  SE          z         p(>|z|)
      (Intercept)  5.943543  0.06790814  87.52327  0      
      
      Message: Success; Asymptotic SE's
      Function: HALFNORM  
      Strip: 0 [ft] to 869.4226 [ft] 
      Effective detection radius (EDR): 518.8003 [ft] 
      Probability of detection: 0.3560727
      Scaling: g(0 [ft]) = 1
      Log likelihood: -178.7684 
      AICc: 359.5577
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.015544 
         Group size range: 1 to 2 
      Density in sampled area: 1.931632e-06 [1/ft^2]
      Abundance in 44185852261 [ft^2] study area: 85350.82

# halfnorm-NoCovarExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m")
      Coefficients:
                   Estimate     SE          z           p(>|z|)     
      (Intercept)   4.39411633  0.04642238  94.6551222  0.0000000000
      a1            0.01534556  0.14070662   0.1090607  0.9131543334
      a2           -0.39749295  0.11773759  -3.3760922  0.0007352327
      
      Message: Success; Asymptotic SE's
      Function: HALFNORM with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 265 [m] 
      Effective detection radius (EDR): 150.5867 [m] 
      Probability of detection: 0.3229098
      Scaling: g(0 [m]) = 1
      Log likelihood: -998.9319 
      AICc: 2003.991
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.015544 
         Group size range: 1 to 2 
      Density in sampled area: 2.292725e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 94116.36

# halfnorm-ContCovarExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ bare +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m")
      Coefficients:
                   Estimate     SE           z           p(>|z|)     
      (Intercept)   3.61318507  0.304039764  11.8839228  1.434728e-32
      bare          0.01936504  0.007581224   2.5543418  1.063888e-02
      a1           -0.03711532  0.148746108  -0.2495213  8.029576e-01
      a2           -0.39367728  0.120215533  -3.2747622  1.057510e-03
      
      Message: Success; Asymptotic SE's
      Function: HALFNORM with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 265 [m] 
      Average effective detection radius (EDR): 155.8571 [m] (range 130.1055 [m] to 190.2429 [m]) 
      Average probability of detection: 0.3486906 (range 0.2410457 to 0.5153772)
      Scaling: g(0 [m]) = 1
      Log likelihood: -995.4657 
      AICc: 1999.144
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.015544 
         Group size range: 1 to 2 
      Density in sampled area: 2.18922e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 89867.5

---

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ bare +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", x.scl = xScl, g.x.scl = gXscl)
      Coefficients:
                   Estimate     SE           z           p(>|z|)     
      (Intercept)   3.61318507  0.304039764  11.8839228  1.434728e-32
      bare          0.01936504  0.007581224   2.5543418  1.063888e-02
      a1           -0.03711532  0.148746108  -0.2495213  8.029576e-01
      a2           -0.39367728  0.120215533  -3.2747622  1.057510e-03
      
      Message: Success; Asymptotic SE's
      Function: HALFNORM with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 265 [m] 
      Average effective detection radius (EDR): 134.9762 [m] (range 112.6746 [m] to 164.7552 [m]) 
      Average probability of detection: 0.261518 (range 0.1807842 to 0.3865329)
      Scaling: g(0 [m]) = 0.75
      Log likelihood: -995.4657 
      AICc: 1999.144
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.015544 
         Group size range: 1 to 2 
      Density in sampled area: 2.918961e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 119823.3

# halfnorm-Bootstraps

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~
         groupsize(groupsize), likelihood = lhood)
      Coefficients:
                   Estimate  SE          z         p(>|z|)
      (Intercept)  4.755443  0.05665596  83.93545  0      
      
      Message: Success; Bootstrap SE's
      Function: HALFNORM  
      Strip: 0 [m] to 265 [m] 
      Effective detection radius (EDR): 158.1303 [m] 
      Probability of detection: 0.3560727
      Scaling: g(0 [m]) = 1
      Log likelihood: -178.7684 
      AICc: 359.5577
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.015544 
         Group size range: 1 to 2 
      
      Density in sampled area: 2.079192e-05 [1/m^2]
                       95% CI: 1.642835e-05 [1/m^2] to 2.418883e-05 [1/m^2]
      
      Abundance in 4.105e+09 [m^2] study area: 85350.82
                                       95% CI: 67438.37 to 99295.13

