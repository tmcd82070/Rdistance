# negexp-MinimumInputs

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~
         groupsize(groupsize), likelihood = lhood)
      Coefficients:
                   Estimate   SE         z          p(>|z|)      
      (Intercept)  -5.064799  0.1593647  -31.78118  1.178074e-221
      
      Message: Success; Asymptotic SE's
      Function: NEGEXP  
      Strip: 0 [m] to 265 [m] 
      Effective detection radius (EDR): 158.1089 [m] 
      Probability of detection: 0.3559761
      Scaling: g(0 [m]) = 1
      Log likelihood: -194.4789 
      AICc: 390.9788
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.015544 
         Group size range: 1 to 2 
      Density in sampled area: 2.079756e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 85373.99

# negexp-NoCovar

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, w.lo = w.lo, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = 0, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                   Estimate   SE        z          p(>|z|)     
      (Intercept)  -5.990994  0.512316  -11.69394  1.368791e-31
      
      Message: Success; Asymptotic SE's
      Function: NEGEXP  
      Strip: 0 [m] to 200 [m] 
      Effective detection radius (EDR): 169.8851 [m] 
      Probability of detection: 0.7215238
      Scaling: g(0 [m]) = 1
      Log likelihood: -160.1684 
      AICc: 322.3587
      
           Surveyed Units: 120 
         Individuals seen: 188 in 185 groups 
       Average group size: 1.016216 
         Group size range: 1 to 2 
      Density in sampled area: 1.72789e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 70929.88

# negexp-ContinuousCovar

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ bare +
         groupsize(groupsize), likelihood = lhood, w.lo = w.lo, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = 0, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                   Estimate     SE          z           p(>|z|)  
      (Intercept)  -2.90232898  3.50303484  -0.8285184  0.4073770
      bare         -0.07957888  0.09668331  -0.8230880  0.4104579
      
      Message: Success; Asymptotic SE's
      Function: NEGEXP  
      Strip: 0 [m] to 200 [m] 
      Average effective detection radius (EDR): 169.2604 [m] (range 143.5847 [m] to 188.2986 [m]) 
      Average probability of detection: 0.7190263 (range 0.515414 to 0.8864095)
      Scaling: g(0 [m]) = 1
      Log likelihood: -159.7218 
      AICc: 323.5096
      
           Surveyed Units: 120 
         Individuals seen: 188 in 185 groups 
       Average group size: 1.016216 
         Group size range: 1 to 2 
      Density in sampled area: 1.760006e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 72248.23

# negexp-FactorCovar

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ observer +
         groupsize(groupsize), likelihood = lhood, w.lo = w.lo, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = 0, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                    Estimate    SE   z    p(>|z|)
      (Intercept)   -22.368324  NaN  NaN  NaN    
      observerobs2    5.384726  NaN  NaN  NaN    
      observerobs3   17.006444  NaN  NaN  NaN    
      observerobs4   17.822822  NaN  NaN  NaN    
      observerobs5    7.586429  NaN  NaN  NaN    
      observerobs6   17.330880  NaN  NaN  NaN    
      
      Message: FAILURE (Exit code= 1, false convergence (8))
      Function: NEGEXP  
      Strip: 0 [m] to 200 [m] 
      Average effective detection radius (EDR): 161.771 [m] (range 105.4332 [m] to 200 [m]) 
      Average probability of detection: 0.6982589 (range 0.2779042 to 1)
      Scaling: g(0 [m]) = 1
      Log likelihood: -151.413 
      AICc: 315.2979
      
           Surveyed Units: 120 
         Individuals seen: 188 in 185 groups 
       Average group size: 1.016216 
         Group size range: 1 to 2 
      Density in sampled area: NA
      Abundance in 4105 [km^2] study area: NA

# negexp-NoCovarWloWhi

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, w.lo = w.20, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = w.20, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                   Estimate   SE         z          p(>|z|)      
      (Intercept)  -4.993954  0.2170581  -23.00746  3.925202e-117
      
      Message: Success; Asymptotic SE's
      Function: NEGEXP  
      Strip: 20 [m] to 200 [m] 
      Effective detection radius (EDR): 138.4182 [m] 
      Probability of detection: 0.5913457
      Scaling: g(20 [m]) = 1
      Log likelihood: -187.2356 
      AICc: 376.4931
      
           Surveyed Units: 120 
         Individuals seen: 187 in 184 groups 
       Average group size: 1.016304 
         Group size range: 1 to 2 
      Density in sampled area: 2.588952e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 106276.5

# negexp-NoCovarWlo

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, w.lo = w.20, expansions =
         0, series = "cosine", x.scl = w.20, g.x.scl = 1, outputUnits = "m")
      Coefficients:
                   Estimate   SE         z      p(>|z|)
      (Intercept)  -4.624975  0.1188939  -38.9  0      
      
      Message: Success; Asymptotic SE's
      Function: NEGEXP  
      Strip: 20 [m] to 265 [m] 
      Effective detection radius (EDR): 134.5714 [m] 
      Probability of detection: 0.3016989
      Scaling: g(20 [m]) = 1
      Log likelihood: -216.1018 
      AICc: 434.2247
      
           Surveyed Units: 120 
         Individuals seen: 195 in 192 groups 
       Average group size: 1.015625 
         Group size range: 1 to 2 
      Density in sampled area: 2.856259e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 117249.4

# negexp-NoCovarsFt

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, outputUnits = "ft")
      Coefficients:
                   Estimate   SE         z         p(>|z|)
      (Intercept)  -6.252899  0.1593647  -39.2364  0      
      
      Message: Success; Asymptotic SE's
      Function: NEGEXP  
      Strip: 0 [ft] to 869.4226 [ft] 
      Effective detection radius (EDR): 518.7299 [ft] 
      Probability of detection: 0.3559761
      Scaling: g(0 [ft]) = 1
      Log likelihood: -194.4789 
      AICc: 390.9788
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.015544 
         Group size range: 1 to 2 
      Density in sampled area: 1.932157e-06 [1/ft^2]
      Abundance in 44185852261 [ft^2] study area: 85373.99

# negexp-NoCovarExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m")
      Coefficients:
                   Estimate   SE          z           p(>|z|)     
      (Intercept)  -3.805407  0.09248092  -41.148022  0.000000e+00
      a1           -0.540741  0.13168213   -4.106411  4.018535e-05
      a2           -0.433379  0.12262038   -3.534314  4.088349e-04
      
      Message: Success; Asymptotic SE's
      Function: NEGEXP with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 265 [m] 
      Effective detection radius (EDR): 405.3633 [m] 
      Probability of detection: 2.339899 > 1
      Scaling: g(0 [m]) = 1 <- Check scaling
      Log likelihood: -996.2673 
      AICc: 1998.662
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.015544 
         Group size range: 1 to 2 
      Density in sampled area: 3.163997e-06 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 12988.21

# negexp-ContCovarExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ bare +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m")
      Coefficients:
                   Estimate     SE         z          p(>|z|)     
      (Intercept)  -2.47283830  0.5227845  -4.730129  2.243771e-06
      bare         -0.03340226  0.0133137  -2.508863  1.211205e-02
      a1           -0.57184569  0.1321256  -4.328045  1.504389e-05
      a2           -0.41538909  0.1254896  -3.310148  9.324675e-04
      
      Message: Success; Asymptotic SE's
      Function: NEGEXP with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 265 [m] 
      Average effective detection radius (EDR): 572.3595 [m] (range 382.8546 [m] to 841.009 [m]) 
      Average probability of detection: 4.827057 193 of 193 P(detect) > 1
      Scaling: g(0 [m]) = 1 <- Check scaling
      Log likelihood: -992.9728 
      AICc: 1994.158
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.015544 
         Group size range: 1 to 2 
      Density in sampled area: 1.755241e-06 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 7205.265

---

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ bare +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", x.scl = xScl, g.x.scl = gXscl)
      Coefficients:
                   Estimate     SE         z          p(>|z|)     
      (Intercept)  -2.47283830  0.5227845  -4.730129  2.243771e-06
      bare         -0.03340226  0.0133137  -2.508863  1.211205e-02
      a1           -0.57184569  0.1321256  -4.328045  1.504389e-05
      a2           -0.41538909  0.1254896  -3.310148  9.324675e-04
      
      Message: Success; Asymptotic SE's
      Function: NEGEXP with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 265 [m] 
      Average effective detection radius (EDR): 495.6778 [m] (range 331.5618 [m] to 728.3352 [m]) 
      Average probability of detection: 3.620292 193 of 193 P(detect) > 1
      Scaling: g(0 [m]) = 0.75 <- Check scaling
      Log likelihood: -992.9728 
      AICc: 1994.158
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.015544 
         Group size range: 1 to 2 
      Density in sampled area: 2.340322e-06 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 9607.02

# negexp-Bootstraps

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~
         groupsize(groupsize), likelihood = lhood)
      Coefficients:
                   Estimate   SE         z          p(>|z|)
      (Intercept)  -5.064799  0.1053484  -48.07664  0      
      
      Message: Success; Bootstrap SE's
      Function: NEGEXP  
      Strip: 0 [m] to 265 [m] 
      Effective detection radius (EDR): 158.1089 [m] 
      Probability of detection: 0.3559761
      Scaling: g(0 [m]) = 1
      Log likelihood: -194.4789 
      AICc: 390.9788
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.015544 
         Group size range: 1 to 2 
      
      Density in sampled area: 2.079756e-05 [1/m^2]
                       95% CI: 1.652084e-05 [1/m^2] to 2.483694e-05 [1/m^2]
      
      Abundance in 4.105e+09 [m^2] study area: 85373.99
                                       95% CI: 67818.07 to 101955.7

