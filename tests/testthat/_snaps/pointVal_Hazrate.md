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
      (Intercept)  3.90757224  0.370784260  10.538668  5.730472e-26
      bare         0.01605944  0.009417653   1.705248  8.814814e-02
      k            4.06653736  0.484368871   8.395538  4.637650e-17
      
      Message: Success; Asymptotic SE's
      Function: HAZRATE  
      Strip: 0 [m] to 200 [m] 
      Average effective detection radius (EDR): 116.4435 [m] (range 101.2568 [m] to 136.9467 [m]) 
      Average probability of detection: 0.3406227 (range 0.2563233 to 0.4688599)
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
      (Intercept)    5.05175727  0.1220394  41.3944662  0.000000e+00
      observerobs2  -0.08950765  0.1299719  -0.6886691  4.910316e-01
      observerobs3  -0.39226960  0.1590886  -2.4657297  1.367345e-02
      observerobs4  -0.65859100  0.1242182  -5.3018894  1.146103e-07
      observerobs5  -0.06682267  0.1449120  -0.4611257  6.447084e-01
      observerobs6  -0.48601441  0.1368076  -3.5525394  3.815318e-04
      k              7.29076716  1.6934219   4.3053460  1.667249e-05
      
      Message: Success; Asymptotic SE's
      Function: HAZRATE  
      Strip: 0 [m] to 200 [m] 
      Average effective detection radius (EDR): 131.5898 [m] (range 90.66239 [m] to 168.3414 [m]) 
      Average probability of detection: 0.4579872 (range 0.2054917 to 0.7084709)
      Scaling: g(0 [m]) = 1
      Log likelihood: -909.6747 
      AICc: 1833.982
      
           Surveyed Units: 120 
         Individuals seen: 188 in 185 groups 
       Average group size: 1.016216 
         Group size range: 1 to 2 
      Density in sampled area: 3.482612e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 142961.2

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
      (Intercept)  4.117368  0.08146998  50.53847  0.000000e+00
      k            3.421336  0.28021407  12.20972  2.758283e-34
      
      Message: Success; Asymptotic SE's
      Function: HAZRATE  
      Strip: 20 [m] to 265 [m] 
      Effective detection radius (EDR): 85.45977 [m] 
      Probability of detection: 0.1216722
      Scaling: g(20 [m]) = 1
      Log likelihood: -981.628 
      AICc: 1967.32
      
           Surveyed Units: 120 
         Individuals seen: 195 in 192 groups 
       Average group size: 1.015625 
         Group size range: 1 to 2 
      Density in sampled area: 7.082394e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 290732.3

# hazrate-NoCovarsFt

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, outputUnits = "ft")
      Coefficients:
                   Estimate  SE          z         p(>|z|)     
      (Intercept)  5.728533  0.06267096  91.40651  0.000000e+00
      k            4.199565  0.39728670  10.57061  4.078142e-26
      
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
      (Intercept)   4.326691  0.1131519  38.2379137  6.605658e-320
      k             3.491206  0.3250280  10.7412475   6.515789e-27
      a1           -8.216837  8.0995863  -1.0144761   3.103556e-01
      a2           -3.766622  3.8274936  -0.9840962   3.250682e-01
      
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
      (Intercept)  4.5975100  0.09410682  48.8541627  0.000000e+00
      k            3.7588446  0.62775698   5.9877384  2.127790e-09
      a1           0.6502632  0.54229312   1.1990991  2.304894e-01
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
      (Intercept)       4.337168  NaN  NaN  NaN    
      k                 3.670728  NaN  NaN  NaN    
      a1             3502.168875  NaN  NaN  NaN    
      a2           -69979.596242  NaN  NaN  NaN    
      
      Message: FAILURE (Exit code= 1, singular convergence (7))
      Function: HAZRATE with 2 expansion(s) of HERMITE series 
      Strip: 0 [m] to 265 [m] 
      Effective detection radius (EDR): 46911.86 [m] 
      Probability of detection: 31338.17 > 1
      Scaling: g(0 [m]) = 1 <- Check scaling
      Log likelihood: -993.2323 
      AICc: 1994.677
      
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
      (Intercept)   4.5329126  0.06833974  66.32908295  0.000000000
      k             3.7423630  1.22387436   3.05779999  0.002229683
      a1           -1.1108324  5.36415302  -0.20708439  0.835943949
      a2            0.4805082  5.09018720   0.09439892  0.924792270
      
      Message: Success; Asymptotic SE's
      Function: HAZRATE with 2 expansion(s) of SIMPLE series 
      Strip: 0 [m] to 265 [m] 
      Effective detection radius (EDR): 118.1195 [m] 
      Probability of detection: 0.1986789
      Scaling: g(0 [m]) = 1
      Log likelihood: -998.6731 
      AICc: 2005.559
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.015544 
         Group size range: 1 to 2 
      Density in sampled area: 3.726332e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 152965.9

# hazrate-ContCovarExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ bare +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m")
      Coefficients:
                   Estimate    SE   z    p(>|z|)
      (Intercept)   4.2266057  NaN  NaN  NaN    
      bare         -0.4492500  NaN  NaN  NaN    
      k             1.3064577  NaN  NaN  NaN    
      a1           -0.4326437  NaN  NaN  NaN    
      a2           -0.4621804  NaN  NaN  NaN    
      
      Message: FAILURE (Exit code= 1, false convergence (8))
      Function: HAZRATE with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 265 [m] 
      Average effective detection radius (EDR): 0.005214684 [m] (range 6.002154e-05 [m] to 0.03710565 [m]) 
      Average probability of detection: 1.05931e-09 (range 5.130061e-14 to 1.960597e-08)
      Scaling: g(0 [m]) = 1
      Log likelihood: -1059.096 
      AICc: 2128.513
      
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
      (Intercept)   4.2266057  NaN  NaN  NaN    
      bare         -0.4492500  NaN  NaN  NaN    
      k             1.3064577  NaN  NaN  NaN    
      a1           -0.4326437  NaN  NaN  NaN    
      a2           -0.4621804  NaN  NaN  NaN    
      
      Message: FAILURE (Exit code= 1, false convergence (8))
      Function: HAZRATE with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 265 [m] 
      Average effective detection radius (EDR): 0.005214684 [m] (range 6.002154e-05 [m] to 0.03710565 [m]) 
      Average probability of detection: 1.05931e-09 (range 5.130061e-14 to 1.960597e-08)
      Scaling: g(0 [m]) = 0.75
      Log likelihood: -1059.096 
      AICc: 2128.513
      
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
      (Intercept)  4.540434  0.05759518  78.833565  0.000000e+00
      k            4.199564  0.48179821   8.716438  2.870904e-18
      
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
                       95% CI: 3.13616e-05 [1/m^2] to 4.094837e-05 [1/m^2]
      
      Abundance in 4.105e+09 [m^2] study area: 151664.1
                                       95% CI: 128739.4 to 168093.1

