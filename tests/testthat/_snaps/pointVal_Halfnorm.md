# halfnorm-MinimumInputs

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~
         groupsize(groupsize), likelihood = lhood)
      Coefficients:
                   Estimate  SE      z      p(>|z|)
      (Intercept)  4.342     0.0378  114.9  0      
      
      Message: Success; Asymptotic SE's
      Function: HALFNORM  
      Strip: 0 [m] to 265 [m] 
      Effective detection radius (EDR): 108.6 [m] 
      Probability of detection: 0.1679
      Scaling: g(0 [m]) = 1
      Log likelihood: -1004 
      AICc: 2011
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.016 
         Group size range: 1 to 2 
      Density in sampled area: 4.409e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 180979

# halfnorm-NoCovar

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, w.lo = w.lo, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = 0, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                   Estimate  SE      z      p(>|z|)
      (Intercept)  4.31      0.0464  92.88  0      
      
      Message: Success; Asymptotic SE's
      Function: HALFNORM  
      Strip: 0 [m] to 200 [m] 
      Effective detection radius (EDR): 103.8 [m] 
      Probability of detection: 0.2695
      Scaling: g(0 [m]) = 1
      Log likelihood: -937.9 
      AICc: 1878
      
           Surveyed Units: 120 
         Individuals seen: 188 in 185 groups 
       Average group size: 1.016 
         Group size range: 1 to 2 
      Density in sampled area: 4.626e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 189899

# halfnorm-ContinuousCovar

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ bare +
         groupsize(groupsize), likelihood = lhood, w.lo = w.lo, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = 0, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                   Estimate  SE        z       p(>|z|)  
      (Intercept)  3.7949    0.356234  10.653  1.692e-26
      bare         0.0131    0.009144   1.432  1.520e-01
      
      Message: Success; Asymptotic SE's
      Function: HALFNORM  
      Strip: 0 [m] to 200 [m] 
      Average effective detection radius (EDR): 103.9 [m] (range 92.24 [m] to 118.9 [m]) 
      Average probability of detection: 0.2706 (range 0.2127 to 0.3535)
      Scaling: g(0 [m]) = 1
      Log likelihood: -936.8 
      AICc: 1878
      
           Surveyed Units: 120 
         Individuals seen: 188 in 185 groups 
       Average group size: 1.016 
         Group size range: 1 to 2 
      Density in sampled area: 4.667e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 191600

# halfnorm-FactorCovar

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ observer +
         groupsize(groupsize), likelihood = lhood, w.lo = w.lo, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = 0, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                    Estimate  SE      z        p(>|z|)  
      (Intercept)    4.7446   0.2322  20.4361  7.991e-93
      observerobs2  -0.1786   0.2705  -0.6602  5.091e-01
      observerobs3  -0.5716   0.2689  -2.1258  3.352e-02
      observerobs4  -0.8311   0.2426  -3.4260  6.126e-04
      observerobs5  -0.2452   0.2796  -0.8767  3.807e-01
      observerobs6  -0.6797   0.2617  -2.5973  9.397e-03
      
      Message: Success; Asymptotic SE's
      Function: HALFNORM  
      Strip: 0 [m] to 200 [m] 
      Average effective detection radius (EDR): 105.3 [m] (range 70.81 [m] to 143.6 [m]) 
      Average probability of detection: 0.2968 (range 0.1253 to 0.5153)
      Scaling: g(0 [m]) = 1
      Log likelihood: -916.7 
      AICc: 1846
      
           Surveyed Units: 120 
         Individuals seen: 188 in 185 groups 
       Average group size: 1.016 
         Group size range: 1 to 2 
      Density in sampled area: 5.633e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 231233

# halfnorm-NoCovarWloWhi

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, w.lo = w.20, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = w.20, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                   Estimate  SE       z      p(>|z|)
      (Intercept)  4.085     0.04172  97.93  0      
      
      Message: Success; Asymptotic SE's
      Function: HALFNORM  
      Strip: 20 [m] to 200 [m] 
      Effective detection radius (EDR): 83.66 [m] 
      Probability of detection: 0.216
      Scaling: g(20 [m]) = 1
      Log likelihood: -919.5 
      AICc: 1841
      
           Surveyed Units: 120 
         Individuals seen: 187 in 184 groups 
       Average group size: 1.016 
         Group size range: 1 to 2 
      Density in sampled area: 7.088e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 290954

# halfnorm-NoCovarWlo

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, w.lo = w.20, expansions =
         0, series = "cosine", x.scl = w.20, g.x.scl = 1, outputUnits = "m")
      Coefficients:
                   Estimate  SE       z      p(>|z|)
      (Intercept)  4.161     0.03676  113.2  0      
      
      Message: Success; Asymptotic SE's
      Function: HALFNORM  
      Strip: 20 [m] to 265 [m] 
      Effective detection radius (EDR): 90.72 [m] 
      Probability of detection: 0.1371
      Scaling: g(20 [m]) = 1
      Log likelihood: -989.2 
      AICc: 1980
      
           Surveyed Units: 120 
         Individuals seen: 195 in 192 groups 
       Average group size: 1.016 
         Group size range: 1 to 2 
      Density in sampled area: 6.286e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 258021

# halfnorm-NoCovarsFt

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, outputUnits = "ft")
      Coefficients:
                   Estimate  SE      z      p(>|z|)
      (Intercept)  5.53      0.0378  146.3  0      
      
      Message: Success; Asymptotic SE's
      Function: HALFNORM  
      Strip: 0 [ft] to 869.4 [ft] 
      Effective detection radius (EDR): 356.3 [ft] 
      Probability of detection: 0.1679
      Scaling: g(0 [ft]) = 1
      Log likelihood: -1234 
      AICc: 2469
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.016 
         Group size range: 1 to 2 
      Density in sampled area: 4.096e-06 [1/ft^2]
      Abundance in 4.419e+10 [ft^2] study area: 180979

# halfnorm-NoCovarCosExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", series = "cosine")
      Coefficients:
                   Estimate  SE       z         p(>|z|)
      (Intercept)   4.344    0.04121  105.4344  0.0000 
      a1           -2.849    2.91854   -0.9763  0.3289 
      a2           -4.712    4.13361   -1.1399  0.2543 
      
      Message: Success; Asymptotic SE's
      Function: HALFNORM with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 265 [m] 
      Effective detection radius (EDR): 252.8 [m] 
      Probability of detection: 0.9103
      Scaling: g(0 [m]) = 1
      Log likelihood: -994.6 
      AICc: 1995
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.016 
         Group size range: 1 to 2 
      Density in sampled area: 8.133e-06 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 33386

# halfnorm-NoCovarSinExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", series = "sine")
      Coefficients:
                   Estimate  SE       z         p(>|z|) 
      (Intercept)  4.50317   0.09035  49.84356  0.000000
      a1           1.01647   0.38823   2.61823  0.008839
      a2           0.02268   0.26975   0.08406  0.933006
      
      Message: Success; Asymptotic SE's
      Function: HALFNORM with 2 expansion(s) of SINE series 
      Strip: 0 [m] to 265 [m] 
      Effective detection radius (EDR): 135.2 [m] 
      Probability of detection: 0.2603
      Scaling: g(0 [m]) = 1
      Log likelihood: -1000 
      AICc: 2007
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.016 
         Group size range: 1 to 2 
      Density in sampled area: 2.844e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 116739

# halfnorm-NoCovarHermExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", series = "hermite")
      Coefficients:
                   Estimate  SE        z        p(>|z|)
      (Intercept)   4.201     0.06771  62.0463  0.0000 
      a1            4.172    13.08867   0.3187  0.7499 
      a2           -1.572     5.02262  -0.3129  0.7544 
      
      Message: Success; Asymptotic SE's
      Function: HALFNORM with 2 expansion(s) of HERMITE series 
      Strip: 0 [m] to 265 [m] 
      Effective detection radius (EDR): 313.3 [m] 
      Probability of detection: 1.398 > 1
      Scaling: g(0 [m]) = 1 <- Check scaling
      Log likelihood: -1002 
      AICc: 2010
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.016 
         Group size range: 1 to 2 
      Density in sampled area: 5.296e-06 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 21739

# halfnorm-NoCovarSimpExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", series = "simple")
      Coefficients:
                   Estimate  SE      z       p(>|z|)
      (Intercept)   4.378    0.1055  41.515  0.0000 
      a1           -3.343    2.5523  -1.310  0.1903 
      a2            4.283    3.1516   1.359  0.1742 
      
      Message: Success; Asymptotic SE's
      Function: HALFNORM with 2 expansion(s) of SIMPLE series 
      Strip: 0 [m] to 265 [m] 
      Effective detection radius (EDR): 108 [m] 
      Probability of detection: 0.166
      Scaling: g(0 [m]) = 1
      Log likelihood: -1003 
      AICc: 2013
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.016 
         Group size range: 1 to 2 
      Density in sampled area: 4.46e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 183082

# halfnorm-ContCovarExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ bare +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m")
      Coefficients:
                   Estimate  SE        z       p(>|z|)  
      (Intercept)   3.59388  0.280582  12.809  1.466e-37
      bare          0.01877  0.007085   2.650  8.056e-03
      a1           -3.33133  3.348144  -0.995  3.197e-01
      a2           -4.99847  4.432220  -1.128  2.594e-01
      
      Message: Success; Asymptotic SE's
      Function: HALFNORM with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 265 [m] 
      Average effective detection radius (EDR): 261.7 [m] (range 216.5 [m] to 323.1 [m]) 
      Average probability of detection: 0.9837 82 of 193 P(detect) > 1
      Scaling: g(0 [m]) = 1 <- Check scaling
      Log likelihood: -990.9 
      AICc: 1990
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.016 
         Group size range: 1 to 2 
      Density in sampled area: 7.789e-06 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 31974

---

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ bare +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", x.scl = xScl, g.x.scl = gXscl)
      Coefficients:
                   Estimate  SE        z       p(>|z|)  
      (Intercept)   3.59388  0.280582  12.809  1.466e-37
      bare          0.01877  0.007085   2.650  8.056e-03
      a1           -3.33133  3.348144  -0.995  3.197e-01
      a2           -4.99847  4.432220  -1.128  2.594e-01
      
      Message: Success; Asymptotic SE's
      Function: HALFNORM with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 265 [m] 
      Average effective detection radius (EDR): 261.7 [m] (range 216.5 [m] to 323.1 [m]) 
      Average probability of detection: 0.9837 82 of 193 P(detect) > 1
      Scaling: g(0 [m]) = 0.75 <- Check scaling
      Log likelihood: -990.9 
      AICc: 1990
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.016 
         Group size range: 1 to 2 
      Density in sampled area: 7.789e-06 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 31974

# halfnorm-Bootstraps

    Code
      summary(abun)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~
         groupsize(groupsize), likelihood = lhood)
      Coefficients:
                   Estimate  SE       z      p(>|z|)
      (Intercept)  4.342     0.04053  107.1  0      
      
      Message: Success; Bootstrap SE's
      Function: HALFNORM  
      Strip: 0 [m] to 265 [m] 
      Effective detection radius (EDR): 108.6 [m] 
      Probability of detection: 0.1679
      Scaling: g(0 [m]) = 1
      Log likelihood: -1004 
      AICc: 2011
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.016 
         Group size range: 1 to 2 
      
      Density in sampled area: 4.409e-05 [1/m^2]
                       95% CI: 3.558e-05 [1/m^2] to 5.062e-05 [1/m^2]
      
      Abundance in 4.105e+09 [m^2] study area: 180979
                                       95% CI: 146063 to 207788

