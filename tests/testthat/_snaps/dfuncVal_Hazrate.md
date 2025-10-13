# hazrate-MinimumInputs

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~
         groupsize(groupsize), likelihood = lhood)
      Coefficients:
                   Estimate  SE          z          p(>|z|)     
      (Intercept)  3.852207  0.09954916  38.696528  0.000000e+00
      k            2.821023  0.32412762   8.703432  3.219955e-18
      
      Message: Success; Asymptotic SE's
      Function: HAZRATE  
      Strip: 0 [m] to 207 [m] 
      Effective strip width (ESW): 63.88362 [m] 
      Probability of detection: 0.3086165
      Scaling: g(0 [m]) = 1
      Log likelihood: -1665.103 
      AICc: 3334.241
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.050562 
         Group size range: 1 to 3 
      Density in sampled area: 8.131105e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 333781.8

# hazrate-NoCovar

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, w.lo = w.lo, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = 0, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                   Estimate  SE         z          p(>|z|)      
      (Intercept)  3.774729  0.1079167  34.978162  4.833461e-268
      k            2.405957  0.3096556   7.769785   7.861954e-15
      
      Message: Success; Asymptotic SE's
      Function: HAZRATE  
      Strip: 0 [m] to 150 [m] 
      Effective strip width (ESW): 61.11661 [m] 
      Probability of detection: 0.4074441
      Scaling: g(0 [m]) = 1
      Log likelihood: -1631.796 
      AICc: 3267.625
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 371 in 353 groups 
       Average group size: 1.050992 
         Group size range: 1 to 3 
      Density in sampled area: 8.431059e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 346095

# hazrate-ContinuousCovar

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ bare +
         groupsize(groupsize), likelihood = lhood, w.lo = w.lo, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = 0, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                   Estimate    SE           z          p(>|z|)     
      (Intercept)  3.06243838  0.275717186  11.107173  1.157672e-28
      bare         0.01325053  0.004247695   3.119463  1.811810e-03
      k            2.54787816  0.346483941   7.353525  1.930476e-13
      
      Message: Success; Asymptotic SE's
      Function: HAZRATE  
      Strip: 0 [m] to 150 [m] 
      Average effective strip width (ESW): 62.78889 [m] (range 45.37353 [m] to 81.93084 [m]) 
      Average probability of detection: 0.4185926 (range 0.3024902 to 0.5462056)
      Scaling: g(0 [m]) = 1
      Log likelihood: -1626.92 
      AICc: 3259.908
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 371 in 353 groups 
       Average group size: 1.050992 
         Group size range: 1 to 3 
      Density in sampled area: 8.448723e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 346820.1

# hazrate-FactorCovar

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ observer +
         groupsize(groupsize), likelihood = lhood, w.lo = w.lo, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = 0, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                    Estimate     SE         z           p(>|z|)      
      (Intercept)    3.91558993  0.1444321  27.1102456  7.455990e-162
      observerobs2   0.17191702  0.2027945   0.8477400   3.965828e-01
      observerobs3   0.06574510  0.1658300   0.3964609   6.917650e-01
      observerobs4  -0.38573966  0.1757132  -2.1952801   2.814351e-02
      observerobs5  -0.09584549  0.1671431  -0.5734338   5.663510e-01
      k              2.76471844  0.3481394   7.9414113   1.998936e-15
      
      Message: Success; Asymptotic SE's
      Function: HAZRATE  
      Strip: 0 [m] to 150 [m] 
      Average effective strip width (ESW): 64.83155 [m] (range 46.60926 [m] to 77.35035 [m]) 
      Average probability of detection: 0.4322103 (range 0.3107284 to 0.515669)
      Scaling: g(0 [m]) = 1
      Log likelihood: -1627.509 
      AICc: 3267.26
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 371 in 353 groups 
       Average group size: 1.050992 
         Group size range: 1 to 3 
      Density in sampled area: 8.124965e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 333529.8

# hazrate-NoCovarWloWhi

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, w.lo = w.20, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = w.20, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                   Estimate  SE         z          p(>|z|)      
      (Intercept)  3.814338  0.1117919  34.119989  3.727893e-255
      k            2.481511  0.3484759   7.121041   1.071152e-12
      
      Message: Success; Asymptotic SE's
      Function: HAZRATE  
      Strip: 2 [m] to 150 [m] 
      Effective strip width (ESW): 62.58799 [m] 
      Probability of detection: 0.4228918
      Scaling: g(2 [m]) = 1
      Log likelihood: -1526.016 
      AICc: 3056.069
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 347 in 330 groups 
       Average group size: 1.051515 
         Group size range: 1 to 3 
      Density in sampled area: 7.70027e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 316096.1

# hazrate-NoCovarWlo

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, w.lo = w.20, expansions =
         0, series = "cosine", x.scl = w.20, g.x.scl = 1, outputUnits = "m")
      Coefficients:
                   Estimate  SE         z          p(>|z|)      
      (Intercept)  3.894830  0.1031639  37.753805  6.514747e-312
      k            2.933948  0.3696785   7.936486   2.079907e-15
      
      Message: Success; Asymptotic SE's
      Function: HAZRATE  
      Strip: 2 [m] to 207 [m] 
      Effective strip width (ESW): 65.61884 [m] 
      Probability of detection: 0.3200919
      Scaling: g(2 [m]) = 1
      Log likelihood: -1558.732 
      AICc: 3121.5
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 350 in 333 groups 
       Average group size: 1.051051 
         Group size range: 1 to 3 
      Density in sampled area: 7.408103e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 304102.6

# hazrate-NoCovarsFt

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, outputUnits = "ft")
      Coefficients:
                   Estimate  SE          z          p(>|z|)     
      (Intercept)  5.040306  0.09954522  50.633332  0.000000e+00
      k            2.821023  0.32411484   8.703775  3.210234e-18
      
      Message: Success; Asymptotic SE's
      Function: HAZRATE  
      Strip: 0 [ft] to 679.1339 [ft] 
      Effective strip width (ESW): 209.5919 [ft] 
      Probability of detection: 0.3086165
      Scaling: g(0 [ft]) = 1
      Log likelihood: -2088.067 
      AICc: 4180.168
      
           Surveyed Units: 118110.2 [ft] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.050562 
         Group size range: 1 to 3 
      Density in sampled area: 7.554043e-06 [1/ft^2]
      Abundance in 44185852261 [ft^2] study area: 333781.8

# hazrate-NoCovarExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m")
      Coefficients:
                   Estimate    SE         z          p(>|z|)     
      (Intercept)   2.9185280  0.4528771   6.444415  1.160469e-10
      k             2.2401149  0.2029433  11.038131  2.501839e-28
      a1           -0.3749832  0.1759458  -2.131243  3.306915e-02
      a2           -0.3825795  0.1988336  -1.924119  5.433966e-02
      
      Message: Success; Asymptotic SE's
      Function: HAZRATE with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 207 [m] 
      Effective strip width (ESW): 64.45754 [m] 
      Probability of detection: 0.3113891
      Scaling: g(0 [m]) = 1
      Log likelihood: -1661.781 
      AICc: 3331.677
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.050562 
         Group size range: 1 to 3 
      Density in sampled area: 8.058707e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 330809.9

# hazrate-ContCovarExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ bare +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m")
      Coefficients:
                   Estimate     SE           z          p(>|z|)     
      (Intercept)   2.57285361  0.348271942   7.387485  1.496325e-13
      bare          0.01312417  0.004277266   3.068356  2.152400e-03
      k             2.43201406  0.239839532  10.140172  3.664544e-24
      a1           -0.32652796  0.194667766  -1.677360  9.347207e-02
      a2           -0.23085353  0.175986295  -1.311770  1.895978e-01
      
      Message: Success; Asymptotic SE's
      Function: HAZRATE with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 207 [m] 
      Average effective strip width (ESW): 68.24569 [m] (range 42.42778 [m] to 100.5137 [m]) 
      Average probability of detection: 0.3296893 (range 0.2049651 to 0.4855735)
      Scaling: g(0 [m]) = 1
      Log likelihood: -1657.406 
      AICc: 3324.983
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.050562 
         Group size range: 1 to 3 
      Density in sampled area: 8.086028e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 331931.4

# hazrate-ContCovarExpansionsScaling

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ bare +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", x.scl = xScl, g.x.scl = gXscl)
      Coefficients:
                   Estimate     SE           z          p(>|z|)     
      (Intercept)   2.57285361  0.348271942   7.387485  1.496325e-13
      bare          0.01312417  0.004277266   3.068356  2.152400e-03
      k             2.43201406  0.239839532  10.140172  3.664544e-24
      a1           -0.32652796  0.194667766  -1.677360  9.347207e-02
      a2           -0.23085353  0.175986295  -1.311770  1.895978e-01
      
      Message: Success; Asymptotic SE's
      Function: HAZRATE with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 207 [m] 
      Average effective strip width (ESW): 51.18427 [m] (range 31.82083 [m] to 75.38529 [m]) 
      Average probability of detection: 0.247267 (range 0.1537238 to 0.3641801)
      Scaling: g(0 [m]) = 0.75
      Log likelihood: -1657.406 
      AICc: 3324.983
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.050562 
         Group size range: 1 to 3 
      Density in sampled area: 0.0001078137 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 442575.2

# hazrate-Bootstraps

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~
         groupsize(groupsize), likelihood = lhood)
      Coefficients:
                   Estimate  SE         z          p(>|z|)      
      (Intercept)  3.852207  0.1429937  26.939698  7.532086e-160
      k            2.821023  0.4312821   6.541015   6.110274e-11
      
      Message: Success; Bootstrap SE's
      Function: HAZRATE  
      Strip: 0 [m] to 207 [m] 
      Effective strip width (ESW): 63.88362 [m] 
      Probability of detection: 0.3086165
      Scaling: g(0 [m]) = 1
      Log likelihood: -1665.103 
      AICc: 3334.241
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.050562 
         Group size range: 1 to 3 
      
      Density in sampled area: 8.131105e-05 [1/m^2]
                       95% CI: 6.331e-05 [1/m^2] to 0.0001033262 [1/m^2]
      
      Abundance in 4.105e+09 [m^2] study area: 333781.8
                                       95% CI: 259887.5 to 424153.9

