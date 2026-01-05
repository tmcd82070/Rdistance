# hazrate-MinimumInputs

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~
         groupsize(groupsize), likelihood = lhood)
      Coefficients:
                   Estimate  SE          z          p(>|z|)     
      (Intercept)  3.852295  0.09950604  38.714183  0.000000e+00
      k            2.821360  0.32401751   8.707431  3.108393e-18
      
      Message: Success; Asymptotic SE's
      Function: HAZRATE  
      Strip: 0 [m] to 207 [m] 
      Effective strip width (ESW): 63.88645 [m] 
      Probability of detection: 0.3086302
      Scaling: g(0 [m]) = 1
      Log likelihood: -1665.104 
      AICc: 3334.242
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.050562 
         Group size range: 1 to 3 
      Density in sampled area: 8.130746e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 333767.1

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
      (Intercept)  3.774830  0.1078587  34.997924  2.419532e-268
      k            2.406237  0.3095688   7.772868   7.672851e-15
      
      Message: Success; Asymptotic SE's
      Function: HAZRATE  
      Strip: 0 [m] to 150 [m] 
      Effective strip width (ESW): 61.11997 [m] 
      Probability of detection: 0.4074665
      Scaling: g(0 [m]) = 1
      Log likelihood: -1631.796 
      AICc: 3267.627
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 371 in 353 groups 
       Average group size: 1.050992 
         Group size range: 1 to 3 
      Density in sampled area: 8.430596e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 346076

# hazrate-ContinuousCovar

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ bare +
         groupsize(groupsize), likelihood = lhood, w.lo = w.lo, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = 0, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                   Estimate   SE           z          p(>|z|)     
      (Intercept)  3.0625868  0.275629246  11.111255  1.105941e-28
      bare         0.0132495  0.004247067   3.119682  1.810466e-03
      k            2.5481844  0.346358964   7.357062  1.880019e-13
      
      Message: Success; Asymptotic SE's
      Function: HAZRATE  
      Strip: 0 [m] to 150 [m] 
      Average effective strip width (ESW): 62.79174 [m] (range 45.37667 [m] to 81.9333 [m]) 
      Average probability of detection: 0.4186116 (range 0.3025112 to 0.546222)
      Scaling: g(0 [m]) = 1
      Log likelihood: -1626.92 
      AICc: 3259.91
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 371 in 353 groups 
       Average group size: 1.050992 
         Group size range: 1 to 3 
      Density in sampled area: 8.448308e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 346803

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
      (Intercept)    3.91561596  0.1444196  27.1127749  6.961189e-162
      observerobs2   0.17193055  0.2027853   0.8478451   3.965243e-01
      observerobs3   0.06575710  0.1658222   0.3965518   6.916980e-01
      observerobs4  -0.38576915  0.1757095  -2.1954939   2.812819e-02
      observerobs5  -0.09584489  0.1671379  -0.5734479   5.663414e-01
      k              2.76485170  0.3480909   7.9429016   1.975056e-15
      
      Message: Success; Asymptotic SE's
      Function: HAZRATE  
      Strip: 0 [m] to 150 [m] 
      Average effective strip width (ESW): 64.83241 [m] (range 46.60845 [m] to 77.35221 [m]) 
      Average probability of detection: 0.4322161 (range 0.310723 to 0.5156814)
      Scaling: g(0 [m]) = 1
      Log likelihood: -1627.509 
      AICc: 3267.261
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 371 in 353 groups 
       Average group size: 1.050992 
         Group size range: 1 to 3 
      Density in sampled area: 8.124889e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 333526.7

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
      (Intercept)  3.814405  0.1117546  34.131985  2.474696e-255
      k            2.481714  0.3484249   7.122666   1.058595e-12
      
      Message: Success; Asymptotic SE's
      Function: HAZRATE  
      Strip: 2 [m] to 150 [m] 
      Effective strip width (ESW): 62.59031 [m] 
      Probability of detection: 0.4229075
      Scaling: g(2 [m]) = 1
      Log likelihood: -1526.017 
      AICc: 3056.07
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 347 in 330 groups 
       Average group size: 1.051515 
         Group size range: 1 to 3 
      Density in sampled area: 7.699985e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 316084.4

# hazrate-NoCovarWlo

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, w.lo = w.20, expansions =
         0, series = "cosine", x.scl = w.20, g.x.scl = 1, outputUnits = "m")
      Coefficients:
                   Estimate  SE         z          p(>|z|)      
      (Intercept)  3.894889  0.1031291  37.767132  3.937297e-312
      k            2.934207  0.3695835   7.939225   2.034484e-15
      
      Message: Success; Asymptotic SE's
      Function: HAZRATE  
      Strip: 2 [m] to 207 [m] 
      Effective strip width (ESW): 65.62078 [m] 
      Probability of detection: 0.3201014
      Scaling: g(2 [m]) = 1
      Log likelihood: -1558.732 
      AICc: 3121.501
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 350 in 333 groups 
       Average group size: 1.051051 
         Group size range: 1 to 3 
      Density in sampled area: 7.407884e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 304093.6

# hazrate-NoCovarsFt

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, outputUnits = "ft")
      Coefficients:
                   Estimate  SE          z          p(>|z|)     
      (Intercept)  5.040394  0.09950205  50.656184  0.000000e+00
      k            2.821360  0.32400459   8.707778  3.098889e-18
      
      Message: Success; Asymptotic SE's
      Function: HAZRATE  
      Strip: 0 [ft] to 679.1339 [ft] 
      Effective strip width (ESW): 209.6012 [ft] 
      Probability of detection: 0.3086302
      Scaling: g(0 [ft]) = 1
      Log likelihood: -2088.068 
      AICc: 4180.169
      
           Surveyed Units: 118110.2 [ft] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.050562 
         Group size range: 1 to 3 
      Density in sampled area: 7.55371e-06 [1/ft^2]
      Abundance in 44185852261 [ft^2] study area: 333767.1

# hazrate-NoCovarCosExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", series = "cosine")
      Coefficients:
                   Estimate    SE         z            p(>|z|)      
      (Intercept)  4.13074740  0.1939813  21.29456685  1.274793e-100
      k            4.10850039  0.6069398   6.76920531   1.294917e-11
      a1           0.03600197  0.5892702   0.06109586   9.512829e-01
      a2           0.49596215  0.2060589   2.40689477   1.608880e-02
      
      Message: Success; Asymptotic SE's
      Function: HAZRATE with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 207 [m] 
      Effective strip width (ESW): 53.91132 [m] 
      Probability of detection: 0.2604412
      Scaling: g(0 [m]) = 1
      Log likelihood: -1658.738 
      AICc: 3325.589
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.050562 
         Group size range: 1 to 3 
      Density in sampled area: 9.635164e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 395523.5

# hazrate-NoCovarSinExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", series = "sine")
      Coefficients:
                   Estimate    SE         z          p(>|z|)     
      (Intercept)  -1.5316527  0.9472262  -1.616987  1.058810e-01
      k             0.5338098  0.0664294   8.035747  9.301056e-16
      a1            2.6708045  0.4609032   5.794719  6.843563e-09
      a2            1.1830604  0.5447523   2.171740  2.987526e-02
      
      Message: Success; Asymptotic SE's
      Function: HAZRATE with 2 expansion(s) of SINE series 
      Strip: 0 [m] to 207 [m] 
      Effective strip width (ESW): 14.8014 [m] 
      Probability of detection: 0.07150434
      Scaling: g(0 [m]) = 1
      Log likelihood: -1646.852 
      AICc: 3301.817
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.050562 
         Group size range: 1 to 3 
      Density in sampled area: 0.0003509428 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 1440620

# hazrate-NoCovarHermExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", series = "hermite")
      Coefficients:
                   Estimate     SE   z    p(>|z|)
      (Intercept)   5.30722410  NaN  NaN  NaN    
      k             0.50000000  NaN  NaN  NaN    
      a1           -0.23831400  NaN  NaN  NaN    
      a2            0.09501933  NaN  NaN  NaN    
      
      Message: FAILURE (Exit code= 1, false convergence (8))
      Function: HAZRATE with 2 expansion(s) of HERMITE series 
      Strip: 0 [m] to 207 [m] 
      Effective strip width (ESW): 50.20573 [m] 
      Probability of detection: 0.2425398
      Scaling: g(0 [m]) = 1
      Log likelihood: -1654.285 
      AICc: 3316.684
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.050562 
         Group size range: 1 to 3 
      Density in sampled area: NA
      Abundance in 4105 [km^2] study area: NA

# hazrate-NoCovarSimpExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", series = "simple")
      Coefficients:
                   Estimate   SE         z           p(>|z|)      
      (Intercept)   3.632143  0.1635637   22.206283  2.986485e-109
      k             1.531208  0.2547318    6.011061   1.843129e-09
      a1           -7.062476  0.2143129  -32.954033  3.703471e-238
      a2            6.770288  0.2463899   27.477942  3.221974e-166
      
      Message: Success; Asymptotic SE's
      Function: HAZRATE with 2 expansion(s) of SIMPLE series 
      Strip: 0 [m] to 207 [m] 
      Effective strip width (ESW): 57.62591 [m] 
      Probability of detection: 0.2783861
      Scaling: g(0 [m]) = 1
      Log likelihood: -1654.278 
      AICc: 3316.67
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.050562 
         Group size range: 1 to 3 
      Density in sampled area: 9.014077e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 370027.9

# hazrate-NoCovarBSplineExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", series = "bspline")
      Coefficients:
                   Estimate   SE   z    p(>|z|)
      (Intercept)   5.305273  NaN  NaN  NaN    
      k             0.500000  NaN  NaN  NaN    
      a1           -1.236699  NaN  NaN  NaN    
      a2           -0.953312  NaN  NaN  NaN    
      
      Message: FAILURE (Exit code= 1, false convergence (8))
      Function: HAZRATE with 2 expansion(s) of BSPLINE series 
      Strip: 0 [m] to 207 [m] 
      Effective strip width (ESW): 50.197 [m] 
      Probability of detection: 0.2424976
      Scaling: g(0 [m]) = 1
      Log likelihood: -1654.285 
      AICc: 3316.684
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.050562 
         Group size range: 1 to 3 
      Density in sampled area: NA
      Abundance in 4105 [km^2] study area: NA

# hazrate-ContCovarExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ bare +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m")
      Coefficients:
                   Estimate     SE           z          p(>|z|)     
      (Intercept)  3.726901877  0.203283173  18.333548  4.467848e-75
      bare         0.008458977  0.002507473   3.373506  7.421737e-04
      k            4.464618749  0.624885037   7.144704  9.018989e-13
      a1           0.228023774  0.358885679   0.635366  5.251897e-01
      a2           0.420252245  0.150636311   2.789847  5.273297e-03
      
      Message: Success; Asymptotic SE's
      Function: HAZRATE with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 207 [m] 
      Average effective strip width (ESW): 54.79514 [m] (range 45.49435 [m] to 65.39776 [m]) 
      Average probability of detection: 0.2647108 (range 0.2197795 to 0.3159312)
      Scaling: g(0 [m]) = 1
      Log likelihood: -1652.948 
      AICc: 3316.068
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.050562 
         Group size range: 1 to 3 
      Density in sampled area: 9.586432e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 393523.1

# hazrate-ContCovarExpansionsScaling

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ bare +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", x.scl = xScl, g.x.scl = gXscl)
      Coefficients:
                   Estimate     SE           z          p(>|z|)     
      (Intercept)  3.726901877  0.203283173  18.333548  4.467848e-75
      bare         0.008458977  0.002507473   3.373506  7.421737e-04
      k            4.464618749  0.624885037   7.144704  9.018989e-13
      a1           0.228023774  0.358885679   0.635366  5.251897e-01
      a2           0.420252245  0.150636311   2.789847  5.273297e-03
      
      Message: Success; Asymptotic SE's
      Function: HAZRATE with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 207 [m] 
      Average effective strip width (ESW): 54.79514 [m] (range 45.49435 [m] to 65.39776 [m]) 
      Average probability of detection: 0.2647108 (range 0.2197795 to 0.3159312)
      Scaling: g(0 [m]) = 0.75
      Log likelihood: -1652.948 
      AICc: 3316.068
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.050562 
         Group size range: 1 to 3 
      Density in sampled area: 9.586432e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 393523.1

# hazrate-Bootstraps

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~
         groupsize(groupsize), likelihood = lhood)
      Coefficients:
                   Estimate  SE         z         p(>|z|)      
      (Intercept)  3.852295  0.1128324  34.14174  1.773514e-255
      k            2.821360  0.2770262  10.18445   2.326694e-24
      
      Message: Success; Bootstrap SE's
      Function: HAZRATE  
      Strip: 0 [m] to 207 [m] 
      Effective strip width (ESW): 63.88645 [m] 
      Probability of detection: 0.3086302
      Scaling: g(0 [m]) = 1
      Log likelihood: -1665.104 
      AICc: 3334.242
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.050562 
         Group size range: 1 to 3 
      
      Density in sampled area: 8.130746e-05 [1/m^2]
                       95% CI: 6.559429e-05 [1/m^2] to 9.066089e-05 [1/m^2]
      
      Abundance in 4.105e+09 [m^2] study area: 333767.1
                                       95% CI: 269264.6 to 372163

