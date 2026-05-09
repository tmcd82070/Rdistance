# Gamma-MinimumInputs

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = dataDf, formula = dist ~ groupsize(groupsize),
         likelihood = lhood)
      Coefficients:
                   Estimate  SE       z      p(>|z|)  
      (Intercept)  4.055     0.04778  84.87  0.000e+00
      Shape        1.436     0.10310  13.93  4.439e-44
      
      Message: Success; Asymptotic SE's
      Function: GAMMA  
      Strip: 0 [m] to 207 [m] 
      Effective strip width (ESW): 57.54 [m] 
      Probability of detection: 0.278
      Scaling: g(max) = 1
      Log likelihood: -2124 
      AICc: 4251
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.051 
         Group size range: 1 to 3 
      Density in sampled area: 9.027e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 370548

# Gamma-NoCovar

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = dataDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, w.lo = w.lo, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = 0, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                   Estimate  SE       z      p(>|z|)  
      (Intercept)  4.061     0.05232  77.63  0.000e+00
      Shape        1.434     0.10950  13.10  3.434e-39
      
      Message: Success; Asymptotic SE's
      Function: GAMMA  
      Strip: 0 [m] to 150 [m] 
      Effective strip width (ESW): 57.16 [m] 
      Probability of detection: 0.381
      Scaling: g(max) = 1
      Log likelihood: -2092 
      AICc: 4187
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 371 in 353 groups 
       Average group size: 1.051 
         Group size range: 1 to 3 
      Density in sampled area: 9.015e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 370081

# Gamma-ContinuousCovar

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = dataDf, formula = dist ~ bare +
         groupsize(groupsize), likelihood = lhood, w.lo = w.lo, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = 0, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                   Estimate  SE        z      p(>|z|)  
      (Intercept)  3.42642   0.198390  17.27  7.764e-67
      bare         0.01111   0.003484   3.19  1.425e-03
      Shape        1.46586   0.111951  13.09  3.575e-39
      
      Message: Success; Asymptotic SE's
      Function: GAMMA  
      Strip: 0 [m] to 150 [m] 
      Average effective strip width (ESW): 57.34 [m] (range 43.15 [m] to 72.42 [m]) 
      Average probability of detection: 0.3823 (range 0.2877 to 0.4828)
      Scaling: g(max) = 1
      Log likelihood: -2086 
      AICc: 4179
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 371 in 353 groups 
       Average group size: 1.051 
         Group size range: 1 to 3 
      Density in sampled area: 9.191e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 377308

# Gamma-FactorCovar

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = dataDf, formula = dist ~ observer +
         groupsize(groupsize), likelihood = lhood, w.lo = w.lo, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = 0, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                    Estimate  SE      z        p(>|z|)   
      (Intercept)    4.09379  0.1123  36.4629  4.290e-291
      observerobs2   0.11621  0.1791   0.6489   5.164e-01
      observerobs3   0.07713  0.1591   0.4850   6.277e-01
      observerobs4  -0.34062  0.1610  -2.1154   3.440e-02
      observerobs5  -0.06582  0.1551  -0.4245   6.712e-01
      Shape          1.46002  0.1116  13.0816   4.195e-39
      
      Message: Success; Asymptotic SE's
      Function: GAMMA  
      Strip: 0 [m] to 150 [m] 
      Average effective strip width (ESW): 57.64 [m] (range 42.55 [m] to 65.45 [m]) 
      Average probability of detection: 0.3842 (range 0.2836 to 0.4364)
      Scaling: g(max) = 1
      Log likelihood: -2087 
      AICc: 4187
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 371 in 353 groups 
       Average group size: 1.051 
         Group size range: 1 to 3 
      Density in sampled area: 9.098e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 373487

# Gamma-NoCovarWloWhi

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = dataDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, w.lo = w.20, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = w.20, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                   Estimate  SE       z      p(>|z|)   
      (Intercept)  3.777     0.11076  34.10  7.816e-255
      Shape        1.093     0.09944  10.99   4.316e-28
      
      Message: Success; Asymptotic SE's
      Function: GAMMA  
      Strip: 20 [m] to 150 [m] 
      Effective strip width (ESW): 42.58 [m] 
      Probability of detection: 0.3275
      Scaling: g(max) = 1
      Log likelihood: -1044 
      AICc: 2091
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 241 in 233 groups 
       Average group size: 1.034 
         Group size range: 1 to 2 
      Density in sampled area: 7.861e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 322695

# Gamma-NoCovarWlo

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = dataDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, w.lo = w.20, expansions =
         0, series = "cosine", x.scl = w.20, g.x.scl = 1, outputUnits = "m")
      Coefficients:
                   Estimate  SE       z      p(>|z|)   
      (Intercept)  3.765     0.10816  34.81  1.681e-265
      Shape        1.098     0.09324  11.77   5.509e-32
      
      Message: Success; Asymptotic SE's
      Function: GAMMA  
      Strip: 20 [m] to 207 [m] 
      Effective strip width (ESW): 42.99 [m] 
      Probability of detection: 0.2299
      Scaling: g(max) = 1
      Log likelihood: -1074 
      AICc: 2153
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 244 in 236 groups 
       Average group size: 1.034 
         Group size range: 1 to 2 
      Density in sampled area: 7.883e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 323593

# Gamma-NoCovarsFt

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = dataDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, outputUnits = "ft")
      Coefficients:
                   Estimate  SE       z       p(>|z|)  
      (Intercept)  5.243     0.04778  109.74  0.000e+00
      Shape        1.436     0.10310   13.93  4.452e-44
      
      Message: Success; Asymptotic SE's
      Function: GAMMA  
      Strip: 0 [ft] to 679.1 [ft] 
      Effective strip width (ESW): 188.8 [ft] 
      Probability of detection: 0.278
      Scaling: g(max) = 1
      Log likelihood: -2526 
      AICc: 5057
      
           Surveyed Units: 118110 [ft] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.051 
         Group size range: 1 to 3 
      Density in sampled area: 8.386e-06 [1/ft^2]
      Abundance in 4.419e+10 [ft^2] study area: 370548

# Gamma-NoCovarCosExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = dataDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", series = "cosine")
      Coefficients:
                   Estimate  SE      z        p(>|z|)   
      (Intercept)   4.0089   0.1126  35.5921  1.856e-277
      Shape         1.4009   0.1620   8.6477   5.254e-18
      a1           -0.2179   0.4301  -0.5066   6.124e-01
      a2            0.1010   0.2215   0.4560   6.484e-01
      
      Message: Success; Asymptotic SE's
      Function: GAMMA with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 207 [m] 
      Effective strip width (ESW): 55.97 [m] 
      Probability of detection: 0.2704
      Scaling: g(max) = 1
      Log likelihood: -2122 
      AICc: 4253
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.051 
         Group size range: 1 to 3 
      Density in sampled area: 9.281e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 380993

# Gamma-NoCovarSinExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = dataDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", series = "sine")
      Coefficients:
                   Estimate  SE   z    p(>|z|)
      (Intercept)  4.0596    NaN  NaN  NaN    
      Shape        1.0000    NaN  NaN  NaN    
      a1           1.8546    NaN  NaN  NaN    
      a2           0.6436    NaN  NaN  NaN    
      
      Message: FAILURE (Exit code= 1, false convergence (8))
      Function: GAMMA with 2 expansion(s) of SINE series 
      Strip: 0 [m] to 207 [m] 
      Effective strip width (ESW): 82.9 [m] 
      Probability of detection: 0.4005
      Scaling: g(max) = 1
      Log likelihood: -2117 
      AICc: 4243
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.051 
         Group size range: 1 to 3 
      Density in sampled area: NA
      Abundance in 4105 [km^2] study area: NA

# Gamma-NoCovarHermExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = dataDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", series = "hermite")
      Coefficients:
                   Estimate    SE         z      p(>|z|)
      (Intercept)   3.2146561         NA     NA  NA     
      Shape         1.0000000  5.451e-05  18344   0     
      a1            4.2085255         NA     NA  NA     
      a2           -0.0001395         NA     NA  NA     
      
      Message: FAILURE (Exit code= -1, Shape parameter at lower boundary.)
      Function: GAMMA with 2 expansion(s) of HERMITE series 
      Strip: 0 [m] to 207 [m] 
      Effective strip width (ESW): 74.49 [m] 
      Probability of detection: 0.3599
      Scaling: g(max) = 1
      Log likelihood: -2122 
      AICc: 4252
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.051 
         Group size range: 1 to 3 
      Density in sampled area: NA
      Abundance in 4105 [km^2] study area: NA

# Gamma-NoCovarSimpExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = dataDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", series = "simple")
      Coefficients:
                   Estimate  SE       z      p(>|z|) 
      (Intercept)   4.252    0.08696  48.89  0.00e+00
      Shape         1.166    0.11623  10.03  1.08e-23
      a1           -7.290         NA     NA        NA
      a2            7.414         NA     NA        NA
      
      Message: VARIANCE FAILURE (singular variance-covariance matrix)
      Function: GAMMA with 2 expansion(s) of SIMPLE series 
      Strip: 0 [m] to 207 [m] 
      Effective strip width (ESW): 60.06 [m] 
      Probability of detection: 0.2901
      Scaling: g(max) = 1
      Log likelihood: -2117 
      AICc: 4241
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.051 
         Group size range: 1 to 3 
      Density in sampled area: 8.649e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 355029

# Gamma-NoCovarBSplineExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = dataDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", series = "bspline")
      Coefficients:
                   Estimate  SE         z          p(>|z|)  
      (Intercept)   3.215    2.194e-01  1.465e+01  1.308e-48
      Shape         1.000    5.451e-05  1.834e+04  0.000e+00
      a1            8.418    4.031e+00  2.088e+00  3.677e-02
      a2           16.845    3.192e+01  5.277e-01  5.977e-01
      
      Message: FAILURE (Exit code= -1, Shape parameter at lower boundary.)
      Function: GAMMA with 2 expansion(s) of BSPLINE series 
      Strip: 0 [m] to 207 [m] 
      Effective strip width (ESW): 74.49 [m] 
      Probability of detection: 0.3599
      Scaling: g(max) = 1
      Log likelihood: -2122 
      AICc: 4252
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.051 
         Group size range: 1 to 3 
      Density in sampled area: NA
      Abundance in 4105 [km^2] study area: NA

# Gamma-ContCovarExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = dataDf, formula = dist ~ bare +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m")
      Coefficients:
                   Estimate  SE        z        p(>|z|)  
      (Intercept)   3.33261  0.210978  15.7960  3.317e-56
      bare          0.01132  0.003128   3.6205  2.940e-04
      Shape         1.43135  0.165866   8.6295  6.160e-18
      a1           -0.35731  0.492433  -0.7256  4.681e-01
      a2            0.10013  0.236688   0.4231  6.723e-01
      
      Message: Success; Asymptotic SE's
      Function: GAMMA with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 207 [m] 
      Average effective strip width (ESW): 57.17 [m] (range 40.91 [m] to 75.7 [m]) 
      Average probability of detection: 0.2762 (range 0.1976 to 0.3657)
      Scaling: g(max) = 1
      Log likelihood: -2116 
      AICc: 4242
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.051 
         Group size range: 1 to 3 
      Density in sampled area: 9.376e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 384868

# Gamma-ContCovarExpansionsWScaling

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = dataDf, formula = dist ~ bare +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", x.scl = xScl, g.x.scl = gXscl)
      Coefficients:
                   Estimate  SE        z        p(>|z|)  
      (Intercept)   3.33261  0.210978  15.7960  3.317e-56
      bare          0.01132  0.003128   3.6205  2.940e-04
      Shape         1.43135  0.165866   8.6295  6.160e-18
      a1           -0.35731  0.492433  -0.7256  4.681e-01
      a2            0.10013  0.236688   0.4231  6.723e-01
      
      Message: Success; Asymptotic SE's
      Function: GAMMA with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 207 [m] 
      Average effective strip width (ESW): 42.88 [m] (range 30.68 [m] to 56.77 [m]) 
      Average probability of detection: 0.2072 (range 0.1482 to 0.2743)
      Scaling: g(max) = 0.75
      Log likelihood: -2116 
      AICc: 4242
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.051 
         Group size range: 1 to 3 
      Density in sampled area: 0.000125 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 513157

# Gamma-SerialBootstraps

    Code
      summary(abun)
    Output
      Call: dfuncEstim(data = dataDf, formula = dist ~ groupsize(groupsize),
         likelihood = lhood)
      Coefficients:
                   Estimate  SE      z      p(>|z|)  
      (Intercept)  4.055     0.0646  62.77  0.000e+00
      Shape        1.436     0.1177  12.19  3.379e-34
      
      Message: Success; Bootstrap SE's
      Function: GAMMA  
      Strip: 0 [m] to 207 [m] 
      Effective strip width (ESW): 57.54 [m] 
      Probability of detection: 0.278
      Scaling: g(max) = 1
      Log likelihood: -2124 
      AICc: 4251
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.051 
         Group size range: 1 to 3 
      
      Density in sampled area: 9.027e-05 [1/m^2]
                       95% CI: 7.092e-05 [1/m^2] to 0.0001047 [1/m^2]
      
      Abundance in 4.105e+09 [m^2] study area: 370548
                                       95% CI: 291110 to 429966

