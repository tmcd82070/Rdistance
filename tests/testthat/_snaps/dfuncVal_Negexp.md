# negexp-MinimumInputs

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = dataDf, formula = dist ~ groupsize(groupsize),
         likelihood = lhood)
      Coefficients:
                   Estimate  SE       z    p(>|z|)
      (Intercept)  -3.713    0.05801  -64  0      
      
      Message: Success; Asymptotic SE's
      Function: NEGEXP  
      Strip: 0 [m] to 207 [m] 
      Effective strip width (ESW): 40.71 [m] 
      Probability of detection: 0.1967
      Scaling: g(0 [m]) = 1
      Log likelihood: -1664 
      AICc: 3330
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.051 
         Group size range: 1 to 3 
      Density in sampled area: 0.0001276 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 523742

# negexp-NoCovar

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = dataDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, w.lo = w.lo, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = 0, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                   Estimate  SE       z       p(>|z|)
      (Intercept)  -3.761    0.06845  -54.94  0      
      
      Message: Success; Asymptotic SE's
      Function: NEGEXP  
      Strip: 0 [m] to 150 [m] 
      Effective strip width (ESW): 41.68 [m] 
      Probability of detection: 0.2778
      Scaling: g(0 [m]) = 1
      Log likelihood: -1631 
      AICc: 3264
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 371 in 353 groups 
       Average group size: 1.051 
         Group size range: 1 to 3 
      Density in sampled area: 0.0001236 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 507523

# negexp-ContinuousCovar

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = dataDf, formula = dist ~ bare +
         groupsize(groupsize), likelihood = lhood, w.lo = w.lo, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = 0, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                   Estimate  SE        z        p(>|z|)  
      (Intercept)  -2.98953  0.252782  -11.827  2.847e-32
      bare         -0.01359  0.004522   -3.006  2.650e-03
      
      Message: Success; Asymptotic SE's
      Function: NEGEXP  
      Strip: 0 [m] to 150 [m] 
      Average effective strip width (ESW): 42.01 [m] (range 29.96 [m] to 54.96 [m]) 
      Average probability of detection: 0.2801 (range 0.1997 to 0.3664)
      Scaling: g(0 [m]) = 1
      Log likelihood: -1626 
      AICc: 3257
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 371 in 353 groups 
       Average group size: 1.051 
         Group size range: 1 to 3 
      Density in sampled area: 0.0001265 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 519247

# negexp-FactorCovar

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = dataDf, formula = dist ~ observer +
         groupsize(groupsize), likelihood = lhood, w.lo = w.lo, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = 0, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                    Estimate  SE      z          p(>|z|)   
      (Intercept)   -3.81382  0.1522  -25.06236  1.280e-138
      observerobs2  -0.14930  0.2486   -0.60062   5.481e-01
      observerobs3   0.04892  0.2045    0.23917   8.110e-01
      observerobs4   0.36578  0.2121    1.72463   8.459e-02
      observerobs5   0.01951  0.2139    0.09117   9.274e-01
      
      Message: Success; Asymptotic SE's
      Function: NEGEXP  
      Strip: 0 [m] to 150 [m] 
      Average effective strip width (ESW): 41.95 [m] (range 31.17 [m] to 49.58 [m]) 
      Average probability of detection: 0.2796 (range 0.2078 to 0.3305)
      Scaling: g(0 [m]) = 1
      Log likelihood: -1628 
      AICc: 3267
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 371 in 353 groups 
       Average group size: 1.051 
         Group size range: 1 to 3 
      Density in sampled area: 0.0001249 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 512783

# negexp-NoCovarWloWhi

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = dataDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, w.lo = w.20, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = w.20, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                   Estimate  SE       z       p(>|z|)
      (Intercept)  -3.793    0.07265  -52.21  0      
      
      Message: Success; Asymptotic SE's
      Function: NEGEXP  
      Strip: 2 [m] to 150 [m] 
      Effective strip width (ESW): 42.79 [m] 
      Probability of detection: 0.2891
      Scaling: g(2 [m]) = 1
      Log likelihood: -1529 
      AICc: 3060
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 347 in 330 groups 
       Average group size: 1.052 
         Group size range: 1 to 3 
      Density in sampled area: 0.0001126 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 462308

# negexp-NoCovarWlo

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = dataDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, w.lo = w.20, expansions =
         0, series = "cosine", x.scl = w.20, g.x.scl = 1, outputUnits = "m")
      Coefficients:
                   Estimate  SE      z       p(>|z|)
      (Intercept)  -3.735    0.0606  -61.64  0      
      
      Message: Success; Asymptotic SE's
      Function: NEGEXP  
      Strip: 2 [m] to 207 [m] 
      Effective strip width (ESW): 41.59 [m] 
      Probability of detection: 0.2029
      Scaling: g(2 [m]) = 1
      Log likelihood: -1562 
      AICc: 3126
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 350 in 333 groups 
       Average group size: 1.051 
         Group size range: 1 to 3 
      Density in sampled area: 0.0001169 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 479764

# negexp-NoCovarsFt

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = dataDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, outputUnits = "ft")
      Coefficients:
                   Estimate  SE       z       p(>|z|)
      (Intercept)  -4.901    0.05801  -84.48  0      
      
      Message: Success; Asymptotic SE's
      Function: NEGEXP  
      Strip: 0 [ft] to 679.1 [ft] 
      Effective strip width (ESW): 133.6 [ft] 
      Probability of detection: 0.1967
      Scaling: g(0 [ft]) = 1
      Log likelihood: -2087 
      AICc: 4176
      
           Surveyed Units: 118110 [ft] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.051 
         Group size range: 1 to 3 
      Density in sampled area: 1.185e-05 [1/ft^2]
      Abundance in 4.419e+10 [ft^2] study area: 523742

# negexp-NoCovarCosExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = dataDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", series = "cosine")
      Coefficients:
                   Estimate  SE      z         p(>|z|)   
      (Intercept)  -3.5111   0.1108  -31.6912  2.056e-220
      a1           -0.8844   0.6802   -1.3002   1.935e-01
      a2           -0.2557   0.2625   -0.9741   3.300e-01
      
      Message: Success; Asymptotic SE's
      Function: NEGEXP with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 207 [m] 
      Effective strip width (ESW): 44.36 [m] 
      Probability of detection: 0.2143
      Scaling: g(0 [m]) = 1
      Log likelihood: -1661 
      AICc: 3328
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.051 
         Group size range: 1 to 3 
      Density in sampled area: 0.0001171 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 480658

# negexp-NoCovarSinExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = dataDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", series = "sine")
      Coefficients:
                   Estimate  SE      z         p(>|z|)   
      (Intercept)  -3.79592  0.1005  -37.7742  3.019e-312
      a1            1.04975  0.4215    2.4903   1.277e-02
      a2            0.06639  0.2981    0.2227   8.238e-01
      
      Message: Success; Asymptotic SE's
      Function: NEGEXP with 2 expansion(s) of SINE series 
      Strip: 0 [m] to 207 [m] 
      Effective strip width (ESW): 55.64 [m] 
      Probability of detection: 0.2688
      Scaling: g(0 [m]) = 1
      Log likelihood: -1659 
      AICc: 3324
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.051 
         Group size range: 1 to 3 
      Density in sampled area: 9.336e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 383263

# negexp-NoCovarHermExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = dataDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", series = "hermite")
      Coefficients:
                   Estimate  SE      z         p(>|z|)  
      (Intercept)  -3.3678   0.3254  -10.3502  4.177e-25
      a1            0.9412   3.1094    0.3027  7.621e-01
      a2           -0.1473   0.5760   -0.2558  7.981e-01
      
      Message: Success; Asymptotic SE's
      Function: NEGEXP with 2 expansion(s) of HERMITE series 
      Strip: 0 [m] to 207 [m] 
      Effective strip width (ESW): 51.11 [m] 
      Probability of detection: 0.2469
      Scaling: g(0 [m]) = 1
      Log likelihood: -1661 
      AICc: 3328
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.051 
         Group size range: 1 to 3 
      Density in sampled area: 0.0001016 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 417173

# negexp-NoCovarSimpExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = dataDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", series = "simple")
      Coefficients:
                   Estimate  SE   z    p(>|z|)
      (Intercept)  -4.035    NaN  NaN  NaN    
      a1           -7.257    NaN  NaN  NaN    
      a2            7.281    NaN  NaN  NaN    
      
      Message: FAILURE (Exit code= 1, false convergence (8))
      Function: NEGEXP with 2 expansion(s) of SIMPLE series 
      Strip: 0 [m] to 207 [m] 
      Effective strip width (ESW): 48.05 [m] 
      Probability of detection: 0.2321
      Scaling: g(0 [m]) = 1
      Log likelihood: -1654 
      AICc: 3315
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.051 
         Group size range: 1 to 3 
      Density in sampled area: NA
      Abundance in 4105 [km^2] study area: NA

# negexp-NoCovarBSplineExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = dataDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", series = "bspline")
      Coefficients:
                   Estimate  SE       z         p(>|z|)  
      (Intercept)  -3.368     0.3279  -10.2699  9.635e-25
      a1            3.062     2.2336    1.3708  1.704e-01
      a2            3.775    12.5602    0.3006  7.637e-01
      
      Message: Success; Asymptotic SE's
      Function: NEGEXP with 2 expansion(s) of BSPLINE series 
      Strip: 0 [m] to 207 [m] 
      Effective strip width (ESW): 51.11 [m] 
      Probability of detection: 0.2469
      Scaling: g(0 [m]) = 1
      Log likelihood: -1661 
      AICc: 3328
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.051 
         Group size range: 1 to 3 
      Density in sampled area: 0.0001016 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 417163

# negexp-ContCovarExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = dataDf, formula = dist ~ bare +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m")
      Coefficients:
                   Estimate  SE        z        p(>|z|)  
      (Intercept)  -2.73058  0.210091  -12.997  1.270e-38
      bare         -0.01278  0.003442   -3.712  2.054e-04
      a1           -1.21937  0.830717   -1.468  1.421e-01
      a2           -0.31684  0.295890   -1.071  2.843e-01
      
      Message: Success; Asymptotic SE's
      Function: NEGEXP with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 207 [m] 
      Average effective strip width (ESW): 45.87 [m] (range 29.53 [m] to 65.37 [m]) 
      Average probability of detection: 0.2216 (range 0.1426 to 0.3158)
      Scaling: g(0 [m]) = 1
      Log likelihood: -1654 
      AICc: 3316
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.051 
         Group size range: 1 to 3 
      Density in sampled area: 0.0001193 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 489639

# negexp-ContCovarExpansionsWScaling

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = dataDf, formula = dist ~ bare +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", x.scl = xScl, g.x.scl = gXscl)
      Coefficients:
                   Estimate  SE        z        p(>|z|)  
      (Intercept)  -2.73058  0.210091  -12.997  1.270e-38
      bare         -0.01278  0.003442   -3.712  2.054e-04
      a1           -1.21937  0.830717   -1.468  1.421e-01
      a2           -0.31684  0.295890   -1.071  2.843e-01
      
      Message: Success; Asymptotic SE's
      Function: NEGEXP with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 207 [m] 
      Average effective strip width (ESW): 34.4 [m] (range 22.15 [m] to 49.02 [m]) 
      Average probability of detection: 0.1662 (range 0.107 to 0.2368)
      Scaling: g(0 [m]) = 0.75
      Log likelihood: -1654 
      AICc: 3316
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.051 
         Group size range: 1 to 3 
      Density in sampled area: 0.000159 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 652851

# negexp-SerialBootstraps

    Code
      summary(abun)
    Output
      Call: dfuncEstim(data = dataDf, formula = dist ~ groupsize(groupsize),
         likelihood = lhood)
      Coefficients:
                   Estimate  SE       z       p(>|z|)
      (Intercept)  -3.713    0.07079  -52.45  0      
      
      Message: Success; Bootstrap SE's
      Function: NEGEXP  
      Strip: 0 [m] to 207 [m] 
      Effective strip width (ESW): 40.71 [m] 
      Probability of detection: 0.1967
      Scaling: g(0 [m]) = 1
      Log likelihood: -1664 
      AICc: 3330
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.051 
         Group size range: 1 to 3 
      
      Density in sampled area: 0.0001276 [1/m^2]
                       95% CI: 9.926e-05 [1/m^2] to 0.0001486 [1/m^2]
      
      Abundance in 4.105e+09 [m^2] study area: 523742
                                       95% CI: 407470 to 610024

