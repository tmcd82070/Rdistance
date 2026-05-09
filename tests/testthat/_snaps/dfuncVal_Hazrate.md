# hazrate-MinimumInputs

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = dataDf, formula = dist ~ groupsize(groupsize),
         likelihood = lhood)
      Coefficients:
                   Estimate  SE       z       p(>|z|) 
      (Intercept)  3.852     0.09955  38.696  0.00e+00
      k            2.821     0.32413   8.703  3.22e-18
      
      Message: Success; Asymptotic SE's
      Function: HAZRATE  
      Strip: 0 [m] to 207 [m] 
      Effective strip width (ESW): 63.88 [m] 
      Probability of detection: 0.3086
      Scaling: g(0 [m]) = 1
      Log likelihood: -1665 
      AICc: 3334
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.051 
         Group size range: 1 to 3 
      Density in sampled area: 8.131e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 333782

# hazrate-NoCovar

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = dataDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, w.lo = w.lo, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = 0, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                   Estimate  SE      z      p(>|z|)   
      (Intercept)  3.775     0.1079  34.98  4.838e-268
      k            2.406     0.3097   7.77   7.862e-15
      
      Message: Success; Asymptotic SE's
      Function: HAZRATE  
      Strip: 0 [m] to 150 [m] 
      Effective strip width (ESW): 61.12 [m] 
      Probability of detection: 0.4074
      Scaling: g(0 [m]) = 1
      Log likelihood: -1632 
      AICc: 3268
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 371 in 353 groups 
       Average group size: 1.051 
         Group size range: 1 to 3 
      Density in sampled area: 8.431e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 346095

# hazrate-ContinuousCovar

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = dataDf, formula = dist ~ bare +
         groupsize(groupsize), likelihood = lhood, w.lo = w.lo, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = 0, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                   Estimate  SE        z       p(>|z|)  
      (Intercept)  3.06244   0.275717  11.107  1.158e-28
      bare         0.01325   0.004248   3.119  1.812e-03
      k            2.54788   0.346483   7.354  1.930e-13
      
      Message: Success; Asymptotic SE's
      Function: HAZRATE  
      Strip: 0 [m] to 150 [m] 
      Average effective strip width (ESW): 62.79 [m] (range 45.37 [m] to 81.93 [m]) 
      Average probability of detection: 0.4186 (range 0.3025 to 0.5462)
      Scaling: g(0 [m]) = 1
      Log likelihood: -1627 
      AICc: 3260
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 371 in 353 groups 
       Average group size: 1.051 
         Group size range: 1 to 3 
      Density in sampled area: 8.449e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 346820

# hazrate-FactorCovar

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = dataDf, formula = dist ~ observer +
         groupsize(groupsize), likelihood = lhood, w.lo = w.lo, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = 0, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                    Estimate  SE      z        p(>|z|)   
      (Intercept)    3.91559  0.1444  27.1104  7.423e-162
      observerobs2   0.17190  0.2028   0.8477   3.966e-01
      observerobs3   0.06576  0.1658   0.3966   6.917e-01
      observerobs4  -0.38574  0.1757  -2.1953   2.814e-02
      observerobs5  -0.09584  0.1671  -0.5734   5.664e-01
      k              2.76472  0.3481   7.9416   1.997e-15
      
      Message: Success; Asymptotic SE's
      Function: HAZRATE  
      Strip: 0 [m] to 150 [m] 
      Average effective strip width (ESW): 64.83 [m] (range 46.61 [m] to 77.35 [m]) 
      Average probability of detection: 0.4322 (range 0.3107 to 0.5157)
      Scaling: g(0 [m]) = 1
      Log likelihood: -1628 
      AICc: 3267
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 371 in 353 groups 
       Average group size: 1.051 
         Group size range: 1 to 3 
      Density in sampled area: 8.125e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 333528

# hazrate-NoCovarWloWhi

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = dataDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, w.lo = w.20, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = w.20, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                   Estimate  SE      z       p(>|z|)   
      (Intercept)  3.814     0.1118  34.120  3.726e-255
      k            2.482     0.3485   7.121   1.071e-12
      
      Message: Success; Asymptotic SE's
      Function: HAZRATE  
      Strip: 2 [m] to 150 [m] 
      Effective strip width (ESW): 62.59 [m] 
      Probability of detection: 0.4229
      Scaling: g(2 [m]) = 1
      Log likelihood: -1526 
      AICc: 3056
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 347 in 330 groups 
       Average group size: 1.052 
         Group size range: 1 to 3 
      Density in sampled area: 7.7e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 316096

# hazrate-NoCovarWlo

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = dataDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, w.lo = w.20, expansions =
         0, series = "cosine", x.scl = w.20, g.x.scl = 1, outputUnits = "m")
      Coefficients:
                   Estimate  SE      z       p(>|z|)   
      (Intercept)  3.895     0.1032  37.754  6.544e-312
      k            2.934     0.3697   7.936   2.081e-15
      
      Message: Success; Asymptotic SE's
      Function: HAZRATE  
      Strip: 2 [m] to 207 [m] 
      Effective strip width (ESW): 65.62 [m] 
      Probability of detection: 0.3201
      Scaling: g(2 [m]) = 1
      Log likelihood: -1559 
      AICc: 3122
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 350 in 333 groups 
       Average group size: 1.051 
         Group size range: 1 to 3 
      Density in sampled area: 7.408e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 304103

# hazrate-NoCovarsFt

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = dataDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, outputUnits = "ft")
      Coefficients:
                   Estimate  SE       z       p(>|z|) 
      (Intercept)  5.040     0.09955  50.633  0.00e+00
      k            2.821     0.32411   8.704  3.21e-18
      
      Message: Success; Asymptotic SE's
      Function: HAZRATE  
      Strip: 0 [ft] to 679.1 [ft] 
      Effective strip width (ESW): 209.6 [ft] 
      Probability of detection: 0.3086
      Scaling: g(0 [ft]) = 1
      Log likelihood: -2088 
      AICc: 4180
      
           Surveyed Units: 118110 [ft] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.051 
         Group size range: 1 to 3 
      Density in sampled area: 7.554e-06 [1/ft^2]
      Abundance in 4.419e+10 [ft^2] study area: 333782

# hazrate-NoCovarCosExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = dataDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", series = "cosine")
      Coefficients:
                   Estimate  SE      z        p(>|z|)   
      (Intercept)  4.131     0.1940  21.2946  1.275e-100
      k            4.109     0.6069   6.7692   1.295e-11
      a1           0.036     0.5893   0.0611   9.513e-01
      a2           0.496     0.2061   2.4069   1.609e-02
      
      Message: Success; Asymptotic SE's
      Function: HAZRATE with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 207 [m] 
      Effective strip width (ESW): 53.91 [m] 
      Probability of detection: 0.2604
      Scaling: g(0 [m]) = 1
      Log likelihood: -1659 
      AICc: 3326
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.051 
         Group size range: 1 to 3 
      Density in sampled area: 9.635e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 395523

# hazrate-NoCovarSinExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = dataDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", series = "sine")
      Coefficients:
                   Estimate  SE       z       p(>|z|)  
      (Intercept)  -1.5317   0.94723  -1.617  1.059e-01
      k             0.5338   0.06643   8.036  9.301e-16
      a1            2.6708   0.46090   5.795  6.844e-09
      a2            1.1831   0.54475   2.172  2.988e-02
      
      Message: Success; Asymptotic SE's
      Function: HAZRATE with 2 expansion(s) of SINE series 
      Strip: 0 [m] to 207 [m] 
      Effective strip width (ESW): 14.8 [m] 
      Probability of detection: 0.0715
      Scaling: g(0 [m]) = 1
      Log likelihood: -1647 
      AICc: 3302
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.051 
         Group size range: 1 to 3 
      Density in sampled area: 0.0003509 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 1440620

# hazrate-NoCovarHermExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = dataDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", series = "hermite")
      Coefficients:
                   Estimate  SE   z    p(>|z|)
      (Intercept)   5.30722  NaN  NaN  NaN    
      k             0.50000  NaN  NaN  NaN    
      a1           -0.23831  NaN  NaN  NaN    
      a2            0.09502  NaN  NaN  NaN    
      
      Message: FAILURE (Exit code= 1, false convergence (8))
      Function: HAZRATE with 2 expansion(s) of HERMITE series 
      Strip: 0 [m] to 207 [m] 
      Effective strip width (ESW): 50.21 [m] 
      Probability of detection: 0.2425
      Scaling: g(0 [m]) = 1
      Log likelihood: -1654 
      AICc: 3317
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.051 
         Group size range: 1 to 3 
      Density in sampled area: NA
      Abundance in 4105 [km^2] study area: NA

# hazrate-NoCovarSimpExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = dataDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", series = "simple")
      Coefficients:
                   Estimate  SE      z        p(>|z|)   
      (Intercept)   3.632    0.1636   22.206  2.986e-109
      k             1.531    0.2547    6.011   1.843e-09
      a1           -7.062    0.2143  -32.954  3.703e-238
      a2            6.770    0.2464   27.478  3.222e-166
      
      Message: Success; Asymptotic SE's
      Function: HAZRATE with 2 expansion(s) of SIMPLE series 
      Strip: 0 [m] to 207 [m] 
      Effective strip width (ESW): 57.63 [m] 
      Probability of detection: 0.2784
      Scaling: g(0 [m]) = 1
      Log likelihood: -1654 
      AICc: 3317
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.051 
         Group size range: 1 to 3 
      Density in sampled area: 9.014e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 370028

# hazrate-NoCovarBSplineExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = dataDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", series = "bspline")
      Coefficients:
                   Estimate  SE   z    p(>|z|)
      (Intercept)   5.3053   NaN  NaN  NaN    
      k             0.5000   NaN  NaN  NaN    
      a1           -1.2367   NaN  NaN  NaN    
      a2           -0.9533   NaN  NaN  NaN    
      
      Message: FAILURE (Exit code= 1, false convergence (8))
      Function: HAZRATE with 2 expansion(s) of BSPLINE series 
      Strip: 0 [m] to 207 [m] 
      Effective strip width (ESW): 50.2 [m] 
      Probability of detection: 0.2425
      Scaling: g(0 [m]) = 1
      Log likelihood: -1654 
      AICc: 3317
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.051 
         Group size range: 1 to 3 
      Density in sampled area: NA
      Abundance in 4105 [km^2] study area: NA

# hazrate-ContCovarExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = dataDf, formula = dist ~ bare +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m")
      Coefficients:
                   Estimate  SE        z        p(>|z|)  
      (Intercept)  3.726902  0.203283  18.3335  4.468e-75
      bare         0.008459  0.002507   3.3735  7.422e-04
      k            4.464619  0.624885   7.1447  9.019e-13
      a1           0.228024  0.358886   0.6354  5.252e-01
      a2           0.420252  0.150636   2.7898  5.273e-03
      
      Message: Success; Asymptotic SE's
      Function: HAZRATE with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 207 [m] 
      Average effective strip width (ESW): 54.8 [m] (range 45.49 [m] to 65.4 [m]) 
      Average probability of detection: 0.2647 (range 0.2198 to 0.3159)
      Scaling: g(0 [m]) = 1
      Log likelihood: -1653 
      AICc: 3316
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.051 
         Group size range: 1 to 3 
      Density in sampled area: 9.586e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 393523

# hazrate-ContCovarExpansionsWScaling

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = dataDf, formula = dist ~ bare +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", x.scl = xScl, g.x.scl = gXscl)
      Coefficients:
                   Estimate  SE        z        p(>|z|)  
      (Intercept)  3.726902  0.203283  18.3335  4.468e-75
      bare         0.008459  0.002507   3.3735  7.422e-04
      k            4.464619  0.624885   7.1447  9.019e-13
      a1           0.228024  0.358886   0.6354  5.252e-01
      a2           0.420252  0.150636   2.7898  5.273e-03
      
      Message: Success; Asymptotic SE's
      Function: HAZRATE with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 207 [m] 
      Average effective strip width (ESW): 41.1 [m] (range 34.12 [m] to 49.05 [m]) 
      Average probability of detection: 0.1985 (range 0.1648 to 0.2369)
      Scaling: g(0 [m]) = 0.75
      Log likelihood: -1653 
      AICc: 3316
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.051 
         Group size range: 1 to 3 
      Density in sampled area: 0.0001278 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 524697

# hazrate-SerialBootstraps

    Code
      summary(abun)
    Output
      Call: dfuncEstim(data = dataDf, formula = dist ~ groupsize(groupsize),
         likelihood = lhood)
      Coefficients:
                   Estimate  SE      z       p(>|z|)   
      (Intercept)  3.852     0.1430  26.940  7.548e-160
      k            2.821     0.4313   6.541   6.111e-11
      
      Message: Success; Bootstrap SE's
      Function: HAZRATE  
      Strip: 0 [m] to 207 [m] 
      Effective strip width (ESW): 63.88 [m] 
      Probability of detection: 0.3086
      Scaling: g(0 [m]) = 1
      Log likelihood: -1665 
      AICc: 3334
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.051 
         Group size range: 1 to 3 
      
      Density in sampled area: 8.131e-05 [1/m^2]
                       95% CI: 6.331e-05 [1/m^2] to 0.0001033 [1/m^2]
      
      Abundance in 4.105e+09 [m^2] study area: 333782
                                       95% CI: 259888 to 424154

