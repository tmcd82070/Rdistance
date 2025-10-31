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
                   Estimate   SE         z            p(>|z|)      
      (Intercept)  4.1307652  0.1939809  21.29470401  1.271067e-100
      k            4.1084424  0.6069392   6.76911637   1.295713e-11
      a1           0.0360823  0.5892126   0.06123817   9.511695e-01
      a2           0.4959223  0.2060392   2.40693143   1.608719e-02
      
      Message: Success; Asymptotic SE's
      Function: HAZRATE with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 207 [m] 
      Effective strip width (ESW): 53.91189 [m] 
      Probability of detection: 0.2604439
      Scaling: g(0 [m]) = 1
      Log likelihood: -1658.738 
      AICc: 3325.589
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.050562 
         Group size range: 1 to 3 
      Density in sampled area: 9.635062e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 395519.3

# hazrate-NoCovarSinExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", series = "sine")
      Coefficients:
                   Estimate   SE          z          p(>|z|)     
      (Intercept)  -4.504993  0.95871561  -4.698987  2.614545e-06
      k             0.500000  0.04774123  10.473127  1.147868e-25
      a1            2.763875  0.19807710  13.953530  2.994113e-44
      a2            1.297015  0.28476784   4.554641  5.247518e-06
      
      Message: FAILURE (Exit code= -1, k parameter at lower boundary.)
      Function: HAZRATE with 2 expansion(s) of SINE series 
      Strip: 0 [m] to 207 [m] 
      Effective strip width (ESW): 4.240554 [m] 
      Probability of detection: 0.02048577
      Scaling: g(0 [m]) = 1
      Log likelihood: -1632.87 
      AICc: 3273.853
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.050562 
         Group size range: 1 to 3 
      Density in sampled area: NA
      Abundance in 4105 [km^2] study area: NA

# hazrate-NoCovarHermExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", series = "hermite")
      Coefficients:
                   Estimate     SE           z            p(>|z|)      
      (Intercept)   5.30194512  1.712613456    3.0958212   1.962686e-03
      k             0.50000000  1.140243863    0.4385027   6.610219e-01
      a1           -0.23832360  0.006952997  -34.2763841  1.764767e-257
      a2            0.09497356  0.007102924   13.3710507   8.927082e-41
      
      Message: FAILURE (Exit code= -1, k parameter at lower boundary.)
      Function: HAZRATE with 2 expansion(s) of HERMITE series 
      Strip: 0 [m] to 207 [m] 
      Effective strip width (ESW): 50.19218 [m] 
      Probability of detection: 0.2424743
      Scaling: g(0 [m]) = 1
      Log likelihood: -1654.287 
      AICc: 3316.688
      
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
                   Estimate   SE         z          p(>|z|)      
      (Intercept)   3.635155  0.1633653  22.251694  1.086116e-109
      k             1.539828  0.2846308   5.409913   6.305532e-08
      a1           -7.063818  2.4008474  -2.942218   3.258699e-03
      a2            6.789600  3.0348100   2.237240   2.527063e-02
      
      Message: Success; Asymptotic SE's
      Function: HAZRATE with 2 expansion(s) of SIMPLE series 
      Strip: 0 [m] to 207 [m] 
      Effective strip width (ESW): 57.68989 [m] 
      Probability of detection: 0.2786951
      Scaling: g(0 [m]) = 1
      Log likelihood: -1654.28 
      AICc: 3316.674
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.050562 
         Group size range: 1 to 3 
      Density in sampled area: 9.004081e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 369617.5

# hazrate-ContCovarExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ bare +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m")
      Coefficients:
                   Estimate    SE           z           p(>|z|)     
      (Intercept)  3.72688597  0.203285964  18.3332184  4.495040e-75
      bare         0.00845914  0.002507488   3.3735511  7.420528e-04
      k            4.46459205  0.624874962   7.1447767  9.014232e-13
      a1           0.22800046  0.358909008   0.6352598  5.252590e-01
      a2           0.42026153  0.150643142   2.7897820  5.274353e-03
      
      Message: Success; Asymptotic SE's
      Function: HAZRATE with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 207 [m] 
      Average effective strip width (ESW): 54.79522 [m] (range 45.49414 [m] to 65.39822 [m]) 
      Average probability of detection: 0.2647112 (range 0.2197785 to 0.3159334)
      Scaling: g(0 [m]) = 1
      Log likelihood: -1652.948 
      AICc: 3316.068
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.050562 
         Group size range: 1 to 3 
      Density in sampled area: 9.586425e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 393522.8

# hazrate-ContCovarExpansionsScaling

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ bare +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", x.scl = xScl, g.x.scl = gXscl)
      Coefficients:
                   Estimate    SE           z           p(>|z|)     
      (Intercept)  3.72688597  0.203285964  18.3332184  4.495040e-75
      bare         0.00845914  0.002507488   3.3735511  7.420528e-04
      k            4.46459205  0.624874962   7.1447767  9.014232e-13
      a1           0.22800046  0.358909008   0.6352598  5.252590e-01
      a2           0.42026153  0.150643142   2.7897820  5.274353e-03
      
      Message: Success; Asymptotic SE's
      Function: HAZRATE with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 207 [m] 
      Average effective strip width (ESW): 54.79522 [m] (range 45.49414 [m] to 65.39822 [m]) 
      Average probability of detection: 0.2647112 (range 0.2197785 to 0.3159334)
      Scaling: g(0 [m]) = 0.75
      Log likelihood: -1652.948 
      AICc: 3316.068
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.050562 
         Group size range: 1 to 3 
      Density in sampled area: 9.586425e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 393522.8

# hazrate-Bootstraps

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~
         groupsize(groupsize), likelihood = lhood)
      Coefficients:
                   Estimate  SE         z          p(>|z|)      
      (Intercept)  3.852295  0.1428392  26.969444  3.374648e-160
      k            2.821360  0.4309208   6.547282   5.859362e-11
      
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
                       95% CI: 6.330966e-05 [1/m^2] to 0.0001032802 [1/m^2]
      
      Abundance in 4.105e+09 [m^2] study area: 333767.1
                                       95% CI: 259886.2 to 423965.2

