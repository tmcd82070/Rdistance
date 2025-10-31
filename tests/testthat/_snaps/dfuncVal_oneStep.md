# oneStep-MinimumInputs

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~
         groupsize(groupsize), likelihood = lhood)
      Coefficients:
                   Estimate 
      (Intercept)  4.3956830
      p            0.8960674
      
      Message: Success; SE's pending bootstrap
      Function: ONESTEP  
      Strip: 0 [m] to 207 [m] 
      Effective strip width (ESW): 90.50658 [m] 
      Probability of detection: 0.4372299
      Scaling: g(0 [m]) = 1
      Log likelihood: -1699.911 
      AICc: 3403.857
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.050562 
         Group size range: 1 to 3 
      Density in sampled area: 5.7393e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 235598.3

# oneStep-NoCovar

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, w.lo = w.lo, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = 0, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                   Estimate 
      (Intercept)  4.3567088
      p            0.8923513
      
      Message: Success; SE's pending bootstrap
      Function: ONESTEP  
      Strip: 0 [m] to 150 [m] 
      Effective strip width (ESW): 87.40952 [m] 
      Probability of detection: 0.5827302
      Scaling: g(0 [m]) = 1
      Log likelihood: -1655.451 
      AICc: 3314.937
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 371 in 353 groups 
       Average group size: 1.050992 
         Group size range: 1 to 3 
      Density in sampled area: 5.894984e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 241989.1

# oneStep-ContinuousCovar

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ height +
         groupsize(groupsize), likelihood = lhood, w.lo = w.lo, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = 0, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                   Estimate    
      (Intercept)  4.356709e+00
      height       2.220446e-16
      p            8.923513e-01
      
      Message: Success; SE's pending bootstrap
      Function: ONESTEP  
      Strip: 0 [m] to 150 [m] 
      Average effective strip width (ESW): 87.40952 [m] (range 87.40952 [m] to 87.40952 [m]) 
      Average probability of detection: 0.5827302 (range 0.5827302 to 0.5827302)
      Scaling: g(0 [m]) = 1
      Log likelihood: -1655.451 
      AICc: 3316.971
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 371 in 353 groups 
       Average group size: 1.050992 
         Group size range: 1 to 3 
      Density in sampled area: 5.894984e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 241989.1

# oneStep-FactorCovar

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ observer +
         groupsize(groupsize), likelihood = lhood, w.lo = w.lo, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = 0, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                    Estimate    
      (Intercept)    4.356708827
      observerobs2  -0.002567392
      observerobs3  -0.080042705
      observerobs4  -0.764891084
      observerobs5  -0.213574100
      p              0.847025496
      
      Message: Success; SE's pending bootstrap
      Function: ONESTEP  
      Strip: 0 [m] to 150 [m] 
      Average effective strip width (ESW): 78.50201 [m] (range 42.85585 [m] to 92.08696 [m]) 
      Average probability of detection: 0.5233467 (range 0.2857057 to 0.613913)
      Scaling: g(0 [m]) = 1
      Log likelihood: -1640.871 
      AICc: 3293.985
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 371 in 353 groups 
       Average group size: 1.050992 
         Group size range: 1 to 3 
      Density in sampled area: 7.018442e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 288107

# oneStep-NoCovarWloWhi

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, w.lo = w.20, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = w.20, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                   Estimate 
      (Intercept)  4.0724397
      p            0.8412017
      
      Message: Success; SE's pending bootstrap
      Function: ONESTEP  
      Strip: 20 [m] to 150 [m] 
      Effective strip width (ESW): 69.78112 [m] 
      Probability of detection: 0.5367779
      Scaling: g(20 [m]) = 1
      Log likelihood: -1058.051 
      AICc: 2120.154
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 241 in 233 groups 
       Average group size: 1.034335 
         Group size range: 1 to 2 
      Density in sampled area: 4.796745e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 196906.4

# oneStep-NoCovarWlo

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, w.lo = w.20, expansions =
         0, series = "cosine", x.scl = w.20, g.x.scl = 1, outputUnits = "m")
      Coefficients:
                   Estimate 
      (Intercept)  4.1125119
      p            0.8432203
      
      Message: Success; SE's pending bootstrap
      Function: ONESTEP  
      Strip: 20 [m] to 207 [m] 
      Effective strip width (ESW): 72.4603 [m] 
      Probability of detection: 0.3874882
      Scaling: g(20 [m]) = 1
      Log likelihood: -1099.796 
      AICc: 2203.643
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 244 in 236 groups 
       Average group size: 1.033898 
         Group size range: 1 to 2 
      Density in sampled area: 4.67689e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 191986.4

# oneStep-NoCovarsFt

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, outputUnits = "ft")
      Coefficients:
                   Estimate 
      (Intercept)  5.5537426
      p            0.8876404
      
      Message: Success; SE's pending bootstrap
      Function: ONESTEP  
      Strip: 0 [ft] to 679.1339 [ft] 
      Effective strip width (ESW): 290.8859 [ft] 
      Probability of detection: 0.428319
      Scaling: g(0 [ft]) = 1
      Log likelihood: -2121.787 
      AICc: 4247.608
      
           Surveyed Units: 118110.2 [ft] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.050562 
         Group size range: 1 to 3 
      Density in sampled area: 5.442913e-06 [1/ft^2]
      Abundance in 44185852261 [ft^2] study area: 240499.7

# oneStep-NoCovarCosExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", series = "cosine")
      Coefficients:
                   Estimate   
      (Intercept)  4.395682961
      p            0.903832853
      a1           0.156876892
      a2           0.008454863
      
      Message: Success; SE's pending bootstrap
      Function: ONESTEP with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 207 [m] 
      Effective strip width (ESW): 83.02478 
      Probability of detection: 0.4010859
      Scaling: g(0 [m]) = 1
      Log likelihood: -1699.324 
      AICc: 3406.763
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.050562 
         Group size range: 1 to 3 
      Density in sampled area: 6.256499e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 256829.3

# oneStep-NoCovarSinExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", series = "sine")
      Coefficients:
                   Estimate 
      (Intercept)  4.3956830
      p            0.8960674
      a1           0.9244590
      a2           0.2953658
      
      Message: Success; SE's pending bootstrap
      Function: ONESTEP with 2 expansion(s) of SINE series 
      Strip: 0 [m] to 207 [m] 
      Effective strip width (ESW): 90.50658 
      Probability of detection: 0.4372299
      Scaling: g(0 [m]) = 1
      Log likelihood: -1680.439 
      AICc: 3368.991
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.050562 
         Group size range: 1 to 3 
      Density in sampled area: 5.7393e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 235598.3

# oneStep-NoCovarHermExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", series = "hermite")
      Coefficients:
                   Estimate   
      (Intercept)   4.54329478
      p             0.96642617
      a1           -0.20919709
      a2            0.03788777
      
      Message: Success; SE's pending bootstrap
      Function: ONESTEP with 2 expansion(s) of HERMITE series 
      Strip: 0 [m] to 207 [m] 
      Effective strip width (ESW): 48.4394 
      Probability of detection: 0.2340068
      Scaling: g(0 [m]) = 1
      Log likelihood: -1666.774 
      AICc: 3341.663
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.050562 
         Group size range: 1 to 3 
      Density in sampled area: 0.0001072359 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 440203.5

# oneStep-NoCovarSimpExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", series = "simple")
      Coefficients:
                   Estimate  
      (Intercept)   4.3956831
      p             0.9218832
      a1           -3.7664433
      a2            3.3869379
      
      Message: FAILURE (Exit code= 1, did not converge)
      Function: ONESTEP with 2 expansion(s) of SIMPLE series 
      Strip: 0 [m] to 207 [m] 
      Effective strip width (ESW): 66.12078 
      Probability of detection: 0.319424
      Scaling: g(0 [m]) = 1
      Log likelihood: -1676.64 
      AICc: 3361.394
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.050562 
         Group size range: 1 to 3 
      Density in sampled area: NA
      Abundance in 4105 [km^2] study area: NA

# oneStep-ContCovarExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ height +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m")
      Coefficients:
                   Estimate    
      (Intercept)  4.365643e+00
      height       2.220446e-16
      p            8.965383e-01
      a1           1.496812e-01
      a2           2.697779e-02
      
      Message: Success; SE's pending bootstrap
      Function: ONESTEP with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 207 [m] 
      Average effective strip width (ESW): 80.83056 (range 80.83056 to 80.83056) 
      Average probability of detection: 0.3904858 (range 0.3904858 to 0.3904858)
      Scaling: g(0 [m]) = 1
      Log likelihood: -1698.265 
      AICc: 3406.702
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.050562 
         Group size range: 1 to 3 
      Density in sampled area: 6.426338e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 263801.2

---

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ height +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", x.scl = xScl, g.x.scl = gXscl)
      Coefficients:
                   Estimate    
      (Intercept)  4.365643e+00
      height       2.220446e-16
      p            8.965383e-01
      a1           1.496811e-01
      a2           2.697784e-02
      
      Message: Success; SE's pending bootstrap
      Function: ONESTEP with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 207 [m] 
      Average effective strip width (ESW): 80.83056 (range 80.83056 to 80.83056) 
      Average probability of detection: 0.3904858 (range 0.3904858 to 0.3904858)
      Scaling: g(0 [m]) = 0.75
      Log likelihood: -1698.265 
      AICc: 3406.702
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.050562 
         Group size range: 1 to 3 
      Density in sampled area: 6.426338e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 263801.2

# oneStep-Bootstraps

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~
         groupsize(groupsize), likelihood = lhood)
      Coefficients:
                   Estimate   SE          z         p(>|z|)      
      (Intercept)  4.3956830  0.09239633  47.57422   0.000000e+00
      p            0.8960674  0.02674663  33.50207  4.496644e-246
      
      Message: Success; Bootstrap SE's
      Function: ONESTEP  
      Strip: 0 [m] to 207 [m] 
      Effective strip width (ESW): 90.50658 [m] 
      Probability of detection: 0.4372299
      Scaling: g(0 [m]) = 1
      Log likelihood: -1699.911 
      AICc: 3403.857
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.050562 
         Group size range: 1 to 3 
      
      Density in sampled area: 5.7393e-05 [1/m^2]
                       95% CI: 4.63371e-05 [1/m^2] to 6.578654e-05 [1/m^2]
      
      Abundance in 4.105e+09 [m^2] study area: 235598.3
                                       95% CI: 190213.8 to 270053.8

