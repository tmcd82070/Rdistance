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
      Log likelihood: -1067.199 
      AICc: 2138.451
      
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
      Log likelihood: -1105.251 
      AICc: 2214.553
      
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

# oneStep-NoCovarExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m")
      Coefficients:
                   Estimate  
      (Intercept)   4.3995448
      p             0.9139123
      a1           -0.1653617
      a2            0.5514222
      
      Message: Success; SE's pending bootstrap
      Function: ONESTEP with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 207 [m] 
      Effective strip width (ESW): 57.48284 [m] 
      Probability of detection: 0.2776949
      Scaling: g(0 [m]) = 1
      Log likelihood: -1676.911 
      AICc: 3361.937
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.050562 
         Group size range: 1 to 3 
      Density in sampled area: 9.036512e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 370948.8

# oneStep-ContCovarExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ height +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m")
      Coefficients:
                   Estimate     
      (Intercept)   4.4021899073
      height       -0.0003380217
      p             0.9133356257
      a1           -0.1653617099
      a2            0.5514222942
      
      Message: Success; SE's pending bootstrap
      Function: ONESTEP with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 207 [m] 
      Average effective strip width (ESW): 57.48285 [m] (range 57.44405 [m] to 57.52741 [m]) 
      Average probability of detection: 0.2776949 (range 0.2775075 to 0.2779102)
      Scaling: g(0 [m]) = 1
      Log likelihood: -1676.859 
      AICc: 3363.89
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.050562 
         Group size range: 1 to 3 
      Density in sampled area: 9.036535e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 370949.7

---

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ height +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", x.scl = xScl, g.x.scl = gXscl)
      Coefficients:
                   Estimate     
      (Intercept)   4.4021899073
      height       -0.0003380217
      p             0.9133356294
      a1           -0.1653617471
      a2            0.5514222644
      
      Message: Success; SE's pending bootstrap
      Function: ONESTEP with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 207 [m] 
      Average effective strip width (ESW): 43.11214 [m] (range 43.08304 [m] to 43.14556 [m]) 
      Average probability of detection: 0.2082712 (range 0.2081306 to 0.2084327)
      Scaling: g(0 [m]) = 0.75
      Log likelihood: -1676.859 
      AICc: 3363.89
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.050562 
         Group size range: 1 to 3 
      Density in sampled area: 0.0001204871 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 494599.6

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

