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
      Log likelihood: -1652.603 
      AICc: 3309.239
      
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
      Log likelihood: -1652.603 
      AICc: 3311.274
      
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
      p              0.847025499
      
      Message: Success; SE's pending bootstrap
      Function: ONESTEP  
      Strip: 0 [m] to 150 [m] 
      Average effective strip width (ESW): 78.50201 [m] (range 42.85585 [m] to 92.08696 [m]) 
      Average probability of detection: 0.5233467 (range 0.2857057 to 0.613913)
      Scaling: g(0 [m]) = 1
      Log likelihood: -1637.431 
      AICc: 3287.105
      
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
      Log likelihood: -1055.249 
      AICc: 2114.55
      
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

# oneStep-NoCovarExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m")
      Coefficients:
                   Estimate   
      (Intercept)   4.39962094
      p             0.89653524
      a1            0.09846604
      a2           -0.03376201
      
      Message: Success; SE's pending bootstrap
      Function: ONESTEP with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 207 [m] 
      Effective strip width (ESW): 85.06879 [m] 
      Probability of detection: 0.4109603
      Scaling: g(0 [m]) = 1
      Log likelihood: -1699.511 
      AICc: 3407.137
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.050562 
         Group size range: 1 to 3 
      Density in sampled area: 6.10617e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 250658.3

# oneStep-ContCovarExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ height +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m")
      Coefficients:
                   Estimate     
      (Intercept)   4.373868e+00
      height        2.220446e-16
      p             8.886606e-01
      a1            1.018574e-01
      a2           -4.528153e-02
      
      Message: Success; SE's pending bootstrap
      Function: ONESTEP with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 207 [m] 
      Average effective strip width (ESW): 84.04491 [m] (range 84.04491 [m] to 84.04491 [m]) 
      Average probability of detection: 0.406014 (range 0.406014 to 0.406014)
      Scaling: g(0 [m]) = 1
      Log likelihood: -1698.721 
      AICc: 3407.613
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.050562 
         Group size range: 1 to 3 
      Density in sampled area: 6.180558e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 253711.9

---

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ height +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", x.scl = xScl, g.x.scl = gXscl)
      Coefficients:
                   Estimate     
      (Intercept)   4.373868e+00
      height        2.220446e-16
      p             8.886606e-01
      a1            1.018574e-01
      a2           -4.528153e-02
      
      Message: Success; SE's pending bootstrap
      Function: ONESTEP with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 207 [m] 
      Average effective strip width (ESW): 63.03368 [m] (range 63.03368 [m] to 63.03368 [m]) 
      Average probability of detection: 0.3045105 (range 0.3045105 to 0.3045105)
      Scaling: g(0 [m]) = 0.75
      Log likelihood: -1698.721 
      AICc: 3407.613
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.050562 
         Group size range: 1 to 3 
      Density in sampled area: 8.240744e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 338282.6

# oneStep-Bootstraps

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~
         groupsize(groupsize), likelihood = lhood)
      Coefficients:
                   Estimate   SE          z         p(>|z|)      
      (Intercept)  4.3956830  0.09239633  47.57422   0.000000e+00
      p            0.8960674  0.02674663  33.50207  4.496336e-246
      
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

