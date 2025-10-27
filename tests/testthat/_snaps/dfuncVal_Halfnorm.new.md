# halfnorm-MinimumInputs

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~
         groupsize(groupsize), likelihood = lhood)
      Coefficients:
                   Estimate  SE          z         p(>|z|)
      (Intercept)  3.959313  0.03767961  105.0784  0      
      
      Message: Success; Asymptotic SE's
      Function: HALFNORM  
      Strip: 0 [m] to 207 [m] 
      Effective strip width (ESW): 65.69518 [m] 
      Probability of detection: 0.317368
      Scaling: g(0 [m]) = 1
      Log likelihood: -1667.639 
      AICc: 3337.289
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.050562 
         Group size range: 1 to 3 
      Density in sampled area: 7.906888e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 324577.7

# halfnorm-NoCovar

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, w.lo = w.lo, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = 0, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                   Estimate  SE          z         p(>|z|)
      (Intercept)  3.9095    0.04038592  96.80353  0      
      
      Message: Success; Asymptotic SE's
      Function: HALFNORM  
      Strip: 0 [m] to 150 [m] 
      Effective strip width (ESW): 62.34318 [m] 
      Probability of detection: 0.4156212
      Scaling: g(0 [m]) = 1
      Log likelihood: -1630.716 
      AICc: 3263.443
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 371 in 353 groups 
       Average group size: 1.050992 
         Group size range: 1 to 3 
      Density in sampled area: 8.265183e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 339285.8

# halfnorm-ContinuousCovar

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ bare +
         groupsize(groupsize), likelihood = lhood, w.lo = w.lo, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = 0, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                   Estimate     SE           z          p(>|z|)      
      (Intercept)  3.346183910  0.155241297  21.554728  4.780716e-103
      bare         0.009715925  0.002720183   3.571791   3.545479e-04
      
      Message: Success; Asymptotic SE's
      Function: HALFNORM  
      Strip: 0 [m] to 150 [m] 
      Average effective strip width (ESW): 61.99663 [m] (range 47.95033 [m] to 76.9778 [m]) 
      Average probability of detection: 0.4133109 (range 0.3196689 to 0.5131854)
      Scaling: g(0 [m]) = 1
      Log likelihood: -1624.351 
      AICc: 3252.736
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 371 in 353 groups 
       Average group size: 1.050992 
         Group size range: 1 to 3 
      Density in sampled area: 8.470704e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 347722.4

# halfnorm-FactorCovar

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ observer +
         groupsize(groupsize), likelihood = lhood, w.lo = w.lo, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = 0, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                    Estimate     SE          z           p(>|z|)   
      (Intercept)    3.92800543  0.08793471  44.6695675  0.00000000
      observerobs2   0.17073246  0.14604669   1.1690265  0.24239295
      observerobs3  -0.01647247  0.11923776  -0.1381481  0.89012337
      observerobs4  -0.26739638  0.12897924  -2.0731737  0.03815612
      observerobs5  -0.02839298  0.12345494  -0.2299866  0.81810216
      
      Message: Success; Asymptotic SE's
      Function: HALFNORM  
      Strip: 0 [m] to 150 [m] 
      Average effective strip width (ESW): 62.19478 [m] (range 48.72956 [m] to 74.5625 [m]) 
      Average probability of detection: 0.4146318 (range 0.3248637 to 0.4970833)
      Scaling: g(0 [m]) = 1
      Log likelihood: -1626.142 
      AICc: 3262.456
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 371 in 353 groups 
       Average group size: 1.050992 
         Group size range: 1 to 3 
      Density in sampled area: 8.387749e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 344317.1

# halfnorm-NoCovarWloWhi

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, w.lo = w.20, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = w.20, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                   Estimate  SE          z         p(>|z|)
      (Intercept)  3.783568  0.05013421  75.46878  0      
      
      Message: Success; Asymptotic SE's
      Function: HALFNORM  
      Strip: 20 [m] to 150 [m] 
      Effective strip width (ESW): 54.94 [m] 
      Probability of detection: 0.4226154
      Scaling: g(20 [m]) = 1
      Log likelihood: -1046.467 
      AICc: 2094.952
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 241 in 233 groups 
       Average group size: 1.034335 
         Group size range: 1 to 2 
      Density in sampled area: 6.092505e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 250097.3

# halfnorm-NoCovarWlo

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, w.lo = w.20, expansions =
         0, series = "cosine", x.scl = w.20, g.x.scl = 1, outputUnits = "m")
      Coefficients:
                   Estimate  SE          z         p(>|z|)
      (Intercept)  3.864711  0.04630146  83.46845  0      
      
      Message: Success; Asymptotic SE's
      Function: HALFNORM  
      Strip: 20 [m] to 207 [m] 
      Effective strip width (ESW): 59.76463 [m] 
      Probability of detection: 0.3195969
      Scaling: g(20 [m]) = 1
      Log likelihood: -1083.168 
      AICc: 2168.354
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 244 in 236 groups 
       Average group size: 1.033898 
         Group size range: 1 to 2 
      Density in sampled area: 5.670392e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 232769.6

# halfnorm-NoCovarsFt

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, outputUnits = "ft")
      Coefficients:
                   Estimate  SE         z       p(>|z|)
      (Intercept)  5.147412  0.0376796  136.61  0      
      
      Message: Success; Asymptotic SE's
      Function: HALFNORM  
      Strip: 0 [ft] to 679.1339 [ft] 
      Effective strip width (ESW): 215.5353 [ft] 
      Probability of detection: 0.317368
      Scaling: g(0 [ft]) = 1
      Log likelihood: -2090.602 
      AICc: 4183.216
      
           Surveyed Units: 118110.2 [ft] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.050562 
         Group size range: 1 to 3 
      Density in sampled area: 7.345741e-06 [1/ft^2]
      Abundance in 44185852261 [ft^2] study area: 324577.8

# halfnorm-NoCovarExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m")
      Coefficients:
                   Estimate      SE          z           p(>|z|)   
      (Intercept)   4.079325523  0.05714808  71.3816727  0.00000000
      a1            0.345899401  0.13834322   2.5002989  0.01240886
      a2           -0.007169283  0.10471427  -0.0684652  0.94541532
      
      Message: Success; Asymptotic SE's
      Function: HALFNORM with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 207 [m] 
      Effective strip width (ESW): 79.14468 [m] 
      Probability of detection: 0.3823415
      Scaling: g(0 [m]) = 1
      Log likelihood: -1663.037 
      AICc: 3332.143
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.050562 
         Group size range: 1 to 3 
      Density in sampled area: 6.563226e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 269420.4

# halfnorm-ContCovarExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ bare +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m")
      Coefficients:
                   Estimate      SE           z            p(>|z|)     
      (Intercept)   3.379022911  0.170032029  19.87286125  6.990264e-88
      bare          0.011262240  0.002783444   4.04615273  5.206627e-05
      a1            0.253161809  0.152435436   1.66078056  9.675753e-02
      a2           -0.002003448  0.110141139  -0.01818983  9.854874e-01
      
      Message: Success; Asymptotic SE's
      Function: HALFNORM with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 207 [m] 
      Average effective strip width (ESW): 74.61177 [m] (range 57.9062 [m] to 93.13974 [m]) 
      Average probability of detection: 0.3604433 (range 0.2797401 to 0.4499504)
      Scaling: g(0 [m]) = 1
      Log likelihood: -1655.091 
      AICc: 3318.296
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.050562 
         Group size range: 1 to 3 
      Density in sampled area: 7.094537e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 291230.8

---

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ bare +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", x.scl = xScl, g.x.scl = gXscl)
      Coefficients:
                   Estimate      SE           z            p(>|z|)     
      (Intercept)   3.379022911  0.170032029  19.87286125  6.990264e-88
      bare          0.011262240  0.002783444   4.04615273  5.206627e-05
      a1            0.253161809  0.152435436   1.66078056  9.675753e-02
      a2           -0.002003448  0.110141139  -0.01818983  9.854874e-01
      
      Message: Success; Asymptotic SE's
      Function: HALFNORM with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 207 [m] 
      Average effective strip width (ESW): 74.61177 [m] (range 57.9062 [m] to 93.13974 [m]) 
      Average probability of detection: 0.3604433 (range 0.2797401 to 0.4499504)
      Scaling: g(0 [m]) = 0.75
      Log likelihood: -1655.091 
      AICc: 3318.296
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.050562 
         Group size range: 1 to 3 
      Density in sampled area: 7.094537e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 291230.8

# halfnorm-Bootstraps

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~
         groupsize(groupsize), likelihood = lhood)
      Coefficients:
                   Estimate  SE          z         p(>|z|)
      (Intercept)  3.959313  0.05118412  77.35433  0      
      
      Message: Success; Bootstrap SE's
      Function: HALFNORM  
      Strip: 0 [m] to 207 [m] 
      Effective strip width (ESW): 65.69518 [m] 
      Probability of detection: 0.317368
      Scaling: g(0 [m]) = 1
      Log likelihood: -1667.639 
      AICc: 3337.289
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.050562 
         Group size range: 1 to 3 
      
      Density in sampled area: 7.906888e-05 [1/m^2]
                       95% CI: 6.247555e-05 [1/m^2] to 8.923405e-05 [1/m^2]
      
      Abundance in 4.105e+09 [m^2] study area: 324577.7
                                       95% CI: 256462.1 to 366305.8

