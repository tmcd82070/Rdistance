# halfnorm-MinimumInputs

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~
         groupsize(groupsize), likelihood = lhood)
      Coefficients:
                   Estimate  SE          z         p(>|z|)
      (Intercept)  3.959313  0.03767961  105.0784  0      
      
      Convergence: Success
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
      
      Convergence: Success
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
      (Intercept)  3.346186334  0.155241549  21.554709  4.782716e-103
      bare         0.009715902  0.002720187   3.571777   3.545668e-04
      
      Convergence: Success
      Function: HALFNORM  
      Strip: 0 [m] to 150 [m] 
      Average effective strip width (ESW): 61.9967 [m] (range 47.95041 [m] to 76.97784 [m]) 
      Average probability of detection: 0.4133113 (range 0.3196694 to 0.5131856)
      Scaling: g(0 [m]) = 1
      Log likelihood: -1624.351 
      AICc: 3252.736
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 371 in 353 groups 
       Average group size: 1.050992 
         Group size range: 1 to 3 
      Density in sampled area: 8.470694e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 347722

# halfnorm-FactorCovar

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ observer +
         groupsize(groupsize), likelihood = lhood, w.lo = w.lo, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = 0, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                    Estimate     SE         z           p(>|z|)   
      (Intercept)    3.92800543  0.0879347  44.6695705  0.00000000
      observerobs2   0.17073245  0.1460467   1.1690265  0.24239295
      observerobs3  -0.01647247  0.1192377  -0.1381481  0.89012334
      observerobs4  -0.26739637  0.1289792  -2.0731738  0.03815611
      observerobs5  -0.02839298  0.1234549  -0.2299866  0.81810217
      
      Convergence: Success
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
      
      Convergence: Success
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
      
      Convergence: Success
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
      
      Convergence: Success
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
                   Estimate      SE          z            p(>|z|)   
      (Intercept)   4.079325514  0.05714809  71.38166236  0.00000000
      a1            0.345899344  0.13834328   2.50029742  0.01240891
      a2           -0.007169272  0.10471435  -0.06846504  0.94541545
      
      Convergence: Success
      Function: HALFNORM with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 207 [m] 
      Effective strip width (ESW): 59.11922 [m] 
      Probability of detection: 0.2856001
      Scaling: g(0 [m]) = 1
      Log likelihood: -1663.037 
      AICc: 3332.143
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.050562 
         Group size range: 1 to 3 
      Density in sampled area: 8.786388e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 360681.2

# halfnorm-ContCovarExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ bare +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m")
      Coefficients:
                   Estimate      SE           z            p(>|z|)     
      (Intercept)   3.379022069  0.170031939  19.87286675  6.989499e-88
      bare          0.011262251  0.002783443   4.04615847  5.206499e-05
      a1            0.253160857  0.152435242   1.66077643  9.675836e-02
      a2           -0.002002772  0.110140598  -0.01818377  9.854922e-01
      
      Convergence: Success
      Function: HALFNORM with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 207 [m] 
      Average effective strip width (ESW): 59.63415 [m] (range 46.28205 [m] to 74.44282 [m]) 
      Average probability of detection: 0.2880877 (range 0.2235848 to 0.3596271)
      Scaling: g(0 [m]) = 1
      Log likelihood: -1655.091 
      AICc: 3318.296
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.050562 
         Group size range: 1 to 3 
      Density in sampled area: 8.876391e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 364375.9

---

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ bare +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", x.scl = xScl, g.x.scl = gXscl)
      Coefficients:
                   Estimate      SE           z            p(>|z|)     
      (Intercept)   3.379022805  0.170032017  19.87286202  6.990157e-88
      bare          0.011262242  0.002783446   4.04615060  5.206674e-05
      a1            0.253160841  0.152437890   1.66074748  9.676417e-02
      a2           -0.002002868  0.110144844  -0.01818395  9.854921e-01
      
      Convergence: Success
      Function: HALFNORM with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 207 [m] 
      Average effective strip width (ESW): 44.72562 [m] (range 34.71155 [m] to 55.83212 [m]) 
      Average probability of detection: 0.2160658 (range 0.1676887 to 0.2697204)
      Scaling: g(0 [m]) = 0.75
      Log likelihood: -1655.091 
      AICc: 3318.296
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.050562 
         Group size range: 1 to 3 
      Density in sampled area: 0.0001183518 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 485834.3

# halfnorm-Bootstraps

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~
         groupsize(groupsize), likelihood = lhood)
      Coefficients:
                   Estimate  SE          z         p(>|z|)
      (Intercept)  3.959313  0.03767961  105.0784  0      
      
      Convergence: Success
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
                       95% CI: 6.31604e-05 [1/m^2] to 8.926331e-05 [1/m^2]
      
      Abundance in 4.105e+09 [m^2] study area: 324577.7
                                       95% CI: 259273.4 to 366425.9

