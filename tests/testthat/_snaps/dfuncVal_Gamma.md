# Gamma-MinimumInputs

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~
         groupsize(groupsize), likelihood = lhood)
      Coefficients:
                   Estimate  SE          z         p(>|z|)     
      (Intercept)  4.054975  0.04777766  84.87177  0.000000e+00
      Shape        1.435744  0.10310235  13.92542  4.438995e-44
      
      Message: Success; Asymptotic SE's
      Function: GAMMA  
      Strip: 0 [m] to 207 [m] 
      Effective strip width (ESW): 57.54499 [m] 
      Probability of detection: 0.2779951
      Scaling: g(max) = 1
      Log likelihood: -2123.608 
      AICc: 4251.25
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.050562 
         Group size range: 1 to 3 
      Density in sampled area: 9.026754e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 370548.2

# Gamma-NoCovar

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, w.lo = w.lo, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = 0, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                   Estimate  SE          z         p(>|z|)     
      (Intercept)  4.061269  0.05231719  77.62780  0.000000e+00
      Shape        1.434154  0.10950403  13.09682  3.433515e-39
      
      Message: Success; Asymptotic SE's
      Function: GAMMA  
      Strip: 0 [m] to 150 [m] 
      Effective strip width (ESW): 57.15541 [m] 
      Probability of detection: 0.3810361
      Scaling: g(max) = 1
      Log likelihood: -2091.51 
      AICc: 4187.055
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 371 in 353 groups 
       Average group size: 1.050992 
         Group size range: 1 to 3 
      Density in sampled area: 9.01538e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 370081.3

# Gamma-ContinuousCovar

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ bare +
         groupsize(groupsize), likelihood = lhood, w.lo = w.lo, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = 0, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                   Estimate    SE           z          p(>|z|)     
      (Intercept)  3.42641864  0.198390267  17.271103  7.764479e-67
      bare         0.01111129  0.003483594   3.189604  1.424678e-03
      Shape        1.46586125  0.111951272  13.093744  3.575420e-39
      
      Message: Success; Asymptotic SE's
      Function: GAMMA  
      Strip: 0 [m] to 150 [m] 
      Average effective strip width (ESW): 57.34246 [m] (range 43.15409 [m] to 72.42063 [m]) 
      Average probability of detection: 0.3822831 (range 0.2876939 to 0.4828042)
      Scaling: g(max) = 1
      Log likelihood: -2086.438 
      AICc: 4178.944
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 371 in 353 groups 
       Average group size: 1.050992 
         Group size range: 1 to 3 
      Density in sampled area: 9.191423e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 377307.9

# Gamma-FactorCovar

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ observer +
         groupsize(groupsize), likelihood = lhood, w.lo = w.lo, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = 0, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                    Estimate     SE         z           p(>|z|)      
      (Intercept)    4.09378739  0.1122725  36.4629413  4.290403e-291
      observerobs2   0.11620723  0.1790935   0.6488633   5.164267e-01
      observerobs3   0.07713256  0.1590522   0.4849514   6.277108e-01
      observerobs4  -0.34062360  0.1610215  -2.1153920   3.439655e-02
      observerobs5  -0.06582126  0.1550720  -0.4244560   6.712333e-01
      Shape          1.46002182  0.1116089  13.0815947   4.195451e-39
      
      Message: Success; Asymptotic SE's
      Function: GAMMA  
      Strip: 0 [m] to 150 [m] 
      Average effective strip width (ESW): 57.63599 [m] (range 42.54709 [m] to 65.4527 [m]) 
      Average probability of detection: 0.38424 (range 0.2836472 to 0.4363513)
      Scaling: g(max) = 1
      Log likelihood: -2087.228 
      AICc: 4186.699
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 371 in 353 groups 
       Average group size: 1.050992 
         Group size range: 1 to 3 
      Density in sampled area: 9.098354e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 373487.4

# Gamma-NoCovarWloWhi

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, w.lo = w.20, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = w.20, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                   Estimate  SE          z         p(>|z|)      
      (Intercept)  3.776575  0.11075550  34.09830  7.816217e-255
      Shape        1.092720  0.09943738  10.98903   4.315541e-28
      
      Message: Success; Asymptotic SE's
      Function: GAMMA  
      Strip: 20 [m] to 150 [m] 
      Effective strip width (ESW): 42.57997 [m] 
      Probability of detection: 0.3275383
      Scaling: g(max) = 1
      Log likelihood: -1043.534 
      AICc: 2091.12
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 241 in 233 groups 
       Average group size: 1.034335 
         Group size range: 1 to 2 
      Density in sampled area: 7.861024e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 322695.1

# Gamma-NoCovarWlo

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, w.lo = w.20, expansions =
         0, series = "cosine", x.scl = w.20, g.x.scl = 1, outputUnits = "m")
      Coefficients:
                   Estimate  SE          z         p(>|z|)      
      (Intercept)  3.765181  0.10816190  34.81060  1.681024e-265
      Shape        1.097583  0.09324496  11.77096   5.509381e-32
      
      Message: Success; Asymptotic SE's
      Function: GAMMA  
      Strip: 20 [m] to 207 [m] 
      Effective strip width (ESW): 42.99038 [m] 
      Probability of detection: 0.2298951
      Scaling: g(max) = 1
      Log likelihood: -1074.472 
      AICc: 2152.995
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 244 in 236 groups 
       Average group size: 1.033898 
         Group size range: 1 to 2 
      Density in sampled area: 7.882901e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 323593.1

# Gamma-NoCovarsFt

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, outputUnits = "ft")
      Coefficients:
                   Estimate  SE          z          p(>|z|)     
      (Intercept)  5.243075  0.04777835  109.73746  0.000000e+00
      Shape        1.435745  0.10310392   13.92522  4.451525e-44
      
      Message: Success; Asymptotic SE's
      Function: GAMMA  
      Strip: 0 [ft] to 679.1339 [ft] 
      Effective strip width (ESW): 188.796 [ft] 
      Probability of detection: 0.2779952
      Scaling: g(max) = 1
      Log likelihood: -2526.374 
      AICc: 5056.781
      
           Surveyed Units: 118110.2 [ft] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.050562 
         Group size range: 1 to 3 
      Density in sampled area: 8.386125e-06 [1/ft^2]
      Abundance in 44185852261 [ft^2] study area: 370548.1

# Gamma-NoCovarCosExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", series = "cosine")
      Coefficients:
                   Estimate    SE          z           p(>|z|)     
      (Intercept)   4.0349322  0.09979897  40.4305986  0.000000e+00
      Shape         1.4700357  0.14558845  10.0971997  5.684216e-24
      a1           -0.1547571  0.39289913  -0.3938851  6.936659e-01
      a2            0.1506424  0.19696695   0.7648107  4.443843e-01
      
      Message: Success; Asymptotic SE's
      Function: GAMMA with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 207 [m] 
      Effective strip width (ESW): 55.09737 [m] 
      Probability of detection: 0.2661708
      Scaling: g(max) = 1
      Log likelihood: -2123.07 
      AICc: 4254.253
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.050562 
         Group size range: 1 to 3 
      Density in sampled area: 9.427755e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 387009.3

