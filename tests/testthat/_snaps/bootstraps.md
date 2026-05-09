# No bootstrapping

    Code
      summary(abn)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~
         groupsize(groupsize), likelihood = lhood, w.hi = w.hi)
      Coefficients:
                   Estimate  SE       z      p(>|z|)
      (Intercept)  3.918     0.03777  103.7  0      
      
      Message: Success; Asymptotic SE's
      Function: HALFNORM  
      Strip: 0 [m] to 200 [m] 
      Effective strip width (ESW): 63.01 [m] 
      Probability of detection: 0.315
      Scaling: g(0 [m]) = 1
      Log likelihood: -1644 
      AICc: 3289
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 372 in 354 groups 
       Average group size: 1.051 
         Group size range: 1 to 3 
      Density in sampled area: 8.2e-05 [1/m^2]
      Abundance in 2589988 [m^2] study area: 212.4

# Serial BS; show T; plot T

    Code
      summary(abn)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~
         groupsize(groupsize), likelihood = lhood, w.hi = w.hi)
      Coefficients:
                   Estimate  SE      z      p(>|z|)
      (Intercept)  3.918     0.0442  88.63  0      
      
      Message: Success; Bootstrap SE's
      Function: HALFNORM  
      Strip: 0 [m] to 200 [m] 
      Effective strip width (ESW): 63.01 [m] 
      Probability of detection: 0.315
      Scaling: g(0 [m]) = 1
      Log likelihood: -1644 
      AICc: 3289
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 372 in 354 groups 
       Average group size: 1.051 
         Group size range: 1 to 3 
      
      Density in sampled area: 8.2e-05 [1/m^2]
                       95% CI: 6.662e-05 [1/m^2] to 9.678e-05 [1/m^2]
      
      Abundance in 2589988 [m^2] study area: 212.4
                                     95% CI: 172.6 to 250.7

# Serial BS; show F; plot T

    Code
      summary(abn)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~
         groupsize(groupsize), likelihood = lhood, w.hi = w.hi)
      Coefficients:
                   Estimate  SE      z      p(>|z|)
      (Intercept)  3.918     0.0584  67.08  0      
      
      Message: Success; Bootstrap SE's
      Function: HALFNORM  
      Strip: 0 [m] to 200 [m] 
      Effective strip width (ESW): 63.01 [m] 
      Probability of detection: 0.315
      Scaling: g(0 [m]) = 1
      Log likelihood: -1644 
      AICc: 3289
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 372 in 354 groups 
       Average group size: 1.051 
         Group size range: 1 to 3 
      
      Density in sampled area: 8.2e-05 [1/m^2]
                       95% CI: 5.807e-05 [1/m^2] to 9.856e-05 [1/m^2]
      
      Abundance in 2589988 [m^2] study area: 212.4
                                     95% CI: 150.4 to 255.3

# Serial BS; show T; plot F

    Code
      summary(abn)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~
         groupsize(groupsize), likelihood = lhood, w.hi = w.hi)
      Coefficients:
                   Estimate  SE       z      p(>|z|)
      (Intercept)  3.918     0.06155  63.65  0      
      
      Message: Success; Bootstrap SE's
      Function: HALFNORM  
      Strip: 0 [m] to 200 [m] 
      Effective strip width (ESW): 63.01 [m] 
      Probability of detection: 0.315
      Scaling: g(0 [m]) = 1
      Log likelihood: -1644 
      AICc: 3289
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 372 in 354 groups 
       Average group size: 1.051 
         Group size range: 1 to 3 
      
      Density in sampled area: 8.2e-05 [1/m^2]
                       95% CI: 6.706e-05 [1/m^2] to 0.0001017 [1/m^2]
      
      Abundance in 2589988 [m^2] study area: 212.4
                                     95% CI: 173.7 to 263.3

# Serial BS; show F; plot F

    Code
      summary(abn)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~
         groupsize(groupsize), likelihood = lhood, w.hi = w.hi)
      Coefficients:
                   Estimate  SE       z      p(>|z|)
      (Intercept)  3.918     0.05104  76.76  0      
      
      Message: Success; Bootstrap SE's
      Function: HALFNORM  
      Strip: 0 [m] to 200 [m] 
      Effective strip width (ESW): 63.01 [m] 
      Probability of detection: 0.315
      Scaling: g(0 [m]) = 1
      Log likelihood: -1644 
      AICc: 3289
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 372 in 354 groups 
       Average group size: 1.051 
         Group size range: 1 to 3 
      
      Density in sampled area: 8.2e-05 [1/m^2]
                       95% CI: 6.493e-05 [1/m^2] to 0.0001051 [1/m^2]
      
      Abundance in 2589988 [m^2] study area: 212.4
                                     95% CI: 168.2 to 272.2

# Parallel BS; Cores=2

    Code
      summary(abn)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~
         groupsize(groupsize), likelihood = lhood, w.hi = w.hi)
      Coefficients:
                   Estimate  SE       z      p(>|z|)
      (Intercept)  3.918     0.05611  69.82  0      
      
      Message: Success; Bootstrap SE's
      Function: HALFNORM  
      Strip: 0 [m] to 200 [m] 
      Effective strip width (ESW): 63.01 [m] 
      Probability of detection: 0.315
      Scaling: g(0 [m]) = 1
      Log likelihood: -1644 
      AICc: 3289
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 372 in 354 groups 
       Average group size: 1.051 
         Group size range: 1 to 3 
      
      Density in sampled area: 8.2e-05 [1/m^2]
                       95% CI: 6.727e-05 [1/m^2] to 0.0001152 [1/m^2]
      
      Abundance in 2589988 [m^2] study area: 212.4
                                     95% CI: 174.2 to 298.4

