# negexp-MinimumInputs

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~
         groupsize(groupsize), likelihood = lhood)
      Coefficients:
                   Estimate   SE          z          p(>|z|)
      (Intercept)  -3.712969  0.05801384  -64.00144  0      
      
      Convergence: Success
      Function: NEGEXP  
      Strip: 0 [m] to 207 [m] 
      Effective strip width (ESW): 40.71316 [m] 
      Probability of detection: 0.1966819
      Scaling: g(0 [m]) = 1
      Log likelihood: -1663.952 
      AICc: 3329.915
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.050562 
         Group size range: 1 to 3 
      Density in sampled area: 0.0001275864 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 523742.1

# negexp-NoCovar

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, w.lo = w.lo, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = 0, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                   Estimate   SE          z          p(>|z|)
      (Intercept)  -3.760954  0.06845063  -54.94404  0      
      
      Convergence: Success
      Function: NEGEXP  
      Strip: 0 [m] to 150 [m] 
      Effective strip width (ESW): 41.67721 [m] 
      Probability of detection: 0.2778481
      Scaling: g(0 [m]) = 1
      Log likelihood: -1630.893 
      AICc: 3263.797
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 371 in 353 groups 
       Average group size: 1.050992 
         Group size range: 1 to 3 
      Density in sampled area: 0.0001236354 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 507523.2

# negexp-ContinuousCovar

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ bare +
         groupsize(groupsize), likelihood = lhood, w.lo = w.lo, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = 0, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                   Estimate     SE           z           p(>|z|)     
      (Intercept)  -2.98953113  0.252782161  -11.826511  2.847311e-32
      bare         -0.01359077  0.004521655   -3.005707  2.649638e-03
      
      Convergence: Success
      Function: NEGEXP  
      Strip: 0 [m] to 150 [m] 
      Average effective strip width (ESW): 42.0115 [m] (range 29.95859 [m] to 54.96368 [m]) 
      Average probability of detection: 0.2800767 (range 0.1997239 to 0.3664245)
      Scaling: g(0 [m]) = 1
      Log likelihood: -1626.391 
      AICc: 3256.816
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 371 in 353 groups 
       Average group size: 1.050992 
         Group size range: 1 to 3 
      Density in sampled area: 0.0001264913 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 519246.7

# negexp-FactorCovar

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ observer +
         groupsize(groupsize), likelihood = lhood, w.lo = w.lo, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = 0, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                    Estimate     SE         z             p(>|z|)      
      (Intercept)   -3.81382488  0.1521734  -25.06235796  1.280314e-138
      observerobs2  -0.14929545  0.2485709   -0.60061512   5.480964e-01
      observerobs3   0.04891655  0.2045285    0.23916745   8.109757e-01
      observerobs4   0.36577768  0.2120902    1.72463270   8.459369e-02
      observerobs5   0.01950566  0.2139414    0.09117292   9.273552e-01
      
      Convergence: Success
      Function: NEGEXP  
      Strip: 0 [m] to 150 [m] 
      Average effective strip width (ESW): 41.94674 [m] (range 31.17263 [m] to 49.57912 [m]) 
      Average probability of detection: 0.279645 (range 0.2078175 to 0.3305275)
      Scaling: g(0 [m]) = 1
      Log likelihood: -1628.324 
      AICc: 3266.822
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 371 in 353 groups 
       Average group size: 1.050992 
         Group size range: 1 to 3 
      Density in sampled area: 0.0001249166 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 512782.6

# negexp-NoCovarWloWhi

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, w.lo = w.20, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = w.20, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                   Estimate   SE          z          p(>|z|)
      (Intercept)  -3.792642  0.07264876  -52.20518  0      
      
      Convergence: Success
      Function: NEGEXP  
      Strip: 2 [m] to 150 [m] 
      Effective strip width (ESW): 42.79363 [m] 
      Probability of detection: 0.2891462
      Scaling: g(2 [m]) = 1
      Log likelihood: -1528.975 
      AICc: 3059.963
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 347 in 330 groups 
       Average group size: 1.051515 
         Group size range: 1 to 3 
      Density in sampled area: 0.0001126206 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 462307.5

# negexp-NoCovarWlo

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, w.lo = w.20, expansions =
         0, series = "cosine", x.scl = w.20, g.x.scl = 1, outputUnits = "m")
      Coefficients:
                   Estimate  SE          z          p(>|z|)
      (Intercept)  -3.73547  0.06060492  -61.63641  0      
      
      Convergence: Success
      Function: NEGEXP  
      Strip: 2 [m] to 207 [m] 
      Effective strip width (ESW): 41.59305 [m] 
      Probability of detection: 0.2028929
      Scaling: g(2 [m]) = 1
      Log likelihood: -1562.078 
      AICc: 3126.168
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 350 in 333 groups 
       Average group size: 1.051051 
         Group size range: 1 to 3 
      Density in sampled area: 0.0001168732 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 479764.3

# negexp-NoCovarsFt

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, outputUnits = "ft")
      Coefficients:
                   Estimate   SE          z          p(>|z|)
      (Intercept)  -4.901069  0.05801385  -84.48102  0      
      
      Convergence: Success
      Function: NEGEXP  
      Strip: 0 [ft] to 679.1339 [ft] 
      Effective strip width (ESW): 133.5734 [ft] 
      Probability of detection: 0.1966819
      Scaling: g(0 [ft]) = 1
      Log likelihood: -2086.915 
      AICc: 4175.842
      
           Surveyed Units: 118110.2 [ft] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.050562 
         Group size range: 1 to 3 
      Density in sampled area: 1.185316e-05 [1/ft^2]
      Abundance in 44185852261 [ft^2] study area: 523742

# negexp-NoCovarExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m")
      Coefficients:
                   Estimate     SE         z            p(>|z|)      
      (Intercept)  -3.55545953  0.1136070  -31.2961276  5.268202e-215
      a1           -0.07392903  0.1878678   -0.3935162   6.939382e-01
      a2           -0.24178850  0.1154809   -2.0937531   3.628198e-02
      
      Convergence: Success
      Function: NEGEXP with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 207 [m] 
      Effective strip width (ESW): 45.74097 [m] 
      Probability of detection: 0.2209709
      Scaling: g(0 [m]) = 1
      Log likelihood: -1659.437 
      AICc: 3324.942
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.050562 
         Group size range: 1 to 3 
      Density in sampled area: 0.0001135622 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 466172.7

# negexp-ContCovarExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ bare +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m")
      Coefficients:
                   Estimate     SE           z            p(>|z|)     
      (Intercept)  -2.72609129  0.218953210  -12.4505655  1.388578e-35
      bare         -0.01358029  0.003622584   -3.7487865  1.776923e-04
      a1           -0.13218438  0.190557576   -0.6936716  4.878882e-01
      a2           -0.25877710  0.118296110   -2.1875368  2.870336e-02
      
      Convergence: Success
      Function: NEGEXP with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 207 [m] 
      Average effective strip width (ESW): 47.24028 [m] (range 30.01992 [m] to 67.58551 [m]) 
      Average probability of detection: 0.2282139 (range 0.1450238 to 0.3265001)
      Scaling: g(0 [m]) = 1
      Log likelihood: -1652.409 
      AICc: 3312.932
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.050562 
         Group size range: 1 to 3 
      Density in sampled area: 0.0001160719 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 476475

# negexp-ContCovarExpansionsScaling

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ bare +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", x.scl = xScl, g.x.scl = gXscl)
      Coefficients:
                   Estimate     SE           z            p(>|z|)     
      (Intercept)  -2.72609129  0.218953210  -12.4505655  1.388578e-35
      bare         -0.01358029  0.003622584   -3.7487865  1.776923e-04
      a1           -0.13218438  0.190557575   -0.6936716  4.878882e-01
      a2           -0.25877710  0.118296110   -2.1875368  2.870336e-02
      
      Convergence: Success
      Function: NEGEXP with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 207 [m] 
      Average effective strip width (ESW): 35.43021 [m] (range 22.51494 [m] to 50.68913 [m]) 
      Average probability of detection: 0.1711604 (range 0.1087678 to 0.244875)
      Scaling: g(0 [m]) = 0.75
      Log likelihood: -1652.409 
      AICc: 3312.932
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.050562 
         Group size range: 1 to 3 
      Density in sampled area: 0.0001547625 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 635299.9

# negexp-Bootstraps

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~
         groupsize(groupsize), likelihood = lhood)
      Coefficients:
                   Estimate   SE          z          p(>|z|)
      (Intercept)  -3.712969  0.05801384  -64.00144  0      
      
      Convergence: Success
      Function: NEGEXP  
      Strip: 0 [m] to 207 [m] 
      Effective strip width (ESW): 40.71316 [m] 
      Probability of detection: 0.1966819
      Scaling: g(0 [m]) = 1
      Log likelihood: -1663.952 
      AICc: 3329.915
      
           Surveyed Units: 36000 [m] 
         Individuals seen: 374 in 356 groups 
       Average group size: 1.050562 
         Group size range: 1 to 3 
      
      Density in sampled area: 0.0001275864 [1/m^2]
                       95% CI: 0.0001002809 [1/m^2] to 0.000148606 [1/m^2]
      
      Abundance in 4.105e+09 [m^2] study area: 523742.1
                                       95% CI: 411653.3 to 610027.6

