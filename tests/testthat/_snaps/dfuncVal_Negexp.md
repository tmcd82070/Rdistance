# Hazrate Minimum inputs

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

# Negexp w/ no covariates, same value

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

# Negexp w/ cont cov, same value

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

# Negexp factor covariates, same value

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

# Negexp, no covar, wlo 20, whi 150

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, w.lo = w.20, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = w.20, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                   Estimate   SE          z          p(>|z|)
      (Intercept)  -3.618339  0.09117697  -39.68479  0      
      
      Convergence: Success
      Function: NEGEXP  
      Strip: 24.384 [m] to 150 [m] 
      Effective strip width (ESW): 35.9936 [m] 
      Probability of detection: 0.2865367
      Scaling: g(24.384 [m]) = 1
      Log likelihood: -923.906 
      AICc: 1849.832

# Negexp, no covar, wlo 20, whi high

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = sparrowDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, w.lo = w.20, expansions =
         0, series = "cosine", x.scl = w.20, g.x.scl = 1, outputUnits = "m")
      Coefficients:
                   Estimate  SE         z          p(>|z|)
      (Intercept)  -3.58402  0.0754508  -47.50141  0      
      
      Convergence: Success
      Function: NEGEXP  
      Strip: 24.384 [m] to 207 [m] 
      Effective strip width (ESW): 35.79178 [m] 
      Probability of detection: 0.1959948
      Scaling: g(24.384 [m]) = 1
      Log likelihood: -954.5901 
      AICc: 1911.2

# Negexp, no covar, ft

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

# Negexp, no covar, expansions

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

# Negexp, w/ cont covar, expansions

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

