# Gamma-MinimumInputs

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~
         groupsize(groupsize), likelihood = lhood)
      Coefficients:
                   Estimate  SE       z        p(>|z|)  
      (Intercept)  4.492     0.04013  111.939  0.000e+00
      Shape        3.451     0.46079    7.488  6.976e-14
      
      Message: Success; Asymptotic SE's
      Function: GAMMA  
      Strip: 0 [m] to 265 [m] 
      Effective detection radius (EDR): 116.2 [m] 
      Probability of detection: 0.1924
      Scaling: g(max) = 1
      Log likelihood: -994.9 
      AICc: 1994
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.016 
         Group size range: 1 to 2 
      Density in sampled area: 3.848e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 157973

# Gamma-NoCovar

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, w.lo = w.lo, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = 0, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                   Estimate  SE       z       p(>|z|)  
      (Intercept)  4.441     0.05007  88.698  0.000e+00
      Shape        3.873     0.56458   6.859  6.921e-12
      
      Message: Success; Asymptotic SE's
      Function: GAMMA  
      Strip: 0 [m] to 200 [m] 
      Effective detection radius (EDR): 111.8 [m] 
      Probability of detection: 0.3123
      Scaling: g(max) = 1
      Log likelihood: -928.1 
      AICc: 1860
      
           Surveyed Units: 120 
         Individuals seen: 188 in 185 groups 
       Average group size: 1.016 
         Group size range: 1 to 2 
      Density in sampled area: 3.992e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 163871

# Gamma-ContinuousCovar

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ bare +
         groupsize(groupsize), likelihood = lhood, w.lo = w.lo, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = 0, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                   Estimate  SE        z       p(>|z|)  
      (Intercept)  3.95235   0.285438  13.847  1.333e-43
      bare         0.01234   0.007275   1.696  8.987e-02
      Shape        3.94262   0.572024   6.892  5.485e-12
      
      Message: Success; Asymptotic SE's
      Function: GAMMA  
      Strip: 0 [m] to 200 [m] 
      Average effective detection radius (EDR): 111.8 [m] (range 99.74 [m] to 127.7 [m]) 
      Average probability of detection: 0.3135 (range 0.2487 to 0.408)
      Scaling: g(max) = 1
      Log likelihood: -926.6 
      AICc: 1859
      
           Surveyed Units: 120 
         Individuals seen: 188 in 185 groups 
       Average group size: 1.016 
         Group size range: 1 to 2 
      Density in sampled area: 4.025e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 165218

# Gamma-FactorCovar

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ observer +
         groupsize(groupsize), likelihood = lhood, w.lo = w.lo, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = 0, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                    Estimate  SE      z        p(>|z|)  
      (Intercept)    4.6448   0.1093  42.4903  0.000e+00
      observerobs2  -0.0802   0.1207  -0.6646  5.063e-01
      observerobs3  -0.3741   0.1392  -2.6871  7.207e-03
      observerobs4  -0.6133   0.1121  -5.4684  4.540e-08
      observerobs5  -0.1590   0.1294  -1.2287  2.192e-01
      observerobs6  -0.4464   0.1346  -3.3171  9.095e-04
      Shape          5.3216   0.7213   7.3777  1.610e-13
      
      Message: Success; Asymptotic SE's
      Function: GAMMA  
      Strip: 0 [m] to 200 [m] 
      Average effective detection radius (EDR): 111 [m] (range 79.75 [m] to 141.6 [m]) 
      Average probability of detection: 0.3228 (range 0.159 to 0.5014)
      Scaling: g(max) = 1
      Log likelihood: -902.3 
      AICc: 1819
      
           Surveyed Units: 120 
         Individuals seen: 188 in 185 groups 
       Average group size: 1.016 
         Group size range: 1 to 2 
      Density in sampled area: 4.71e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 193361

# Gamma-NoCovarWloWhi

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, w.lo = w.20, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = w.20, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                   Estimate  SE       z       p(>|z|)  
      (Intercept)  4.225     0.05593  75.554  0.000e+00
      Shape        1.888     0.33253   5.679  1.357e-08
      
      Message: Success; Asymptotic SE's
      Function: GAMMA  
      Strip: 20 [m] to 200 [m] 
      Effective detection radius (EDR): 81.41 [m] 
      Probability of detection: 0.2045
      Scaling: g(max) = 1
      Log likelihood: -915.9 
      AICc: 1836
      
           Surveyed Units: 120 
         Individuals seen: 187 in 184 groups 
       Average group size: 1.016 
         Group size range: 1 to 2 
      Density in sampled area: 7.485e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 307265

# Gamma-NoCovarWlo

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, w.lo = w.20, expansions =
         0, series = "cosine", x.scl = w.20, g.x.scl = 1, outputUnits = "m")
      Coefficients:
                   Estimate  SE       z       p(>|z|)  
      (Intercept)  4.236     0.06353  66.683  0.000e+00
      Shape        1.741     0.28191   6.174  6.661e-10
      
      Message: Success; Asymptotic SE's
      Function: GAMMA  
      Strip: 20 [m] to 265 [m] 
      Effective detection radius (EDR): 82.99 [m] 
      Probability of detection: 0.1147
      Scaling: g(max) = 1
      Log likelihood: -982.1 
      AICc: 1968
      
           Surveyed Units: 120 
         Individuals seen: 195 in 192 groups 
       Average group size: 1.016 
         Group size range: 1 to 2 
      Density in sampled area: 7.511e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 308327

# Gamma-NoCovarsFt

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, outputUnits = "ft")
      Coefficients:
                   Estimate  SE       z        p(>|z|)  
      (Intercept)  5.680     0.04013  141.559  0.000e+00
      Shape        3.451     0.46074    7.489  6.935e-14
      
      Message: Success; Asymptotic SE's
      Function: GAMMA  
      Strip: 0 [ft] to 869.4 [ft] 
      Effective detection radius (EDR): 381.3 [ft] 
      Probability of detection: 0.1924
      Scaling: g(max) = 1
      Log likelihood: -1224 
      AICc: 2452
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.016 
         Group size range: 1 to 2 
      Density in sampled area: 3.575e-06 [1/ft^2]
      Abundance in 4.419e+10 [ft^2] study area: 157973

# Gamma-NoCovarCosExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", series = "cosine")
      Coefficients:
                   Estimate  SE       z        p(>|z|)
      (Intercept)  4.48720   0.07033  63.8059  0.00000
      Shape        5.10199   1.93584   2.6355  0.00840
      a1           0.71371   0.31135   2.2923  0.02189
      a2           0.05846   0.37279   0.1568  0.87539
      
      Message: Success; Asymptotic SE's
      Function: GAMMA with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 265 [m] 
      Effective detection radius (EDR): 83.7 [m] 
      Probability of detection: 0.09976
      Scaling: g(max) = 1
      Log likelihood: -991.6 
      AICc: 1991
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.016 
         Group size range: 1 to 2 
      Density in sampled area: 7.422e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 304656

# Gamma-NoCovarSinExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", series = "sine")
      Coefficients:
                   Estimate  SE       z        p(>|z|)  
      (Intercept)  4.5743    0.09348  48.9321  0.000e+00
      Shape        3.3923    0.61602   5.5069  3.653e-08
      a1           0.5790    0.48600   1.1914  2.335e-01
      a2           0.2878    0.28903   0.9958  3.193e-01
      
      Message: Success; Asymptotic SE's
      Function: GAMMA with 2 expansion(s) of SINE series 
      Strip: 0 [m] to 265 [m] 
      Effective detection radius (EDR): 131.9 [m] 
      Probability of detection: 0.2477
      Scaling: g(max) = 1
      Log likelihood: -994.1 
      AICc: 1996
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.016 
         Group size range: 1 to 2 
      Density in sampled area: 2.989e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 122690

# Gamma-NoCovarHermExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", series = "hermite")
      Coefficients:
                   Estimate  SE       z        p(>|z|)  
      (Intercept)  4.4046    0.09683  45.4884  0.000e+00
      Shape        5.9320    1.13643   5.2198  1.791e-07
      a1           0.1129    0.16530   0.6831  4.945e-01
      a2           0.2588    0.08129   3.1838  1.453e-03
      
      Message: Success; Asymptotic SE's
      Function: GAMMA with 2 expansion(s) of HERMITE series 
      Strip: 0 [m] to 265 [m] 
      Effective detection radius (EDR): 62.8 [m] 
      Probability of detection: 0.05615
      Scaling: g(max) = 1
      Log likelihood: -991.5 
      AICc: 1991
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.016 
         Group size range: 1 to 2 
      Density in sampled area: 0.0001318 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 541206

# Gamma-NoCovarSimpExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", series = "simple")
      Coefficients:
                   Estimate  SE        z        p(>|z|)  
      (Intercept)    4.205    0.07772  54.1002  0.000e+00
      Shape          5.793    0.96908   5.9779  2.261e-09
      a1           -13.733   17.78598  -0.7721  4.400e-01
      a2            89.091   82.00468   1.0864  2.773e-01
      
      Message: Success; Asymptotic SE's
      Function: GAMMA with 2 expansion(s) of SIMPLE series 
      Strip: 0 [m] to 265 [m] 
      Effective detection radius (EDR): 104.7 [m] 
      Probability of detection: 0.1562
      Scaling: g(max) = 1
      Log likelihood: -991.5 
      AICc: 1991
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.016 
         Group size range: 1 to 2 
      Density in sampled area: 4.74e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 194595

# Gamma-ContCovarExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ bare +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m")
      Coefficients:
                   Estimate  SE        z        p(>|z|)  
      (Intercept)  3.96655   0.218147  18.1829  7.048e-74
      bare         0.01300   0.006193   2.0992  3.580e-02
      Shape        5.11612   1.976177   2.5889  9.628e-03
      a1           0.68907   0.355031   1.9409  5.228e-02
      a2           0.03867   0.414055   0.0934  9.256e-01
      
      Message: Success; Asymptotic SE's
      Function: GAMMA with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 265 [m] 
      Average effective detection radius (EDR): 86.09 [m] (range 78.07 [m] to 98.15 [m]) 
      Average probability of detection: 0.1058 (range 0.08679 to 0.1372)
      Scaling: g(max) = 1
      Log likelihood: -988.3 
      AICc: 1987
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.016 
         Group size range: 1 to 2 
      Density in sampled area: 7.069e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 290162

---

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ bare +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", x.scl = xScl, g.x.scl = gXscl)
      Coefficients:
                   Estimate  SE        z        p(>|z|)  
      (Intercept)  3.96655   0.218147  18.1829  7.048e-74
      bare         0.01300   0.006193   2.0992  3.580e-02
      Shape        5.11612   1.976177   2.5889  9.628e-03
      a1           0.68907   0.355031   1.9409  5.228e-02
      a2           0.03867   0.414055   0.0934  9.256e-01
      
      Message: Success; Asymptotic SE's
      Function: GAMMA with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 265 [m] 
      Average effective detection radius (EDR): 86.09 [m] (range 78.07 [m] to 98.15 [m]) 
      Average probability of detection: 0.1058 (range 0.08679 to 0.1372)
      Scaling: g(max) = 0.75
      Log likelihood: -988.3 
      AICc: 1987
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.016 
         Group size range: 1 to 2 
      Density in sampled area: 7.069e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 290162

