# Gamma-MinimumInputs

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~
         groupsize(groupsize), likelihood = lhood)
      Coefficients:
                   Estimate  SE          z           p(>|z|)     
      (Intercept)  4.492178  0.04013058  111.939024  0.000000e+00
      Shape        3.450558  0.46077676    7.488567  6.962965e-14
      
      Message: Success; Asymptotic SE's
      Function: GAMMA  
      Strip: 0 [m] to 265 [m] 
      Effective detection radius (EDR): 116.2324 [m] 
      Probability of detection: 0.1923813
      Scaling: g(max) = 1
      Log likelihood: -994.9024 
      AICc: 1993.868
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.015544 
         Group size range: 1 to 2 
      Density in sampled area: 3.848313e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 157973.3

# Gamma-NoCovar

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, w.lo = w.lo, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = 0, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                   Estimate  SE          z          p(>|z|)     
      (Intercept)  4.441349  0.05007284  88.697763  0.000000e+00
      Shape        3.872593  0.56457227   6.859341  6.917917e-12
      
      Message: Success; Asymptotic SE's
      Function: GAMMA  
      Strip: 0 [m] to 200 [m] 
      Effective detection radius (EDR): 111.7684 [m] 
      Probability of detection: 0.3123046
      Scaling: g(max) = 1
      Log likelihood: -928.0582 
      AICc: 1860.182
      
           Surveyed Units: 120 
         Individuals seen: 188 in 185 groups 
       Average group size: 1.016216 
         Group size range: 1 to 2 
      Density in sampled area: 3.99198e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 163870.8

# Gamma-ContinuousCovar

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ bare +
         groupsize(groupsize), likelihood = lhood, w.lo = w.lo, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = 0, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                   Estimate    SE           z          p(>|z|)     
      (Intercept)  3.95235091  0.285437564  13.846639  1.333050e-43
      bare         0.01233923  0.007275172   1.696074  8.987195e-02
      Shape        3.94262797  0.572020010   6.892465  5.483374e-12
      
      Message: Success; Asymptotic SE's
      Function: GAMMA  
      Strip: 0 [m] to 200 [m] 
      Average effective detection radius (EDR): 111.8055 [m] (range 99.73775 [m] to 127.7451 [m]) 
      Average probability of detection: 0.3135306 (range 0.2486905 to 0.40797)
      Scaling: g(max) = 1
      Log likelihood: -926.5957 
      AICc: 1859.324
      
           Surveyed Units: 120 
         Individuals seen: 188 in 185 groups 
       Average group size: 1.016216 
         Group size range: 1 to 2 
      Density in sampled area: 4.02479e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 165217.6

# Gamma-FactorCovar

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ observer +
         groupsize(groupsize), likelihood = lhood, w.lo = w.lo, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = 0, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                    Estimate     SE         z           p(>|z|)     
      (Intercept)    4.64482264  0.1093150  42.4902663  0.000000e+00
      observerobs2  -0.08019707  0.1206687  -0.6646054  5.063029e-01
      observerobs3  -0.37407231  0.1392098  -2.6871125  7.207268e-03
      observerobs4  -0.61325023  0.1121436  -5.4684355  4.540252e-08
      observerobs5  -0.15904093  0.1294404  -1.2286809  2.191915e-01
      observerobs6  -0.44643659  0.1345852  -3.3171303  9.094720e-04
      Shape          5.32161264  0.7213106   7.3776993  1.610482e-13
      
      Message: Success; Asymptotic SE's
      Function: GAMMA  
      Strip: 0 [m] to 200 [m] 
      Average effective detection radius (EDR): 111.0199 [m] (range 79.75062 [m] to 141.6209 [m]) 
      Average probability of detection: 0.3227822 (range 0.159004 to 0.5014118)
      Scaling: g(max) = 1
      Log likelihood: -902.2717 
      AICc: 1819.176
      
           Surveyed Units: 120 
         Individuals seen: 188 in 185 groups 
       Average group size: 1.016216 
         Group size range: 1 to 2 
      Density in sampled area: 4.71037e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 193360.7

# Gamma-NoCovarWloWhi

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, w.lo = w.20, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = w.20, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                   Estimate  SE          z          p(>|z|)     
      (Intercept)  4.225470  0.05594311  75.531545  0.000000e+00
      Shape        1.888105  0.33262871   5.676315  1.376272e-08
      
      Message: Success; Asymptotic SE's
      Function: GAMMA  
      Strip: 20 [m] to 200 [m] 
      Effective detection radius (EDR): 81.40337 [m] 
      Probability of detection: 0.2045219
      Scaling: g(max) = 1
      Log likelihood: -915.9168 
      AICc: 1835.9
      
           Surveyed Units: 120 
         Individuals seen: 187 in 184 groups 
       Average group size: 1.016304 
         Group size range: 1 to 2 
      Density in sampled area: 7.485584e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 307283.2

# Gamma-NoCovarWlo

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, w.lo = w.20, expansions =
         0, series = "cosine", x.scl = w.20, g.x.scl = 1, outputUnits = "m")
      Coefficients:
                   Estimate  SE          z          p(>|z|)     
      (Intercept)  4.236249  0.06359697  66.610863  0.000000e+00
      Shape        1.739971  0.28201116   6.169865  6.834828e-10
      
      Message: Success; Asymptotic SE's
      Function: GAMMA  
      Strip: 20 [m] to 265 [m] 
      Effective detection radius (EDR): 82.97645 [m] 
      Probability of detection: 0.1147037
      Scaling: g(max) = 1
      Log likelihood: -982.0773 
      AICc: 1968.218
      
           Surveyed Units: 120 
         Individuals seen: 195 in 192 groups 
       Average group size: 1.015625 
         Group size range: 1 to 2 
      Density in sampled area: 7.51266e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 308394.7

# Gamma-NoCovarsFt

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, outputUnits = "ft")
      Coefficients:
                   Estimate  SE          z           p(>|z|)     
      (Intercept)  5.680278  0.04012645  141.559433  0.000000e+00
      Shape        3.450558  0.46072926    7.489339  6.922123e-14
      
      Message: Success; Asymptotic SE's
      Function: GAMMA  
      Strip: 0 [ft] to 869.4226 [ft] 
      Effective detection radius (EDR): 381.3399 [ft] 
      Probability of detection: 0.1923813
      Scaling: g(max) = 1
      Log likelihood: -1224.206 
      AICc: 2452.474
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.015544 
         Group size range: 1 to 2 
      Density in sampled area: 3.5752e-06 [1/ft^2]
      Abundance in 44185852261 [ft^2] study area: 157973.3

# Gamma-NoCovarCosExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", series = "cosine")
      Coefficients:
                   Estimate    SE         z           p(>|z|)   
      (Intercept)  4.48720036  0.0703259  63.8057989  0.00000000
      Shape        5.10197630  1.9358481   2.6355252  0.00840072
      a1           0.71370935  0.3113536   2.2922788  0.02188956
      a2           0.05845747  0.3727904   0.1568106  0.87539415
      
      Message: Success; Asymptotic SE's
      Function: GAMMA with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 265 [m] 
      Effective detection radius (EDR): 83.6981 [m] 
      Probability of detection: 0.09975609
      Scaling: g(max) = 1
      Log likelihood: -991.6059 
      AICc: 1991.425
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.015544 
         Group size range: 1 to 2 
      Density in sampled area: 7.421536e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 304654.1

# Gamma-NoCovarSinExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", series = "sine")
      Coefficients:
                   Estimate   SE          z          p(>|z|)     
      (Intercept)  4.5742838  0.09348163  48.932433  0.000000e+00
      Shape        3.3923810  0.61600770   5.507043  3.649107e-08
      a1           0.5790045  0.48600236   1.191362  2.335117e-01
      a2           0.2878307  0.28902264   0.995876  3.193104e-01
      
      Message: Success; Asymptotic SE's
      Function: GAMMA with 2 expansion(s) of SINE series 
      Strip: 0 [m] to 265 [m] 
      Effective detection radius (EDR): 131.8908 [m] 
      Probability of detection: 0.2477065
      Scaling: g(max) = 1
      Log likelihood: -994.0896 
      AICc: 1996.392
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.015544 
         Group size range: 1 to 2 
      Density in sampled area: 2.988792e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 122689.9

# Gamma-NoCovarHermExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", series = "hermite")
      Coefficients:
                   Estimate   SE          z           p(>|z|)     
      (Intercept)  4.4045794  0.09682677  45.4892716  0.000000e+00
      Shape        5.9319416  1.13643554   5.2197783  1.791374e-07
      a1           0.1129147  0.16529804   0.6830974  4.945453e-01
      a2           0.2588048  0.08128718   3.1838328  1.453389e-03
      
      Message: Success; Asymptotic SE's
      Function: GAMMA with 2 expansion(s) of HERMITE series 
      Strip: 0 [m] to 265 [m] 
      Effective detection radius (EDR): 62.79735 [m] 
      Probability of detection: 0.05615531
      Scaling: g(max) = 1
      Log likelihood: -991.484 
      AICc: 1991.181
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.015544 
         Group size range: 1 to 2 
      Density in sampled area: 0.0001318385 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 541197.2

# Gamma-NoCovarSimpExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", series = "simple")
      Coefficients:
                   Estimate    SE           z          p(>|z|)     
      (Intercept)    4.204719   0.07772137  54.099904  0.000000e+00
      Shape          5.793019   0.96908437   5.977827  2.261336e-09
      a1           -13.732843  17.78603095  -0.772114  4.400469e-01
      a2            89.090671  82.00502578   1.086405  2.772998e-01
      
      Message: Success; Asymptotic SE's
      Function: GAMMA with 2 expansion(s) of SIMPLE series 
      Strip: 0 [m] to 265 [m] 
      Effective detection radius (EDR): 104.7258 [m] 
      Probability of detection: 0.1561765
      Scaling: g(max) = 1
      Log likelihood: -991.5073 
      AICc: 1991.227
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.015544 
         Group size range: 1 to 2 
      Density in sampled area: 4.740428e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 194594.6

# Gamma-ContCovarExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ bare +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m")
      Coefficients:
                   Estimate    SE           z            p(>|z|)     
      (Intercept)  3.96655399  0.218152620  18.18247240  7.105677e-74
      bare         0.01300064  0.006193518   2.09907115  3.581063e-02
      Shape        5.11607593  1.976388809   2.58859790  9.636755e-03
      a1           0.68905423  0.355088768   1.94051261  5.231742e-02
      a2           0.03865736  0.414125635   0.09334695  9.256279e-01
      
      Message: Success; Asymptotic SE's
      Function: GAMMA with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 265 [m] 
      Average effective detection radius (EDR): 86.09498 [m] (range 78.07162 [m] to 98.15196 [m]) 
      Average probability of detection: 0.1058506 (range 0.08679498 to 0.1371849)
      Scaling: g(max) = 1
      Log likelihood: -988.2737 
      AICc: 1986.868
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.015544 
         Group size range: 1 to 2 
      Density in sampled area: 7.068281e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 290152.9

---

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ bare +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", x.scl = xScl, g.x.scl = gXscl)
      Coefficients:
                   Estimate    SE           z            p(>|z|)     
      (Intercept)  3.96655399  0.218152620  18.18247240  7.105677e-74
      bare         0.01300064  0.006193518   2.09907115  3.581063e-02
      Shape        5.11607593  1.976388809   2.58859790  9.636755e-03
      a1           0.68905423  0.355088768   1.94051261  5.231742e-02
      a2           0.03865736  0.414125635   0.09334695  9.256279e-01
      
      Message: Success; Asymptotic SE's
      Function: GAMMA with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 265 [m] 
      Average effective detection radius (EDR): 86.09498 [m] (range 78.07162 [m] to 98.15196 [m]) 
      Average probability of detection: 0.1058506 (range 0.08679498 to 0.1371849)
      Scaling: g(max) = 0.75
      Log likelihood: -988.2737 
      AICc: 1986.868
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.015544 
         Group size range: 1 to 2 
      Density in sampled area: 7.068281e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 290152.9

