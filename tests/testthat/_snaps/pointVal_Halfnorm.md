# halfnorm-MinimumInputs

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~
         groupsize(groupsize), likelihood = lhood)
      Coefficients:
                   Estimate  SE          z         p(>|z|)
      (Intercept)  4.342358  0.03779929  114.8793  0      
      
      Message: Success; Asymptotic SE's
      Function: HALFNORM  
      Strip: 0 [m] to 265 [m] 
      Effective detection radius (EDR): 108.5937 [m] 
      Probability of detection: 0.1679257
      Scaling: g(0 [m]) = 1
      Log likelihood: -1004.259 
      AICc: 2010.54
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.015544 
         Group size range: 1 to 2 
      Density in sampled area: 4.408755e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 180979.4

# halfnorm-NoCovar

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, w.lo = w.lo, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = 0, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                   Estimate  SE          z         p(>|z|)
      (Intercept)  4.309857  0.04640169  92.88147  0      
      
      Message: Success; Asymptotic SE's
      Function: HALFNORM  
      Strip: 0 [m] to 200 [m] 
      Effective detection radius (EDR): 103.8266 [m] 
      Probability of detection: 0.269499
      Scaling: g(0 [m]) = 1
      Log likelihood: -937.906 
      AICc: 1877.834
      
           Surveyed Units: 120 
         Individuals seen: 188 in 185 groups 
       Average group size: 1.016216 
         Group size range: 1 to 2 
      Density in sampled area: 4.626042e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 189899

# halfnorm-ContinuousCovar

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ bare +
         groupsize(groupsize), likelihood = lhood, w.lo = w.lo, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = 0, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                   Estimate    SE           z          p(>|z|)     
      (Intercept)  3.79488634  0.356236368  10.652720  1.693431e-26
      bare         0.01309827  0.009144204   1.432412  1.520260e-01
      
      Message: Success; Asymptotic SE's
      Function: HALFNORM  
      Strip: 0 [m] to 200 [m] 
      Average effective detection radius (EDR): 103.8511 [m] (range 92.24027 [m] to 118.9106 [m]) 
      Average probability of detection: 0.2705556 (range 0.2127067 to 0.3534932)
      Scaling: g(0 [m]) = 1
      Log likelihood: -936.7678 
      AICc: 1877.602
      
           Surveyed Units: 120 
         Individuals seen: 188 in 185 groups 
       Average group size: 1.016216 
         Group size range: 1 to 2 
      Density in sampled area: 4.667449e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 191598.8

# halfnorm-FactorCovar

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ observer +
         groupsize(groupsize), likelihood = lhood, w.lo = w.lo, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = 0, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                    Estimate    SE         z           p(>|z|)     
      (Intercept)    4.7446344  0.2321695  20.4360791  7.990673e-93
      observerobs2  -0.1785698  0.2704647  -0.6602332  5.091042e-01
      observerobs3  -0.5716164  0.2689009  -2.1257511  3.352399e-02
      observerobs4  -0.8310846  0.2425831  -3.4259786  6.125887e-04
      observerobs5  -0.2451606  0.2796457  -0.8766829  3.806589e-01
      observerobs6  -0.6796994  0.2616972  -2.5972741  9.396689e-03
      
      Message: Success; Asymptotic SE's
      Function: HALFNORM  
      Strip: 0 [m] to 200 [m] 
      Average effective detection radius (EDR): 105.2945 [m] (range 70.80655 [m] to 143.5732 [m]) 
      Average probability of detection: 0.2967764 (range 0.1253392 to 0.5153313)
      Scaling: g(0 [m]) = 1
      Log likelihood: -916.6917 
      AICc: 1845.855
      
           Surveyed Units: 120 
         Individuals seen: 188 in 185 groups 
       Average group size: 1.016216 
         Group size range: 1 to 2 
      Density in sampled area: 5.632951e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 231232.7

# halfnorm-NoCovarWloWhi

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, w.lo = w.20, w.hi = w.hi,
         expansions = 0, series = "cosine", x.scl = w.20, g.x.scl = 1,
         outputUnits = "m")
      Coefficients:
                   Estimate  SE          z         p(>|z|)
      (Intercept)  4.085287  0.04171762  97.92714  0      
      
      Message: Success; Asymptotic SE's
      Function: HALFNORM  
      Strip: 20 [m] to 200 [m] 
      Effective detection radius (EDR): 83.65643 [m] 
      Probability of detection: 0.216
      Scaling: g(20 [m]) = 1
      Log likelihood: -919.5146 
      AICc: 1841.051
      
           Surveyed Units: 120 
         Individuals seen: 187 in 184 groups 
       Average group size: 1.016304 
         Group size range: 1 to 2 
      Density in sampled area: 7.087806e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 290954.4

# halfnorm-NoCovarWlo

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, w.lo = w.20, expansions =
         0, series = "cosine", x.scl = w.20, g.x.scl = 1, outputUnits = "m")
      Coefficients:
                   Estimate  SE          z         p(>|z|)
      (Intercept)  4.161495  0.03675837  113.2122  0      
      
      Message: Success; Asymptotic SE's
      Function: HALFNORM  
      Strip: 20 [m] to 265 [m] 
      Effective detection radius (EDR): 90.71538 [m] 
      Probability of detection: 0.1370975
      Scaling: g(20 [m]) = 1
      Log likelihood: -989.1671 
      AICc: 1980.355
      
           Surveyed Units: 120 
         Individuals seen: 195 in 192 groups 
       Average group size: 1.015625 
         Group size range: 1 to 2 
      Density in sampled area: 6.285527e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 258020.9

# halfnorm-NoCovarsFt

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, outputUnits = "ft")
      Coefficients:
                   Estimate  SE          z        p(>|z|)
      (Intercept)  5.530458  0.03779932  146.311  0      
      
      Message: Success; Asymptotic SE's
      Function: HALFNORM  
      Strip: 0 [ft] to 869.4226 [ft] 
      Effective detection radius (EDR): 356.2784 [ft] 
      Probability of detection: 0.1679257
      Scaling: g(0 [ft]) = 1
      Log likelihood: -1233.562 
      AICc: 2469.146
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.015544 
         Group size range: 1 to 2 
      Density in sampled area: 4.095867e-06 [1/ft^2]
      Abundance in 44185852261 [ft^2] study area: 180979.4

# halfnorm-NoCovarCosExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", series = "cosine")
      Coefficients:
                   Estimate   SE          z            p(>|z|)  
      (Intercept)   4.344444  0.04120517  105.4344370  0.0000000
      a1           -2.849472  2.91854393   -0.9763336  0.3288992
      a2           -4.712012  4.13360749   -1.1399272  0.2543166
      
      Message: Success; Asymptotic SE's
      Function: HALFNORM with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 265 [m] 
      Effective detection radius (EDR): 252.8361 [m] 
      Probability of detection: 0.9103037
      Scaling: g(0 [m]) = 1
      Log likelihood: -994.6299 
      AICc: 1995.387
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.015544 
         Group size range: 1 to 2 
      Density in sampled area: 8.132927e-06 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 33385.67

# halfnorm-NoCovarSinExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", series = "sine")
      Coefficients:
                   Estimate    SE         z            p(>|z|)    
      (Intercept)  4.50317096  0.0903461  49.84355761  0.000000000
      a1           1.01646536  0.3882265   2.61822779  0.008838778
      a2           0.02267565  0.2697459   0.08406298  0.933006355
      
      Message: Success; Asymptotic SE's
      Function: HALFNORM with 2 expansion(s) of SINE series 
      Strip: 0 [m] to 265 [m] 
      Effective detection radius (EDR): 135.2105 [m] 
      Probability of detection: 0.260333
      Scaling: g(0 [m]) = 1
      Log likelihood: -1000.449 
      AICc: 2007.025
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.015544 
         Group size range: 1 to 2 
      Density in sampled area: 2.843832e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 116739.3

# halfnorm-NoCovarHermExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", series = "hermite")
      Coefficients:
                   Estimate   SE           z           p(>|z|)  
      (Intercept)   4.201229   0.06771118  62.0463185  0.0000000
      a1            4.171719  13.08866695   0.3187276  0.7499331
      a2           -1.571586   5.02261571  -0.3129019  0.7543552
      
      Message: Success; Asymptotic SE's
      Function: HALFNORM with 2 expansion(s) of HERMITE series 
      Strip: 0 [m] to 265 [m] 
      Effective detection radius (EDR): 313.3249 [m] 
      Probability of detection: 1.397971 > 1
      Scaling: g(0 [m]) = 1 <- Check scaling
      Log likelihood: -1001.734 
      AICc: 2009.595
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.015544 
         Group size range: 1 to 2 
      Density in sampled area: 5.295843e-06 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 21739.44

# halfnorm-NoCovarSimpExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ 1 +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", series = "simple")
      Coefficients:
                   Estimate   SE         z          p(>|z|)  
      (Intercept)   4.377958  0.1054546  41.515114  0.0000000
      a1           -3.343047  2.5523143  -1.309810  0.1902600
      a2            4.282949  3.1515713   1.358988  0.1741503
      
      Message: Success; Asymptotic SE's
      Function: HALFNORM with 2 expansion(s) of SIMPLE series 
      Strip: 0 [m] to 265 [m] 
      Effective detection radius (EDR): 107.9682 [m] 
      Probability of detection: 0.165997
      Scaling: g(0 [m]) = 1
      Log likelihood: -1003.456 
      AICc: 2013.039
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.015544 
         Group size range: 1 to 2 
      Density in sampled area: 4.459981e-05 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 183082.2

# halfnorm-ContCovarExpansions

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ bare +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m")
      Coefficients:
                   Estimate     SE           z           p(>|z|)     
      (Intercept)   3.59388339  0.280581627  12.8086911  1.465863e-37
      bare          0.01877442  0.007085499   2.6496960  8.056423e-03
      a1           -3.33132879  3.348143913  -0.9949778  3.197471e-01
      a2           -4.99847222  4.432220114  -1.1277581  2.594221e-01
      
      Message: Success; Asymptotic SE's
      Function: HALFNORM with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 265 [m] 
      Average effective detection radius (EDR): 261.6523 [m] (range 216.4737 [m] to 323.074 [m]) 
      Average probability of detection: 0.9836834 82 of 193 P(detect) > 1
      Scaling: g(0 [m]) = 1 <- Check scaling
      Log likelihood: -990.9099 
      AICc: 1990.033
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.015544 
         Group size range: 1 to 2 
      Density in sampled area: 7.789121e-06 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 31974.34

---

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~ bare +
         groupsize(groupsize), likelihood = lhood, expansions = 2, outputUnits
         = "m", x.scl = xScl, g.x.scl = gXscl)
      Coefficients:
                   Estimate     SE           z           p(>|z|)     
      (Intercept)   3.59388339  0.280581627  12.8086911  1.465863e-37
      bare          0.01877442  0.007085499   2.6496960  8.056423e-03
      a1           -3.33132879  3.348143913  -0.9949778  3.197471e-01
      a2           -4.99847222  4.432220114  -1.1277581  2.594221e-01
      
      Message: Success; Asymptotic SE's
      Function: HALFNORM with 2 expansion(s) of COSINE series 
      Strip: 0 [m] to 265 [m] 
      Average effective detection radius (EDR): 261.6523 [m] (range 216.4737 [m] to 323.074 [m]) 
      Average probability of detection: 0.9836834 82 of 193 P(detect) > 1
      Scaling: g(0 [m]) = 0.75 <- Check scaling
      Log likelihood: -990.9099 
      AICc: 1990.033
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.015544 
         Group size range: 1 to 2 
      Density in sampled area: 7.789121e-06 [1/m^2]
      Abundance in 4.105e+09 [m^2] study area: 31974.34

# halfnorm-Bootstraps

    Code
      summary(fit)
    Output
      Call: dfuncEstim(data = thrasherDf, formula = dist ~
         groupsize(groupsize), likelihood = lhood)
      Coefficients:
                   Estimate  SE          z         p(>|z|)
      (Intercept)  4.342358  0.04053289  107.1317  0      
      
      Message: Success; Bootstrap SE's
      Function: HALFNORM  
      Strip: 0 [m] to 265 [m] 
      Effective detection radius (EDR): 108.5937 [m] 
      Probability of detection: 0.1679257
      Scaling: g(0 [m]) = 1
      Log likelihood: -1004.259 
      AICc: 2010.54
      
           Surveyed Units: 120 
         Individuals seen: 196 in 193 groups 
       Average group size: 1.015544 
         Group size range: 1 to 2 
      
      Density in sampled area: 4.408755e-05 [1/m^2]
                       95% CI: 3.558164e-05 [1/m^2] to 5.061822e-05 [1/m^2]
      
      Abundance in 4.105e+09 [m^2] study area: 180979.4
                                       95% CI: 146062.6 to 207787.8

