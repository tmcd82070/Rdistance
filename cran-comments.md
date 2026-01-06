# Submission comments

This submission (v4.3.0) contains increased functionality - two new 
features.  I implemented parallel processing of my bootstrap resampling 
routine.  I added a likelihood (Gamma) to the suite of available curves 
that can be fitted. 

Details on the changes between major releases are in **NEWS**. 

# Current submission

* 4.3.0

# Prior submission

* 4.1.1   

# Local Check

R CMD check --as-cran results

No ERRORs or WARNINGs. 

local environment:
Windows 11 Pro, R version 4.5.1

# Rhub Checks (via Github actions)

No ERRORs or WARNINGS with current R version on Linux, MACOS, and Windows.

# Spelling

All words identified by devtools::spell_check() are R code (in documentation)
or names. I do not consider any of the list to be miss-spellings.


# Downstream dependencies

None known
