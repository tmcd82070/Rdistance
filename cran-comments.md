# Submission comments

This submission (v4.3.1) contains a critical bug fix. Prior versions 
incorrectly reported ESW for scaling factors (`g.x.scl`) less than 1.0.

# Current submission

* 4.3.1

# Prior submission

* 4.3.0   

# Local Check

R CMD check --as-cran results

No ERRORs or WARNINGs. 

local environment:
Windows 11 Pro, R version 4.5.2

# Rhub Checks (via Github actions)

No ERRORs or WARNINGS with current R version on Linux, MACOS, and Windows.

# Spelling

All words identified by devtools::spell_check() are R code (in documentation)
or names. I do not consider any of the list to be miss-spellings.


# Downstream dependencies

None known
