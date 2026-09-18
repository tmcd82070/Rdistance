# Submission comments

This submission (v4.5.0) contains substantial updates to two branches of the 
code.  (1) This version updates the optimizer for non-smooth distance functions.
I have changed the default method for optimization from nlminb to OSCARS for 
non-smooth distance functions. OSCARS is better but slower and execution time 
estimates have been added to the command line interface. (2) This version add
routines that make it possible to compute the spacing of line transects given
a study area polygon and constraints such as total kilometers. 

Added dependency on 'sf' and 'OSCARS'. 

# Current submission

* 4.5.0

# Prior submission

* 4.4.3   

# Local Check

R CMD check --as-cran results

No ERRORs or WARNINGs. 

local environment:
Windows 11 Pro
R.version.string = "R version 4.6.0 (2026-04-24 ucrt)"

# Rhub Checks (via Github actions)

No ERRORs or WARNINGS with current R version on Linux, MACOS, and Windows.

# Spelling

All words identified by spelling::spell_check() are R code (in documentation)
or names. I do not consider any of the list to be miss-spellings.


# Downstream dependencies

None known
