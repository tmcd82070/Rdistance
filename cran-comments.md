# Submission comments

This submission (v3.1.3) contains three patches. Two patches 
address NOTES during  CRAN checks that flagged a missing 
braces in documentation equations. One bug fix was related to changes 
changes in the 'units' package. 

Plot method failed on new 'units' package due to use of 'rbind' 
inside 'barplot'.  While 'barplot' should be changed to not 
call 'rbind' with a mix of unit and unitless object, I was able
to safely remove units in the call to 'barplot', and everything 
works. 

Details on the specific changes are in **NEWS**. 

# Current submission

* 3.1.3

# Prior submission

* 3.0.0   

# Local Check

R CMD check --as-cran results

No ERRORs or WARNINGs. 

local environment:
Windows 11 Pro, R version 4.4.3 (2025-02-28) -- "Trophy Case"

# Rhub Checks (via Github actions)

Builds failed on re-build vignettes because pdflatex was not 
present.  Vignettes build when pdflatex is present. 

```
<snip>
* creating vignettes ... ERROR
Error: --- re-building 'Extended_dfuncEstim_Examples.Rmd' using rmarkdown
Warning in system2(..., stdout = if (use_file_stdout()) f1 else FALSE, stderr = f2) :
  '"pdflatex"' not found
Error: Error: processing vignette 'Extended_dfuncEstim_Examples.Rmd' failed with diagnostics:
LaTeX failed to compile C:/Users/runneradmin/AppData/Local/Temp/RtmpgT4aiT/Rbuild112c3fb76dd7/Rdistance/vignettes/Extended_dfuncEstim_Examples.tex. See https://yihui.org/tinytex/r/#debugging for debugging tips.
--- failed re-building 'Extended_dfuncEstim_Examples.Rmd'
--- re-building 'Rdistance_BeginnerLineTransect.Rmd' using rmarkdown
Warning in system2(..., stdout = if (use_file_stdout()) f1 else FALSE, stderr = f2) :
  '"pdflatex"' not found
```

# Spelling

All words identified by devtools::spell_check() are R code (in documentation)
or names. I do not consider any of the list to be miss-spellings.


# Downstream dependencies

None known
