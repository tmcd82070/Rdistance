# Submission comments

This submission (v3.1.3) contains three patches. Two patches 
address NOTES during  CRAN checks that flagged a missing 
braces in documentation equations. One bug fix was related to changes 
changes in the 'units' package. 

Details on the specific changes are in **NEWS**. 

# Current submission

* 3.1.3

# Prior submission

* 3.0.0   

# Local Check

R CMD check --as-cran results

No ERRORs or WARNINGs. 

local environment:
Windows 11 Pro, R version 4.2.3 (2023-03-15 ucrt) -- "Shortstop Beagle"

* Unix (via Travis) : build passing

# Winbuilder Check

Passing with one note,

```
* checking CRAN incoming feasibility ... [8s] NOTE
Maintainer: 'Trent McDonald <trent@mcdonalddatasciences.com>'

New submission

Package was archived on CRAN

Possibly misspelled words in DESCRIPTION:
  Rdistance (31:3)
  covariates (34:21)
  glm (34:12)
  lm (34:6)

CRAN repository db overrides:
  X-CRAN-Comment: Archived on 2022-10-03 as check issues were not
    corrected despite reminders.
```

**Those words are not misspelled. **

# Rhub Checks

## debian-gcc-release

```
* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Trent McDonald <trent@mcdonalddatasciences.com>’

New submission

Package was archived on CRAN

Possibly misspelled words in DESCRIPTION:
  glm (34:12)
  lm (34:6)
  Rdistance (31:3)

CRAN repository db overrides:
  X-CRAN-Comment: Archived on 2022-10-03 as check issues were not
    corrected despite reminders.
```

## ubuntu-gcc-release

```
* checking CRAN incoming feasibility ... [6s/13s] NOTE
Maintainer: ‘Trent McDonald <trent@mcdonalddatasciences.com>’

New submission

Package was archived on CRAN

Possibly misspelled words in DESCRIPTION:
  glm (34:12)
  lm (34:6)
  Rdistance (31:3)

CRAN repository db overrides:
  X-CRAN-Comment: Archived on 2022-10-03 as check issues were not
    corrected despite reminders.
```

## windows-x86-64-release

```
--- re-building 'Extended_dfuncEstim_Examples.Rmd' using rmarkdown
! Sorry, but C:\PROGRA~1\MiKTeX\miktex\bin\x64\pdflatex.exe did not succeed.
```

This appears to be a Latex issue on the rhub instance. The vignettes build without errors on local machine and on Win Builder, Ubuntu, and Debian. No other errors or issues. Same note as the Linux systems re maintainer. 

# Downstream dependencies

None known
