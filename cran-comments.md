# Submission comments

Prior version of this package (v 2.1.5) was archived because I did not respond
to requests to fix an issue. The reason I did not respond
to notes from CRAN is that 
I left my prior place of employment and lost access to my old email: tmcdonald@west-inc.com. 
People at my former employer promised to forward emails, but they did not. My 
former employer and I also 
had some miss-understandings on who was maintaining the package. 

I will remain the package maintainer. 
You may email jwalrath@west-inc.com to confirm that 
my old email address is dead and that I have permission to continue maintaining 
this package. 

This new version (2.2.1) is a major update as follows:

* Extensive use of the `units` package has been included. 
* Many bugs associated with options that are rarely used have been fixed. 
* Documentation has been extensively updated. 
* I removed some problematic vignettes, but will be adding them back as I can.  
I include a revamped "beginner" vignette. 
* Testing functions have been updated.

Details on the specific changes are in **NEWS**. 

# Current submission

* 3.0.0

# Prior submission

* 2.1.5 - Archived.  

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
