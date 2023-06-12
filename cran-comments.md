# Submission comments

Prior version of this package (v 2.1.5) was archived.  The reason I did not respond
to notes from CRAN is that 
I left my prior place of employment and lost access to my old email: tmcdonald@west-inc.com. 
People at my former employer promised to forward emails, but they did not. My 
former employer and I also 
had some miss-understandings on who was maintaining the package. 

I will remain the package maintainer for the foreseeable future. 
I apploogize for the mixup.  You can email jwalrath@west-inc.com to confirm that 
my old email address is dead and that I have permission to continue maintaining 
this package. 

This new version (2.2.1) is a major update as follows:

* Extensive use of the `units` package has been included. 
* Many bugs associated with options that are rarely used have been fixed. 
* Documentation has been extensively updated. 
* I removed some problematic vignettes, but will be adding them back as I can.  
I include a revamped "beginner" vignette. 
* Testing functions have been updated.

# Current submission

* 2.2.1

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

# Rhub Check



# Ubuntu Linux 16.04 LTS 


# Fedora Linux : Latex warning


## Downstream dependencies
None known
