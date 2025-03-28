# Submission comments

This submission (v4.0.2) represents a substantial re-write 
of nearly every routine contained in prior releases.  
I felt the re-write was necessary to 
improve stability of the code (prior code had a lot of "spaghetti" code) 
and lay the ground work for future functionality
improvements. I have been working on this version for over a year. 
Documentation has been substantially improved.  Testing files have 
been substantially tightened and focuses.  

Details on the specific changes are in **NEWS**. 

# Current submission

* 4.0.2

# Prior submission

* 3.1.4   

# Local Check

R CMD check --as-cran results

No ERRORs or WARNINGs. 

local environment:
Windows 11 Pro, R version 4.4.3 (2025-02-28) -- "Trophy Case"

# Rhub Checks (via Github actions)

No ERRORs or WARNINGS with current R version on Linux, MACOS, and Windows.

# Spelling

All words identified by devtools::spell_check() are R code (in documentation)
or names. I do not consider any of the list to be miss-spellings.


# Downstream dependencies

None known
