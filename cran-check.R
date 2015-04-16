# GitHub version of package
require(devtools)
install_github("tmcd82070/Rdistance/Rdistance")


# Build and check the package for problems
devtools::check(pkg="Rdistance")


# Fatal error

# * checking PDF version of manual ...Warning: running command '"C:/PROGRA~1/R/R-31~1.3/bin/x64/Rcmd.exe" Rd2pdf  --batch --no-preview --build-dir="C:/Users/Jason/AppData/Local/Temp/RtmpWAzEK5/Rd2pdf27507ea37cb3" --no-clean -o  Rdistance-manual.pdf  "C:/Users/Jason/AppData/Local/Temp/Rtmpgj16UH/Rdistance.Rcheck/Rdistance"' had status 1
# WARNING
# LaTeX errors when creating PDF version.
# This typically indicates Rd problems.
# LaTeX errors found:
#   ! pdfTeX error (ext4): \pdfendlink ended up in different nesting level than \pd
# fstartlink.
# \AtBegShi@Output ...ipout \box \AtBeginShipoutBox 
# \fi \fi 
# l.143 B
# !  ==> Fatal error occurred, no output PDF file produced!
#   * checking PDF version of manual without hyperrefs or index ... OK