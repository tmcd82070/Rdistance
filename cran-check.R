# Install GitHub version of package
require(devtools)
install_github("tmcd82070/Rdistance@master")


# Build and check the package for problems
devtools::check(pkg="Rdistance")
