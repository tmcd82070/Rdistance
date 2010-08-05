print.abund <- function( obj, ... ){
#
#   Print an object of class 'abund', which is class 'dfunc' with
#   an abundance estimate stored in it.
#

print.dfunc( obj )

cat( paste( "Abundance estimate: ", obj$n.hat, "(se=", obj$se.n.hat, ")\n"))
cat( "\n" )

}

