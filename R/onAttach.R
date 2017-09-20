.onAttach <- function(libname, pkgname){
	packageStartupMessage("Rstox_1.6
**********
If problems with Java Memory such as java.lang.OutOfMemoryError occurs, try increasing the Java memory by running options(java.parameters=\"-Xmx4g\"), and possibly using an even higher value than 4g
**********
", appendLF=FALSE)
}
