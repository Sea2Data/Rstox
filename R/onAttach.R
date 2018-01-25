.onAttach <- function(libname, pkgname){
	
	packageStartupMessage("Rstox_1.8.1
**********
If problems with Java Memory such as java.lang.OutOfMemoryError occurs, try increasing the Java memory by running setJavaMemory(4e9), and possibly using an even higher value than 4 gigabytes
**********
", appendLF=FALSE)
}
