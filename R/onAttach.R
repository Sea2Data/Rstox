.onAttach <- function(libname, pkgname){
	
	packageStartupMessage("Rstox_1.9.2
**********
WARNING: This version of Rstox is an unofficial/developer version and bugs should be expected.
If problems with Java Memory such as java.lang.OutOfMemoryError occurs, see ?setJavaMemory.
**********
", appendLF=FALSE)
}
