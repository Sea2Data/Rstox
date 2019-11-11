x <- Sys.time()

.onAttach <- function(libname, pkgname){
	packageStartupMessage("Rstox_1.11.1 \n**********\nWARNING: This version of Rstox is an unofficial/developer version and bugs should be expected.\nBuild time:", x,"\nIf problems with Java Memory such as java.lang.OutOfMemoryError occurs, see ?setJavaMemory.\n**********\n", appendLF=FALSE)
}
