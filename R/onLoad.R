.onLoad <- function(libname, pkgname){
	
	if(Sys.getenv("JAVA_HOME")!="") Sys.setenv(JAVA_HOME="")
	# options(java.parameters="-Xmx2g")
	# Initiate the Rstox envitonment:
	Definitions <- initiateRstoxEnv()
	# Set the Java memory:
	setJavaMemory(Definitions$JavaMem)
}
