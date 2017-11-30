.onLoad <- function(libname, pkgname){
	
	if(Sys.getenv("JAVA_HOME")!="") Sys.setenv(JAVA_HOME="")
	options(java.parameters="-Xmx2g")
# Create a Rstox environment in which the baseline objects of the various projects are placed. This allows for a check for previously run baseline models and avoids memory leakage:
	assign("RstoxEnv", new.env(), envir=.GlobalEnv)
	# Assign fundamental variables to the RstoxEnv:
	Definitions <- list(
		StoXFolders = c("input", "output", "process"), 
		NMD_data_types = c("echosounder", "biotic", "landing"), 
		StoX_data_types = c("acoustic", "biotic", "landing"), 
		StoX_data_type_keys = c(acoustic="echosounder_dataset", biotic="missions xmlns", landing="Sluttseddel"), 
		model_types = c("AcousticTrawl", "SweptAreaLength", "SweptAreaTotal"), 
		processLevels = c("bootstrap", "bootstrapImpute")
		)
	assign("Definitions", Definitions, envir=get("RstoxEnv"))
	assign("Projects", list(), envir=get("RstoxEnv"))
}
