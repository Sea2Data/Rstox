.onLoad <- function(libname, pkgname){
	
	Sys.setenv(JAVA_HOME="")
	options(java.parameters="-Xmx2g")
# Create a Rstox environment in which the baseline objects of the various projects are placed. This allows for a check for previously run baseline models and avoids memory leakage:
	assign("RstoxEnv", new.env(), envir=.GlobalEnv)
	assign("StoXFolders", c("input", "output", "process"), envir=get("RstoxEnv"))
	assign("NMD_data_types", c("echosounder", "biotic", "landing"), envir=get("RstoxEnv"))
	assign("StoX_data_types", c("acoustic", "biotic", "landing"), envir=get("RstoxEnv"))
	assign("StoX_data_type_keys", c(acoustic="echosounder_dataset", biotic="missions xmlns", landing="Sluttseddel"), envir=get("RstoxEnv"))
	assign("bootstrapTypes", c("Acoustic", "SweptArea"), envir=get("RstoxEnv"))
	assign("processLevels", c("bootstrap", "bootstrapImpute"), envir=get("RstoxEnv"))
}
