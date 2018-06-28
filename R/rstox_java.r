#*********************************************
#*********************************************
#' Initialize rJava
#' 
#' This funcion initializes the connection to Java.
#' 
#' @import grDevices
#' @import graphics
#' @import stats
#' @import utils
#'
#' @importFrom rJava .jpackage
#' @export
#' @keywords internal
#' 
Rstox.init <- function() {
	# Package initialization of rJava. Note that the documentatino of this functions also contains importing of the four packages grDevices, graphics, stats and utils. This is a bit of cheating, but avoids using :: for such common functions.
	pkgname <- "Rstox";
	loc = dirname(path.package(pkgname))
	# rJava - load jar files in package java directory 
	.jpackage(pkgname, lib.loc=loc)
}


#*********************************************
#*********************************************
#' Get available StoX functions
#' 
#' This funcion gets available StoX functions.
#' 
#' @importFrom rJava .jnew
#' @export
#' @keywords internal
#' 
getAvailableFunctions <- function(){
	if(length(getRstoxEnv()$Definitions$availableFunctions) == 0){
		# The functions J and .jnew and other functions in the rJava package needs initialization:
		Rstox.init()
		projectName <- .jnew("no/imr/stox/model/Project")
		functions <- projectName$getLibrary()$getMetaFunctions()$toString()
		functions <- JavaString2vector(functions)
		
		categories <- sapply(functions, function(x) projectName$getLibrary()$findMetaFunction(x)$getCategory())
		functions <- split(functions, categories)
		
		RstoxEnv$Definitions$availableFunctions <- functions
		#if(!identical(type, "all")){
		#	categories <- sapply(functions, function(x) projectName$getLibrary()$findMetaFunction(x)$getCategory())
		#	functions <- functions[categories %in% type]
		#}
	}
	else{
		functions <- getRstoxEnv()$Definitions$availableFunctions
	}
	return(functions)
}


#*********************************************
#*********************************************
#' Add processes to a project, given output from getParlist():
#' 
#' This function extracts the project names and adds function names where these are not given, and adds processes to the project.
#' 
#' @param project	A project object.
#' @param processes	A list of processes as returned from \code{\link{getParlist}}.
#' 
#' @export
#' @importFrom rJava .jrcall
#' @keywords internal
#' 
addProcesses <- function(project, processes){
	# Function for adding one process. This should be replaced by something less akward, where .jcall should be used and the function name given as input to .jcall instead of the series of if and else:
	addOneProcess <- function(i, project, modelFun, processNames, functionNames){
		getProject(project, modelFun[i])$addProcess(processNames[i], functionNames[i])
		###
		###if(modelFun[i]=="getBaseline"){
		###	project$getBaseline()$addProcess(processNames[i], functionNames[i])
		###}
		###else if(modelFun[i]=="getBaselineReport"){
		###	project$getBaselineReport()$addProcess(processNames[i], functionNames[i])
		###}
		###else if(modelFun[i]=="getRModel"){
		###	project$getRModel()$addProcess(processNames[i], functionNames[i])
		###}
		###else if(modelFun[i]=="getRModelReport"){
		###	project$getRModelReport()$addProcess(processNames[i], functionNames[i])
		###}
		###else{
		###	stop("Invalid 'modelFun'")
		###}
	}
	
	if(!is.list(processes)){
		stop("processes must be a list of elements 'processName' (as a string naming the process with no parameters) or processName = list(par1 = value, par2 = value, ...)")
	}
	# Discard duplicated process names, since we are only using the process and function names, and not the parameter values, which are treated by setBaselineParameters():
	processes <- processes[!duplicated(names(processes))]
	# Get the processes names and realted function names:
	processNames <- names(processes)
	functionNames <- sapply(processes, function(x) if(length(x$functionName)) x$functionName else "")
	empty <- nchar(functionNames)==0
	functionNames[empty] <- processNames[empty]
	
	# Remove the processes with invalid functions:
	availableFunctions <- getAvailableFunctions()
	availableFunctionsFlat <- unlist(availableFunctions)
	modelTypes <- rep(names(availableFunctions), sapply(availableFunctions, length))
	valid <- functionNames %in% unlist(availableFunctions)
	if(any(!valid)){
		warning(paste0("The following functions were not recognized (use getAvailableFunctions() to get a list of available functions in Stox):\n"), paste(functionNames[!valid], sep="\n"))
	}
	functionNames <- functionNames[valid]
	processNames <- processNames[valid]
	
	# Identify the model types of the processes:
	modelFun <- modelTypes[match(functionNames, availableFunctionsFlat)]
	### # Match with the defined modelTypeJavaNames, and extract the corresponding modelTypeJavaFuns:
	### modelTypeJavaNames <- getRstoxEnv()$Definitions$modelTypeJavaNames
	### modelTypeJavaFuns <- getRstoxEnv()$Definitions$modelTypeJavaFuns
	### modelFun <- modelTypeJavaFuns[match(modelFun, modelTypeJavaNames)]
	
	# Add the processes:
	#lapply(seq_along(processNames), function(i) project$getBaseline()$addProcess(processNames[i], functionNames[i]))
	lapply(seq_along(processNames), addOneProcess, project, modelFun, processNames, functionNames)
	return(list(processNames=processNames, functionNames=functionNames))
}


#*********************************************
#*********************************************
#' Convert a Java string to R vector
#' 
#' When reading some data from the StoX Java memory using toString(), the resulting string is converted to a string vector by this function.
#' 
#' @param x	A Java string imported from the StoX Java library.
#' 
#' @export
#' @keywords internal
#' 
JavaString2vector <- function(x){
	x <- gsub("[", "", x, fixed=TRUE)
	x <- gsub("]", "", x, fixed=TRUE)
	strsplit(x, ", ")[[1]]
}


#*********************************************
#*********************************************
#' Wrap basic R objects into Java objects
#' 
#' Wraps a value \code{i} into a Java integer, double, og Boolean for use in Java functions accessable by the package Rstox.
#' 
#' @param i	A single integer, double, or Boolean
#'
#' @return A Java integer, double, or Boolean
#'
#' @importFrom rJava .jnew
#' @export
#' @keywords internal
#' @rdname jInt
#' 
jInt <- function(i) {
	.jnew("java/lang/Integer", as.integer(i))
}
#' 
#' @importFrom rJava .jnew
#' @export
#' @keywords internal
#' @rdname jInt
#' 
jDouble <- function(i) {
	.jnew("java/lang/Double", as.double(i))
}
#' 
#' @importFrom rJava .jnew
#' @export
#' @keywords internal
#' @rdname jInt
#' 
jBoolean <- function(i) {
	.jnew("java/lang/Boolean", i)
}


#*********************************************
#*********************************************
#' Convert a storage object into a dataframe representation
#' 
#' \code{getDataFrame} converts a StoX storage object into a dataframe, and is used by the more user friendly \code{getDataFrame1} to convert a baseline object to dataframe. \cr \cr
#' \code{getProcessDataTableAsDataFrame} gets a joined table with meanNASC, psu, stratum, and area. Reads transect data, strata and area information from baseline Java object and merges them into one data frame.
#' 
#' @param baseline		A StoX baseline object
#' @param projectName  	The name or full path of the project, a baseline object (as returned from getBaseline() or runBaseline()), og a project object (as returned from open).
#' @param processName	The name of the process to extract data by, such as "ReadBioticXML"
#' @param functionName	The name of the function to extract data by, such as "ReadBioticXML"
#' @param tableName		Supported processdata tables: TRAWLASSIGNMENT, DISTANCEASSIGNMENT, PSUASSIGNMENT, DISTANCEPSU, PSUSTRATUM, STRATUMPOLYGON
#' @param storage		The StoX storage object
#' @param data			The StoX data
#' @param level			The level of tables for some datatypes like FishStation, which contains different levels, i.e., 1 for fishstation, 2 for sample and 3 for individuals. \code{getDataFrame1} selects the first level, and \code{getDataFrame} must be used to select a different level
#' @param drop			Logical: if TRUE (defalut) drop the list if only one data frame is requested.
#'
#' @return A dataframe
#'
#' @export
#' @keywords internal
#' @rdname getDataFrame
#' 
getDataFrame <- function(baseline, processName=NULL, functionName=NULL, level=NULL, drop=TRUE){
	if(is.null(baseline)){
		warning("Empty baseline object")
		return(NULL)
	}
	# Get the process by process name or function name:
	if(length(processName)){
		pr <- baseline$findProcess(processName)
		if(is.null(pr)){
			warning(paste0("Process \"", processName, "\" not found"))
			return(NULL)
		} 
	}
	else if(length(functionName)){
		pr <- baseline$findProcessByFunction(functionName)
		if(is.null(pr)){
			warning(paste0("Function \"", functionName, "\" not found"))
			return(NULL)
		} 
	}
	else{
		warning("One of 'processName' or 'functionName' must be given")
		return(NULL)
	}
	
	# Get the data storage object:
	storage <- pr$getDataStorage()
	if(is.null(storage)){
		warning(paste0("Data from ", if(length(processName)) paste0("process \"", processName, "\"") else paste0("function \"", functionName, "\""), " not found"))
		return(NULL)
	}
	
	# Get the stored data:
	data <- pr$getOutput()
	if(is.null(data)){
		warning(paste0("Output from ", if(length(processName)) paste0("process \"", processName, "\"") else paste0("function \"", functionName, "\""), " not found"))
		return(NULL)
	}
	
	# Get the requested levels:
	if(length(level)==0){
		level <- seq_len(storage$getNumDataStorageFiles())
	}
	else{
		level <- level[level<=storage$getNumDataStorageFiles()]
	}
	
	# Output a list of the data of each requested level:
	out <- lapply(level, getDataFrameAtLevel, storage=storage, data=data)
	outnames <- sapply(seq_along(out), function(xx) basename(storage$getStorageFileName(jInt(xx))))
	# Remove leading integers in sublists of the output from processes:
	outnames <- sub("[0-9]+_", "", outnames)
	names(out) <- outnames
	if(drop && length(out)==1){
		out[[1]]
	}
	else{
		out
	}
}
#' 
#' @export
#' @keywords internal
#' @rdname getDataFrame
#' 
getDataFrame1 <- function(baseline, processName=NULL, functionName=NULL, level=NULL, drop=TRUE){
	warning("getDataFrame1() is deprecated. Use getbaseline() instead (or the dependent funciton getDataFrame())")
	getDataFrame(baseline, processName=processName, functionName=functionName, level=level, drop=drop)
}
#' 
#' @export
#' @keywords internal
#' @rdname getDataFrame
#' 
getDataFrameAtLevel <- function(level, storage, data) {
	if(is.null(storage) || is.null(data)){
		return(NULL)
	} 
	s <- storage$asTable(data, jInt(level))
	#out <- read.csv(textConnection(s), sep='\t', stringsAsFactors=F)
	
	# A grave error was found when reading all data from 2016 (all serialno), where serial number were missing in e.g., FishStation versus CatchSample. This was tracked to the read.csv() of getDataFrameAtLevel(), where uncompleted quotes resulted in removed lines. The solution was to add the parameter quote=NULL:
	#out <- read.csv(textConnection(s), sep='\t', stringsAsFactors=FALSE, na.strings="-", encoding="UTF-8")
	### out <- read.csv(textConnection(s), sep='\t', stringsAsFactors=FALSE, na.strings="-", encoding="UTF-8", quote=NULL)
	### # Interpret true/false as TRUE/FALSE (move along the columns of 'out'):
	### for(i in seq_along(out)){
	### 	if(length(out[[i]])>0 && head(out[[i]], 1) %in% c("true", "false")){
	### 	 	out[[i]] <- as.logical(out[[i]])
	### 	}
	### }
	### out
	#apply(out, 2, function(xx) if(head(xx, 1) %in% c("true", "false")) as.logical(xx) else xx)
	
	# Added the funciton readBaselineFiles() for use in this function and as a separate utility:
	readBaselineFiles(textConnection(s))
}
#' 
#' @export
#' @keywords internal
#' @rdname getDataFrame
#' 
getProcessDataTableAsDataFrame <- function(projectName, tableName) {
	# Get the project object:
	project <- openProject(projectName, out="project")
	s <- project$getProcessData()$asTable(tableName)
	if(nchar(s)>0){
		out <- read.csv(textConnection(s), sep='\t', row.names=NULL, stringsAsFactors=FALSE, na.strings="-", encoding="UTF-8")
		# Interpret true/false as TRUE/FALSE:
		for(i in seq_along(out)){
			if(length(out[[i]])>0 && head(out[[i]], 1) %in% c("true", "false")){
			 	out[[i]] <- as.logical(out[[i]])
			}
		}
		return(out)
	}
	else{
		warning(paste0("Table \"", tableName, "\" missing in the project.xml file"))
		return(NULL)
	}
}


#*********************************************
#*********************************************
#' Set NASC data and assignments to memory
#' 
#' \code{setAssignments} Sets assignments to the assignment table in the process data (in memory).
#' \code{setMeanNASCValues} Sets NASC or meanNASC values to the baseline data (in memory).
#' 
#' @param projectName  	The name or full path of the project, a baseline object (as returned from getBaseline() or runBaseline()), og a project object (as returned from open).
#' @param assignments	The modified trawl assignments (StationWeight for each combination of AssignmentID and Station).
#' @param process		The process to set NASC values to.
#' @param data			The modified NASC data (Value for each combination of AcoCat, SampleUnit and Layer).
#'
#' @return An assignment
#'
#' @importFrom rJava J .jarray
#' @export
#' @rdname setAssignments
#'
setAssignments <- function(projectName, assignments){
	# Get the baseline object:
	baseline <- runBaseline(projectName=projectName, out="baseline", msg=FALSE)
	# Define the Java-object to modify:
	JavaPath <- baseline$getProject()$getProcessData()$getMatrix("bioticassignment")
	# The functions J and .jnew and other functions in the rJava package needs initialization:
	Rstox.init()
	# Modify with the 'assignments':
	J("no.imr.stox.functions.utils.AbndEstProcessDataUtil")$setAssignments(JavaPath, .jarray(as.character(assignments$AssignmentID)), .jarray(as.character(assignments$Station)), .jarray(as.double(assignments$StationWeight)))
}
#'
#' @importFrom rJava J .jarray
#' @export
#' @rdname setAssignments
#'
setNASC <- function(projectName, process="MeanNASC", data){
	# Get the baseline object:
	baseline <- runBaseline(projectName=projectName, out="baseline", msg=FALSE)
	# Define the Java-object to modify:
	JavaPath <- baseline$findProcessByFunction(process)$getOutput()$getData()
	# The functions J and .jnew and other functions in the rJava package needs initialization:
	Rstox.init()
	# Modify with the 'data':
	J("no.imr.stox.bo.MatrixUtil")$setGroupRowColValues(JavaPath, .jarray(as.character(data$AcoCat)), .jarray(as.character(data$SampleUnit)), .jarray(as.character(data$Layer)), .jarray(as.double(data$Value)))
}


#*********************************************
#*********************************************
#' Get or set the size of the Java memory
#' 
#' @param size	The size of the memory (in bytes) assigned to Java for each project. This value should not be as large as the total memory of the system. 
#'
#' @details
#' The function \code{setJavaMemory} sets the memory reserved for the Java virtual machine. Use \code{getJavaMemory()} to get the current value. The default is 2e9 (2 gigabytes), which should be sufficient in most cases. See \code{\link{runBootstrap}} for how to reduce the memory occupied by Java during parallel bootstrap operations.
#'
#' @export
#' @rdname setJavaMemory
#'
setJavaMemory <- function(size=2e9){
	sizeString <- paste0(round(size * 1e-6), "m")
	old <- options()$java.parameters
	options(java.parameters=paste0("-Xmx", sizeString))
	#cat("Java memory set to ", size, "\n", sep="")
}
#'
#' @export
#' @rdname setJavaMemory
#'
getJavaMemory <- function(){
	out <- options("java.parameters")
	out <- substring(out, 5)
	# Get the last character and interpret as mega or gigabytes:
	byteChar <- substring(out, nchar(out))
	out <- as.numeric(substr(out, 1, nchar(out) - 1))
	out <- out * if(byteChar=="g") 1e9 else 1e6
	out
}
