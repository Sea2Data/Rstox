##### Create and install the package stox in R: #####

##### Load devtools: #####
library("devtools")
##########

buildRstox <- function(buildDir, pkgName="Rstox", version="1.0", Rversion="3.3.1", pckversion=list(), official=FALSE, check=FALSE, exportDir=NULL) {
	
	########## Functions ##########
	# Function used for writing the README file automatically, including package dependencies, R and Rstox version and release notes:
	writeRstoxREADME <- function(READMEfile, NEWSfile, version, Rversion, betaAlpha, betaAlphaString, imports, official=FALSE){
		# Write Rstox and R version in the first two lines. THIS SHOULD NEVER BE CHANGED, SINCE STOX READS THESE TWO LINES TO CHECK VERSIONS:
		write(paste0("# Rstox version: ", version, " (latest ", betaAlphaString, ", ", format(Sys.time(), "%Y-%m-%d"), ")"), READMEfile)
		write(paste0("# R version: ", Rversion), READMEfile, append=TRUE)
		
		write("", READMEfile, append=TRUE)
		# Package description and installation code:
		write("# The package Rstox contains most of the functionality of the stock assesment utility StoX, which is an open source approach to acoustic and swept area survey calculations. Download Rstox from ftp://ftp.imr.no/StoX/Download/Rstox or install by running the following commands in R:", READMEfile, append=TRUE)
		
		write("", READMEfile, append=TRUE)
		write("# Install the packages that Rstox depends on. Note that this updates all the specified packages to the latest (binary) version:", READMEfile, append=TRUE)
		write(paste0("dep.pck <- c(\"", paste0(imports, collapse="\", \""), "\")"), READMEfile, append=TRUE)
		# WARNING: IT IS CRUSIAL TO ENCLUDE THE repos IN THIS CALL, FOR STOX TO SOURCE THE README FILE PROPERLY (RESULTS IN AN ERROR IF ABSENT) IT SEEMS "R CMD BATCH source(TheReadMeFile)" RETURNS AN ERROR WHEN repos IS NOT SET (2016-12-16):
		write("install.packages(dep.pck, repos=\"http://cran.us.r-project.org\", type=\"binary\")", READMEfile, append=TRUE)
		#write("install.packages(dep.pck, type=\"binary\")", READMEfile, append=TRUE)
		
		write("", READMEfile, append=TRUE)
		write("# Install Rstox:", READMEfile, append=TRUE)
		# Get the version string, the name of the Rstox tar file, the ftp root and, finally, the ftp directory and full path to the Rstox tar file:
		versionString <- paste0("Rstox_", version)
		tarName <- paste0(versionString, ".tar.gz")
		ftpRoot <- "ftp://ftp.imr.no/StoX/Download/Rstox"
		if(betaAlpha==3){
			ftpDir <- file.path(ftpRoot, "Versions", "Alpha", versionString)
		}
		else{
			if(official){
				ftpDir <- ftpRoot
			}
			else{
				ftpDir <- file.path(ftpRoot, "Versions", versionString)
			}
		}
		tarFile <- file.path(ftpDir, tarName)
		# Write the Rstox install command:
		write(paste0("install.packages(\"", tarFile, "\", repos=NULL)"), READMEfile, append=TRUE)
		
		write("", READMEfile, append=TRUE)
		write("# Alternatively, install the latest development version from GitHub.", READMEfile, append=TRUE)
		write(paste0("# Note that this does not guarantee a stable version."), READMEfile, append=TRUE)
		write(paste0("# For official versions of Rstox, refer to the ftp server ", ftpDir, " as described above."), READMEfile, append=TRUE)
		write("# Install from github using the devtools package:", READMEfile, append=TRUE)
		write("# devtools::install_github(\"Sea2Data/Rstox\", ref=\"develop\")", READMEfile, append=TRUE)
		write("", READMEfile, append=TRUE)
		write("# R should be installed as the 64 bit version (and 64 bit version ONLY for Windows 10).", READMEfile, append=TRUE)
		write("# On Windows systems with adminstrator requirements, it is recommended to install R in C:/users/<user>/documents/R.", READMEfile, append=TRUE)
		write("", READMEfile, append=TRUE)
		write("# Note that 64 bit Java is required to run Rstox", READMEfile, append=TRUE)
		write("# On Windows, install Java from this webpage: https://www.java.com/en/download/windows-64bit.jsp,", READMEfile, append=TRUE)
		write("# or follow the instructions found on ftp://ftp.imr.no/StoX/Tutorials/", READMEfile, append=TRUE)
		write("# On Mac, getting Java and Rstox to communicate can be challenging.", READMEfile, append=TRUE)
		write("# If you run into problems such as \"Unsupported major.minor version ...\", try the following:", READMEfile, append=TRUE)
		write("# Update java, on", READMEfile, append=TRUE)
		write("# \thttp://www.oracle.com/technetwork/java/javase/downloads/jre8-downloads-2133155.html", READMEfile, append=TRUE)
		write("# If this does not work install first the JDK and then the JRE:", READMEfile, append=TRUE)
		write("# \thttp://www.oracle.com/technetwork/java/javase/downloads/jdk8-downloads-2133151.html", READMEfile, append=TRUE)
		write("# \thttp://www.oracle.com/technetwork/java/javase/downloads/jre8-downloads-2133155.html", READMEfile, append=TRUE)
		write("# You may want to check that the downloaded version is first in the list by running the following in the Terminal:", READMEfile, append=TRUE)
		write("# \t/usr/libexec/java_home -V", READMEfile, append=TRUE)
		write("# \tjava -version", READMEfile, append=TRUE)
		write("# Then run this in the Terminal.app:", READMEfile, append=TRUE)
		write("# \tsudo ln -s $(/usr/libexec/java_home)/jre/lib/server/libjvm.dylib /usr/local/lib", READMEfile, append=TRUE)
		write("# \tsudo R CMD javareconf", READMEfile, append=TRUE)
		write("# Open R (close and then open if already open) and install rJava:", READMEfile, append=TRUE)
		write("# \tinstall.packages('rJava', type='source')", READMEfile, append=TRUE)
		write("# Then the installed Rstox should work.", READMEfile, append=TRUE)
		
		# Write release notes:
		write("", READMEfile, append=TRUE)
		write("", READMEfile, append=TRUE)
		write("# Release notes:", READMEfile, append=TRUE)
	
		# Read the changes:
		l <- readLines(NEWSfile)
		# Split into vesions:
		atversion <- which(substr(l, 1, 1) == "#")
		# Strip off "#", and extract the version string:
		versionStringInChanges <- l[atversion]
		startversion <- regexpr("Version ", versionStringInChanges)
		startversion <- startversion + attributes(startversion)$match.length
		versionStringInChanges <- substring(versionStringInChanges, startversion)
		endversion <- regexpr(" ", versionStringInChanges) - 1
		versionStringInChanges <- substr(versionStringInChanges, 1, endversion)
		# Split into versions and format to insert into the README file:
		l <- split(l, findInterval(seq_along(l), c(atversion, length(l)+1)))
		names(l) <- versionStringInChanges
		# Remove the version line:
		l <- lapply(l, function(xx) xx[substr(xx, 1, 1) != "#"])
		thisl <- l[[version]]
		hasText <- which(nchar(thisl)>1)
		thisl[hasText] <- paste0("# ", seq_along(hasText), ". ", thisl[hasText])
		write(thisl, READMEfile, append=TRUE)
	}
	
	# Function used for extracting the imports in Rstox, in order to inform about these in the README file. This will not be needed once the package is on CRAN:
	#getImports <- function(buildDir){
	#	# Read the NAMESPACE file and get the package dependencies:
	#	buildDirList <- list.files(buildDir, recursive=TRUE, full.names=TRUE)
	#	NAMESPACE <- readLines(buildDirList[basename(buildDirList) == "NAMESPACE"])
	#	atImports <- grep("import", NAMESPACE)
	#	imports <- NAMESPACE[atImports]
	#	imports <- sapply(strsplit(imports, "(", fixed=TRUE), "[", 2)
	#	imports <- sapply(strsplit(imports, ")", fixed=TRUE), "[", 1)
	#	imports <- unique(sapply(strsplit(imports, ",", fixed=TRUE), "[", 1))
	#
	#	importsPresent <- intersect(imports, installed.packages()[,"Package"])
	#	imports <- setdiff(imports, importsPresent[installed.packages()[importsPresent,"Priority"] %in% "base"])
	#	imports <- sort(imports)
	#
	#	return(imports)
	#}
	discardBasePackages <- function(x){
		inst <- installed.packages()
		Base <- inst[, "Package"][inst[,"Priority"] %in% c("base", "recommended")]
		sort(setdiff(x, Base))
	}
	
	
	getImports <- function(buildDir, version=list()){
		# Read the NAMESPACE file and get the package dependencies:
		buildDirList <- list.files(buildDir, recursive=TRUE, full.names=TRUE)
		imports <- NULL
		if(length(buildDirList)){
			print(buildDirList[basename(buildDirList) == "NAMESPACE"])
			NAMESPACE <- readLines(buildDirList[basename(buildDirList) == "NAMESPACE"])
			atImports <- grep("import", NAMESPACE)
			imports <- NAMESPACE[atImports]
			imports <- sapply(strsplit(imports, "(", fixed=TRUE), "[", 2)
			imports <- sapply(strsplit(imports, ")", fixed=TRUE), "[", 1)
			imports <- unique(sapply(strsplit(imports, ",", fixed=TRUE), "[", 1))
			#inst <- installed.packages()
			#Base <- inst[, "Package"][inst[,"Priority"] %in% c("base", "recommended")]
			#imports <- sort(setdiff(imports, Base))
			imports <- discardBasePackages(imports)
			#notBase <- inst[, "Package"][!(inst[,"Priority"]) %in% "base"]
			#imports <- sort(imports[!imports %in% notBase])
		}
		
		# Add required version for packages:
		if(length(version)){
			# Remove version information for packages that are not present in the imports:
			if(!all(names(version) %in% imports)){
				warning("Not all package versions specified in 'version' are present as imports.")
			}
			version <- version[names(version) %in% imports]
			atversion <- match(names(version), imports)
			for(i in seq_along(version)){
				imports[atversion[i]] <- paste0(imports[atversion[i]], " (>= ", version[i], ")")
			}
		}
		
		return(imports)
	}
	########## End of functions ##########
	
	# Clear the installed package:
	try(lapply(.libPaths(), function(xx) remove.packages(pkgName, xx)), silent=TRUE)
	
	if(length(exportDir)==0){
		exportDir <- file.path(dirname(buildDir), "builds")
	}
	if(length(grep(exportDir, buildDir))>0){
		stop("The 'exportDir' cannot be contained in the 'buildDir', since the exports are build from the 'buildDir'")
	}
	#exportDir <- file.path(buildDir, "bundle")
	manDir <- file.path(buildDir, "man")
	DESCRIPTIONfile <- file.path(buildDir, "DESCRIPTION")
	NAMESPACEfile <- file.path(buildDir, "NAMESPACE")
		unlink(NAMESPACEfile, recursive=TRUE, force=TRUE)
	onLoadFile = file.path(buildDir, "R", "onLoad.R")
	onAttachFile = file.path(buildDir, "R", "onAttach.R")
	thisExportDir <- file.path(exportDir, paste(pkgName, version, sep="_"))
	READMEfile <- file.path(buildDir, "README")
	READMEfileExport <- file.path(thisExportDir, "README")
	NEWSfile <- file.path(buildDir, "NEWS")
	
	
	##### Save the following content to the onLoad.R file in the "R" directory: #####
	# JAVA_HOME is unset to be able to load rJava.dll in R CMD BATCH
	# jPackage is moved to rstox.init for dynamic import of rJava
	# The local Rstox environment is created here, in which all useful outputs from functions are placed, and saved at the end of any code:
	onLoadText = paste(
		".onLoad <- function(libname, pkgname){",
		"	",
		"	if(Sys.getenv(\"JAVA_HOME\")!=\"\") Sys.setenv(JAVA_HOME=\"\")",
		"	options(java.parameters=\"-Xmx2g\")",
		"# Create a Rstox environment in which the baseline objects of the various projects are placed. This allows for a check for previously run baseline models and avoids memory leakage:", 
		"	assign(\"RstoxEnv\", new.env(), envir=.GlobalEnv)",
		"	# Assign fundamental variables to the RstoxEnv:",
		"	Definitions <- list(",
		"		StoXFolders = c(\"input\", \"output\", \"process\"), ",
		"		NMD_data_types = c(\"echosounder\", \"biotic\", \"landing\"), ",
		"		StoX_data_types = c(\"acoustic\", \"biotic\", \"landing\"), ",
		"		StoX_data_type_keys = c(acoustic=\"echosounder_dataset\", biotic=\"missions xmlns\", landing=\"Sluttseddel\"), ",
		"		model_types = c(\"AcousticTrawl\", \"SweptAreaLength\", \"SweptAreaTotal\"), ",
		"		processLevels = c(\"bootstrap\", \"bootstrapImpute\")",
		"		)",
		"	assign(\"Definitions\", Definitions, envir=get(\"RstoxEnv\"))",
		"	assign(\"Projects\", list(), envir=get(\"RstoxEnv\"))",
	"}", sep="\n")
	write(onLoadText, onLoadFile)
	##########
	
	##### Save a Java memory message to the onAttach.R file in the "R" directory: #####
	onAttachText = paste(
		".onAttach <- function(libname, pkgname){",
		"	",
		# paste0("	packageStartupMessage(\"", pkgName, "_", version, "\n**********\nIf problems with Java Memory such as java.lang.OutOfMemoryError occurs, try increasing the Java memory by running options(java.parameters=\\\"-Xmx4g\\\"), and possibly using an even higher value than 4g\n**********\n\", appendLF=FALSE)"),
		paste0("	packageStartupMessage(\"", pkgName, "_", version, "\n**********\nIf problems with Java Memory such as java.lang.OutOfMemoryError occurs, try increasing the Java memory by running setJavaMemory(4e9), and possibly using an even higher value than 4 gigabytes\n**********\n\", appendLF=FALSE)"),
	"}", sep="\n")
	write(onAttachText, onAttachFile)
	##########
	
	##### Add required fields to the DESCRIPTION file (below is the full content of the DESCRIPTION file): #####
	# Depends is replaced by @import specified by functions"
	DESCRIPTIONtext = paste(
		paste0("Package: ", pkgName),
		"Title: Running Stox functionality independently in R",
		paste0("Version: ", version),
		"Authors@R: c(",
		"  person(\"Arne Johannes\", \"Holmin\", role = c(\"aut\",\"cre\"), email = \"arnejh@imr.no\"),",
		"  person(\"Edvin\", \"Fuglebakk\", role = \"ctb\"),",
		"  person(\"Gjert Endre\", \"Dingsoer\", role = \"ctb\"),",
		"  person(\"Aasmund\", \"Skaalevik\", role = \"ctb\"),",
		"  person(\"Espen\", \"Johnsen\", role = \"ctb\"))",
		"Author: Arne Johannes Holmin [aut, cre],",
		"  Edvin Fuglebakk [ctr],",
		"  Gjert Endre Dingsoer [ctr],",
		"  Aasmund Skaalevik [ctr],",
		"  Espen Johnsen [ctr]",
		"Maintainer: Arne Johannes Holmin <arnejh@imr.no>",
		paste0("Depends: R (>= ", Rversion, ")"), 
		"Description: This package contains most of the functionality of the StoX software, which is used for assessment of fish and other marine resources based on biotic and acoustic survey and landings data, among other uses. Rstox is intended for further analyses of such data, facilitating iterations over an arbitrary number of parameter values and data sets.",
		"BugReports: https://github.com/Sea2Data/Rstox/issues", 
		"License: LGPL-3",
		"LazyData: true", sep="\n")
	write(DESCRIPTIONtext, DESCRIPTIONfile)
	##########
	
	##### Create documentation: #####
	# Remove current documentation first:
	unlink(manDir, recursive=TRUE, force=TRUE)
	document(buildDir)
	
	# Alter the DESCRIPTION file to contain the imports listed in the NAMESPACE file:
	imports <- getImports(buildDir, version=pckversion)
	#DESCRIPTIONtext <- readLines(DESCRIPTIONfile)
	if(length(imports)){
		cat("Imports:\n		", file=DESCRIPTIONfile, append=TRUE)
		cat(paste(imports, collapse=",\n		"), file=DESCRIPTIONfile, append=TRUE)
		cat("", file=DESCRIPTIONfile, append=TRUE)
	}
	##########
	
	##### Run R cmd check with devtools: #####
	if(check){
		devtools::check(buildDir)
	}
	##########
	
	### Generate the README file: ###
	betaAlpha <- length(gregexpr(".", version, fixed=TRUE)[[1]]) + 1
	betaAlphaString <- c("", "beta", "alpha")[betaAlpha]
	# Read the NAMESPACE file and get the package dependencies. This is needed since we are not on CRAN:
	writeRstoxREADME(READMEfile, NEWSfile, version, Rversion, betaAlpha, betaAlphaString, imports=getImports(buildDir), official=official)
	file.copy(READMEfile, READMEfileExport, overwrite=TRUE)
	##########
	
	##### Create platform independent bundle of source package: #####
	dir.create(thisExportDir, recursive=TRUE)
	pkgFileVer <- build(buildDir, path=thisExportDir)
	
	##### Unload the package: #####
	unload(buildDir)
	##########
	
	##### Install local source package by utils (independent of dev-tools), and check that it loads: #####
	install.packages(pkgFileVer, repos=NULL, type="source", lib=.libPaths()[1])
	library(Rstox)
	##########
}



# Define the directory of the working copy:
#arnejh
dir <- "~/Documents/Produktivt/Prosjekt/R-packages/Rstox/Rstox"
# aasmunds
#dir <- "C:/Projects/Sea2Data/NewBeam/trunk/beam/StoX/StoX-Main/src/main/resources/stox/system/r"


# Build 1.6.2:
# buildRstox(dir, version="1.6.2", Rversion="3.3.3", official=FALSE, check=FALSE)


# # Build 1.6.3:
# buildRstox(dir, version="1.6.3", Rversion="3.3.3", official=FALSE, check=FALSE, pckversion=list(data.table="1.10.4-3"))


# Build 1.6.4:
# buildRstox(dir, version="1.6.4", Rversion="3.4.2", pckversion=list(data.table="1.10.4-3"), official=FALSE, check=FALSE)


# Build 1.6.5:
#buildRstox(dir, version="1.6.5", Rversion="3.4.2", pckversion=list(data.table="1.10.4-3"), official=FALSE, check=FALSE)


# Build 1.7:
#buildRstox(dir, version="1.7", Rversion="3.4.2", pckversion=list(data.table="1.10.4-3"), official=TRUE, check=FALSE)

# Build 1.7.1:
#buildRstox(dir, version="1.7.1", Rversion="3.4.2", pckversion=list(data.table="1.10.4-3"), official=FALSE, check=FALSE)

# Build 1.7.1:
buildRstox(dir, version="1.7.2", Rversion="3.4.2", pckversion=list(data.table="1.10.4-3"), official=FALSE, check=FALSE)

# Build 1.8:
#buildRstox(dir, version="1.7.1", Rversion="3.4.2", pckversion=list(data.table="1.10.4-3"), official=FALSE, check=FALSE)
