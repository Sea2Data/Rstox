#*********************************************
#*********************************************
#' Get NMD API data and reference information
#'
#' \code{getNMDinfo} converts, prints and optionally returns NMD reference information given a search string to the reference information. Used in StoX.URL(). \cr \cr
#' \code{getNMDdata} downloads data from specific cruises, cruise series ot survey time series from NMD. \cr \cr
#' \code{downloadXML} downloads xml data from an API, parses the xml data, and converts to a list (the latter is time consuming). \cr \cr
#'
#' @param type					A character string naming the type of information to return for the specifications given in 'spec'. Possible values are shown in the list below. Any reference data can be requested, and the names of the possible reference data are returned by running getNMDinfo():
#' \describe{
#'	\item{NULL}{List of available reference data}
#'	\item{"c"}{List of cruises (Will be implemented in version 2)}
#'	\item{"cs"}{List of cruise series. If given as a vector of length 2, where the first element is "cs" and the second is the exact case sensitive name of a cruise series, information only about that cruise sereis is returned.}
#'	\item{"sts"}{List of survey time series. Can be given as a two element vector as for "cs".}
#'	\item{"v"}{List of vessels, where the first line of the platform information is extracted and presented in a matrix with vessels in the rows. Use "platform" to preserve all details about the platforms/vessels}
#'	}
#' @param ver					The version of the APIs and data, given as a list such as that returned by getRstoxDef("ver"). To use version 1 of the API (the only one available prior to Rstox 1.10) use getRstoxDef("ver", API=list(biotic=1, reference=1)), or simply ver = 1. Giving \code{ver} as a single integer imposes this value on all API versions. To set API versions to 1 and at the same time set the version of the data, use e.g. ver = list(API=1, biotic=1.4).
#' @param API					The path to the API.
#' @param recursive				Logical, special for type \%in\% c("cs","sts"); if FALSE only the list of cruise series or survey time series is returned.
#' @param msg					Logical: if TRUE a message is printed to the consolle stating the estimated time left for the funciton.
#' @param simplify				Logical: if TRUE simplify the data into matrices instead of complicated lists in some cases like taxa.
#' @param cruise				Either the code of a cruise, such as "2015116", or the full or short name of a cruise series or survey time series. In the current version, if given as a cruise code, the parameter 'shipname' must be given as well, based on which the path to the cruise is searched for by functionallity provided by NMD. For cruises prior to the year 1995 several vessels can be linked to the same cruise code, and as of version 2 the user will by default be asked to specify which vessel(s) to specify the vessels when this occurs, instead of having to specify the cruise initially.
#' @param year					Used in conjunction with 'shipname' to get all cruises from one or more years from a specific ship.
#' @param shipname				Specifies the ship name WITHOUT call signal, e.g., "G.O.Sars" and not "G.O.Sars_LMEL" (see 'cruise' and 'year').
#' @param serialno				A vector of the requested serial numbers.
#' @param tsn					The species code for downloading a specific species. See the examples for how to get the \code{tsn} of a species.
#' @param dataSource			The type of data requested. Currently implemented are "echosunder" and "biotic", while "landing" and "ctd" are in the pipeline. dataSource=NULL (default) returns all possible data.
#' @param dir					The path to the directory in which to place the StoX project holding the downloaded data, or TRUE indicating that a sub directory should be created in which to put mulpitle with the name of the in which to put the downloaded projects
#' @param subdir				Either a name of the sub directory in which to put the StoX projects of downloaded data, or TRUE which puts all projects in a sub folder named after the cruise series or survey time series. 
#' @param group					Specifies how to gruop the data: (1) If given as "year", the data are split into years, and one StoX project is saved for each year; (2) if given as "cruise", one Stox project is generated for each cruise, and (3) group is "all", NULL or NA, all data are saved in one StoX project. Abbreviations are accepted (e.g., group="c"). The default "default" groups by years if several cruises are requested and by cruise otherwise. The projects generated from the downloaded data are named differently depending on the grouping, with, e.g., the suffixes "_Year_2004"/"_CruiseNumber_2004204_ShipName_Johan Hjort"/"_Alldata" for group = "year"/"cruise"/"all". This implies that the projects downloaded using group = "year" are not replaced when using group = "cruise", even if there is only one cruise per year.
#' @param abbrev				Logical: If TRUE, abbreviate the project names. Particularly useful when downloading survey time series, which can have long names.
#' @param subset				An vector giving the subset of the cruises/projects to download in a cruise series/survey time series. Can be given either as indices such as 1:3 or -4 or as a vector of years or cruise numbers such as c(2005, 2007, 2008) or c(2005105, 2007845, 2008809). Get the years and cruise numbers using getNMDinfo("cs").
#' @param prefix				The prefix to use in the names of the StoX projects to which the data are downloaded.
#' @param cleanup				Logical: if FALSE, zip files containing cruise series or survey time series are not deleted.
#' @param model					The model to use, either given as a string specifying a template, or a vector of process names or list of processes given as lists of parameter specifications (not yet implemented). Show available templates with createProject().
#' @param ow 					Specifies whether to ovewrite existing project: If TRUE, overwrite; if FALSE, do not overwrite; if NULL (default), aks the user to confitm overwriting.
#' @param return.URL			Logical: If TRUE, return the URL of the files to download.
#' @param run					Logical: If TRUE, download data and generate projects. If FALSE, only generate project names (useful for retrieveing the project names without downloading).
#' @param ...		 			Same as parlist, but can be specified separately (not in a list but as separate inputs).
#' @param URL					The URL(s) to the xml data, or the path to a local XML file.
#' @param list.out				Logical: If TRUE, convert the XML data to a list (time consuming).
#' @param file					The path to a the file to which the data are saved.
#' @param method				The download method. See \code{\link{download.file}}.
#' @param timeout				If given, the timeout of the reponse to download.file() is set. Only used on Windows and if used, method is forced to "internal". Note that setting \code{timeout} in options() will not have the desired effect, since it requires method = "internal" to be set in \code{\link{download.file}} (which is used by \code{getNMDdata}).
#' @param zipSTS				Logical: If TRUE, download the zipped survey time series projects.
#'
#' @details
#' If non-standard characters are not shown as expected, it might be an issue of locale encoding. 
#' It may help to run Sys.setlocale(category = "LC_ALL", locale = ""), at least to display nordic characters on a nordic system.
#'
#' @keywords NMD-API
#'
#' @examples
#' # A list of available reference data:
#' g1 <- getNMDinfo()
#' # List of cruise series:
#' g2 <- getNMDinfo("cs")
#' # List of survey time series:
#' g3 <- getNMDinfo("sts")
#' # List of vessels (the first vessel per platform) and the more complicated list of platforms,
#' # (where there can be several vessels per platform). These requestes may take tens of seconds:
#' g5 <- getNMDinfo("platform")
#' # Get other types of information:
#' g6 <- getNMDinfo("gearcondition")
#' g7 <- getNMDinfo("missiontype")
#' g8 <- getNMDinfo("person")
#' g9 <- getNMDinfo("taxa")
#' # Get the tsn code of torsk:
#' g9[g9$Norwegian=="torsk",]
#' # And all names containing "torsk":
#' g9[grep("torsk", g9$Norwegian, ignore.case=TRUE),]
#' 
#' # Download all reference data (takes at lest 5 minutes):
#' # system.time(l <- lapply(g1, getNMDinfo, msg=TRUE))
#'
#' # For examples of downloading data from Norwegian Marine Data Centre (NMD in norwegian), 
#' # go to ftp://ftp.imr.no/StoX/Download/Rstox/Examples/Rstox-example_1.8.R.
#' 
#' @importFrom XML getNodeSet xmlValue xmlParse xmlNamespaceDefinitions
#' @export
#' @rdname getNMDinfo
#' 
getNMDinfo <- function(type=NULL, ver=getRstoxDef("ver"), API="http://tomcat7.imr.no:8080/apis/nmdapi", recursive=TRUE, msg=FALSE, simplify=TRUE, return.URL=FALSE){
	
	# Interpret the version info:
	ver <- getNMDver(ver)
	
	# Get the list of reference data types:
	if(length(type)==0){
		data <- getReferenceList(ver=ver, API=API, msg=FALSE)
		return(data)
	}
	
	# Treat the requested type of information:
	type[1] <- tolower(type[1])
	if(length(intersect(type, c("v", "vessel")))){
		type <- "platform"
		simplify <- TRUE
	}
	
	# Get the list of cruises:
	if(type[1] %in% c("c", "cruise", "cruises")){
		data <- getCruiseInfo(ver=ver, API=API, msg=msg)
	}
	# Get the list of cruises series with cruises for each series:
	else if(type[1] %in% c("cs", "cruiseseries")){
		# Set the full name of the type:
		type[1] <- "cruiseseries"
		data <- getSeriesInfo(ver=ver, API=API, type=type, msg=msg, recursive=recursive)
	}
	# Get the list of survey time series with StoX projets for each series:
	else if(type[1] %in% c("sts", "surveytimeseries")){
		# Set the full name of the type:
		type[1] <- "surveytimeseries"
		data <- getSeriesInfo(ver=ver, API=API, type=type, msg=msg, recursive=recursive)
	}
	else{
		# Get the available reference data types:
		ref <- getReferenceList(ver=ver, API=API, msg=FALSE)
		
		# Match the 'type' with the reference data available:
		type <- ref[tolower(ref) == type[1]]
		
		# Download the reference data:
		if(ver$API$reference == 1){
			tempver <- ver
			tempver$reference <- NULL
			URLbase <- getURLbase(ver=tempver, API=API, dataSource="reference", unnamed=type[1])
		}
		else if(ver$API$reference == 2){
			URLbase <- getURLbase(ver=ver, API=API, dataSource="reference", dataset=type[1])
		}
		
		# Return the URL:
		if(return.URL){
			return(URLbase)
		}
		
		data <- downloadXML(URLbase, msg=msg)
		# Simplify the data:
		if(simplify){
			if(length(data)){
				# If the reference data contains a list of data, it will in API version 2 be either 'platform' or 'taxa', which requires more elaborated extraction. If not using getElements() will suffice, since each "row" of the data represents one row in the matrix:
				if(isPlatform(data)){
					data <- getPlatform(data, ver=ver)
				}
				else if(isTaxa(data)){
					data <- getTaxa(data, ver=ver)
				}
				else{
					data <- getReference(data, ver=ver)
				}
			}
		}
	}
	
	return(data)
}

#'
#' @export
#' @rdname getNMDinfo
#' 
getNMDdata <- function(cruise=NULL, year=NULL, shipname=NULL, serialno=NULL, tsn=NULL, dataSource=NULL, dir=NULL, subdir=FALSE, group="default", abbrev=FALSE, subset=NULL, prefix="NMD", ver=getRstoxDef("ver"), API="http://tomcat7.imr.no:8080/apis/nmdapi", cleanup=TRUE, model="StationLengthDistTemplate", msg=TRUE, ow=NULL, return.URL=FALSE, run=TRUE, timeout=NULL, zipSTS=TRUE, ...){
	
	# Support for giving 'prefix' as 'filebase' for backwards compatibility:
	l <- list(...)
	if("filebase" %in% names(l)){
		prefix <- l$filebase
	}
	
	#######################################
	############ Preparations: ############
	#######################################
	# Interpret the version info:
	ver <- getNMDver(ver)
	
	# Define the valid types:
	NMD_data_sources <- getRstoxDef("NMD_data_sources")
	NMD_API_versions <- getRstoxDef("NMD_API_versions")[NMD_data_sources]
	if(length(dataSource)==0){
		dataSource <- names(NMD_API_versions[sapply(NMD_API_versions, length) > 0])
	}
	# Set the data types used in StoX (differing from NMD in the echosounder/acoustic type):
	StoX_data_sources <- getRstoxDef("StoX_data_sources")
	StoX_data_sources <- StoX_data_sources[NMD_data_sources %in% dataSource]
	
	# Get the project root:
	dir <- getProjectPaths(projectName="", projectRoot=dir)$projectRoot
	#######################################
	
	# Get serialno:
	serialno <- getSerialno(serialno, year)
	
	# Get download type:
	if(isSTS(cruise, ver=ver)){
		if(zipSTS){
			downloadType <- "stszip"
		}
		else{
			downloadType <- "sts"
		}
	}
	else if(isCS(cruise, ver=ver)){
		downloadType <- "cs"
	}
	else if(length(serialno)){
		downloadType <- "serialno"
	}
	else{
		downloadType <- "c"
	}
	#######################################
	
		
	########################################
	########## (1) Serial number: ##########
	########################################
	# Download serialno data:
	#if(length(serialno) && run){
	if(downloadType == "serialno"){
		out <- downloadSerialno(serialno=serialno, downloadType=downloadType, year=year, tsn=tsn, prefix=prefix, dir=dir, model=model, ow=ow, ver=ver, API=API, run=run, return.URL=return.URL, msg=msg, timeout=timeout, ...)
		return(out)
	}
	########################################
	
	
	#############################################
	########## (2) Survey time series: ##########
	#############################################
	#else if(isSTS(cruise, ver=ver) && run){
	else if(downloadType == "stszip"){
		# Get the info of only the requested survey time series to save time, requiring the [[1]]:
		stsInfo <- getNMDinfo(c("sts", cruise))[[1]]
		out <- getSurveyTimeSeriesZip(stsInfo=stsInfo, dir=dir, subdir=subdir, subset=subset, cleanup=cleanup, ow=ow, abbrev=abbrev, run=run, ver=ver, msg=msg, return.URL=return.URL)
		return(out)
	}
	#############################################
	
	
	####################################################
	########## (3) Cruises and cruise series: ##########
	####################################################
	#else if(run){
	else{
		if(downloadType == "cs"){
			# Get the matrix of stoxProjectId and sampleTime (i.e., year), and the name of the survey time series (sts):
			cruiseInfo <- getNMDinfo(c("cs", cruise))[[1]]
			# Add both StoX and NMD dataSource:
			cruiseInfo <- addDataSources(cruiseInfo, dataSource=dataSource)
		}
		else if(downloadType == "sts"){
			stsInfo <- getNMDinfo(c("sts", cruise))[[1]]
			cruiseInfo <- getCruiseInfoFromStsInfo(stsInfo)
		}
		else if(downloadType == "c"){
			# Define the info needed to get the URLs:
			cruiseInfo <- data.frame(
				Year = getYearFromCruiseNumber(cruise), 
				# Bug fix on 2018-09-28 after comment from Ibrahim. With Cruise named CruiseNr, getPaths() did not find the cruise number:
				# CruiseNr = cruise, 
				Cruise = cruise, 
				ShipName = shipname
			)
			# Add both StoX and NMD dataSource:
			cruiseInfo <- addDataSources(cruiseInfo, dataSource=dataSource)
		}
		else{
			stop("Unknown downloadType")
		}
		# Get the URLs as columns named by the data sources given :
		#cruiseInfo <- getCruiseURLs(cruiseInfo, dataSource=dataSource, StoX_data_sources=StoX_data_sources, ver=ver, API=API)
		# Report warnings for missing URLs (searchNMDCruise() not finding the file) but only for "sts":
		cruiseInfo <- getCruiseURLs(cruiseInfo, ver=ver, API=API, checkURL=downloadType == "sts")
		
		# Apply the default grouping:
		cruiseInfo <- getGroup(cruiseInfo, group)
		
		# Add the project ID:
		cruiseInfo <- getProjectID(cruiseInfo)
		
		# Download the cruises:
		out <- getCruises(cruiseInfo, downloadType=downloadType, cruise=cruise, StoX_data_sources=StoX_data_sources, model=model, dir=dir, subdir=subdir, subset=subset, prefix=prefix, dataSource=dataSource, ow=ow, abbrev=abbrev, timeout=timeout, return.URL=return.URL, ...)
		
		# Return the project paths:
		return(out)
	}
	####################################################
}

#'
#' @export
#' @rdname getNMDinfo
#' 
getNMDver <- function(ver=NULL, name=NULL){
	
	replaceAllAPIs <- function(out, ver){
		out$API <- rapply(out$API, function(x) ver, how="replace")
		out
	}
	
	out <- getRstoxDef("ver")
	
	if(length(ver)==1 && is.integer(ver)){
		out <- rapply(out, function(x) ver, how="replace")
	}
	else if(length(ver)==1 && is.numeric(ver)){
		out <- replaceAllAPIs(out=out, ver=ver)
		#out <- rapply(out, function(x) ver, how="replace")
	}
	else if(is.list(ver)){
		# If ver is a list with one element API, use this value for all APIs:
		if("API" %in% names(ver) && !is.list(ver$API) && length(ver$API)==1){
			out <- replaceAllAPIs(out=out, ver=ver$API)
			#out$API <- rapply(out$API, function(x) ver$API, how="replace")
			# Remove the API element of 'ver':
			ver <- ver[names(ver) != "API"]
		}
		
		# Insert any data versions:
		# Get the list indices by unlisting and splitting by the dots introduced by unlist():
		u <- unlist(ver)
		if(length(u)){
			ind <- strsplit(names(u), ".", fixed=TRUE)
			# Remove the names on u:
			u <- unname(u)
			
			# Insert each version:
			for(i in seq_along(ind)){
				out[[ind[[i]]]] <- u[i]
			}
		}
	}
	
	# Make sure that the data versions are decimal numbers and API versions are integers:
	out$API <- lapply(out$API, as.integer)
	out$API <- lapply(out$API, as.character)
	notAPI <- names(out) != "API"
	# Suppress warning when 
	out[notAPI] <- lapply(out[notAPI], as.numeric)
	out[notAPI] <- lapply(out[notAPI], format, nsmall=1)
	
	# Return all or some of the data:
	if(length(name)){
		out[[as.character(name)]]
	}
	else{
		out
	}
}


###############################################
##### Internal functions of getNMDinfo>>> #####
###############################################

# Detect platform or taxa reference data:
isPlatform <- function(data){
	any(tolower(names(data[[length(data)]])) == "platformcodes")
}
isTaxa <- function(data){
	any(tolower(names(data[[length(data)]])) == "taxasynonyms")
}

# Function for extracting the list of reference names (support for both version 1 and 2 of reference):
getReferenceList <- function(ver, API, msg=FALSE){
	# V1: http://tomcat7.imr.no:8080/apis/nmdapi/reference/v1
	# V1: http://tomcat7.imr.no:8080/apis/nmdapi/reference/v1?version=1.0
	# V2: http://tomcat7.imr.no:8080/apis/nmdapi/reference/v2?version=2.0
	dataSource <- "reference"
	URLbase <- getURLbase(ver=ver, API=API, dataSource=dataSource)
	# Get the list of cruise series:
	data <- downloadXML(URLbase, msg=msg)

	# Get the list of reference names, from the simpler output in version 1, or the more elaborated output in version 2 of the 'reference':
	if(ver$API[[dataSource]]==1){
		data <- getElementsValue(data, element="element", value="text")
	}
	else{
		data <- getElements(data, levels=list("row", c("name")), data.frame.out=FALSE)
	}

	data
}

# Function used for applying either getCruiseSeriesCruises() or getSurveyTimeSeriesProjects() for all years:
getSeriesInfo <- function(ver, API, type, msg=FALSE, recursive=TRUE){

	# In version 1 of the NMD API the cruiseseries and surveytimeseries were stored as separate datasources named "cruiseseries" and "surveytimeseries". 
	# Thus one call was first made to the top level of this datasource, retrieving the list of series, and then (if recursive=TRUE or a specific series was requested) a loop was made to get info of the requested series.

	# In version 2 of the MND API cruiseseries and surveytimeseries have been moved to the datasource "reference", and a distinction has been made between the list of series names (model) and the full info of all series (dataset). 
	# As a consequence, to get full info of only one series, the full info of all series must be downloaded.

	# Examples of URLs in version 1 and 2:
	# Cruiseseries:
	# V1: http://tomcat7.imr.no:8080/apis/nmdapi/cruiseseries/v1
	# V2: http://tomcat7.imr.no:8080/apis/nmdapi/reference/v2/model/cruiseseries?version=2.0
	# V2: http://tomcat7.imr.no:8080/apis/nmdapi/reference/v2/dataset/cruiseseries?version=2.0
	# Surveytimeseries:
	# V1: http://tomcat7.imr.no:8080/apis/nmdapi/surveytimeseries/v1
	# V2: http://tomcat7.imr.no:8080/apis/nmdapi/reference/v2/model/surveytimeseries?version=2.0
	# V2: http://tomcat7.imr.no:8080/apis/nmdapi/reference/v2/dataset/surveytimeseries?version=2.0

	# If the user requests info from cruise or surveytimeseries in version 1 of the API, this must be specified in ver$API$reference, even though these data are not in reference in version 1. 
	if(ver$API$reference == 1){

		# Function used for extracting a data frame of the element of a cruise series:
		downloadAndGetOneCruiseSeriesCruises_V1 <- function(x, URLbase, msg=FALSE){
			this <- downloadXML(paste(URLbase, x, sep="/"), msg=msg)$Sample
			# Get years and repeat by the number of cruises for each year
			years <- unname(sapply(this, "[[", ".attrs"))
			nCruisesPerYear <- sapply(this, function(xx) length(xx$Cruises))
			years <- rep(years, nCruisesPerYear)
			CruiseShipName <- as.matrix_full(unlist(lapply(this, "[[", "Cruises"), use.names=FALSE, recursive=FALSE))
			# 2018-09-07: Changed 'year' to 'Year' here, to be consistent over the column names:
			out <- as.data.frame(cbind(Year=years, Cruise=CruiseShipName[,1], ShipName=CruiseShipName[,2]), stringsAsFactors=FALSE)
			out <- as.numericDataFrame(out)
			out
		}

		# Function used for extracting a data frame of the StoX projects used in a survey time series:
		downloadAndGetOneSurveyTimeSeriesProjects_V1 <- function(x, URLbase, msg=FALSE){
			this <- downloadXML(paste(URLbase, x, sep="/"), msg=msg)$Sample
			out <- as.data.frame(as.matrix_full(this), stringsAsFactors=FALSE)
			out <- as.numericDataFrame(out)
			out
		}

		# Get the function to use for extracting the series info (one of getCruiseSeriesCruises() and getSurveyTimeSeriesProjects()):
		fun <- switch(type[1], 
			cruiseseries = downloadAndGetOneCruiseSeriesCruises_V1, 
			surveytimeseries = downloadAndGetOneSurveyTimeSeriesProjects_V1
		)

		# Get the series names:
		URLbase <- paste(API, type[1], "v1", sep="/")
		data <- downloadXML(URLbase, msg=msg)
		data <- unlist(data[names(data)=="element"], use.names=FALSE)

		# Get the list of cruise/survey time series. The name can be given exactly as the second element of 'type', in which case the list of series is reduced, but still a list (used in getNMDdata()):
		requestedSeries <- NULL
		if(length(type)==2){
			if(is.numeric(type[2])){
				requestedSeries <- data[type[2]]
			}
			else{
				requestedSeries <- type[2]
			}
			recursive <- TRUE
		}
		# Otherwise get info of all series:
		else{
			requestedSeries <- data
		}

		# Download the 'dataset' (the contents of the series):
		if(recursive){
			warning("Requesting survey time series with recursive=TRUE is no longer maintained in version 1 of the MND API.")
			data <- lapply(requestedSeries, fun, URLbase=URLbase, msg=msg)
			names(data) <- requestedSeries
		}
	}
	#else if(ver$API$reference==2){
	else if(ver$API$reference >= 2){
		
		 nestedList2data.frame <- function(x, except=".attrs"){
			# Unlist, obtaining a vector with names recursively separated by dots:
			u <- unlist(x)
			
			# Remove except:
			u <- u[!grepl(except, names(u))]
			
			# Create a list with unique names:
			s <- split(u, names(u))
			
			# Convert to a data frame and return:
			out <- suppressWarnings(as.data.frame(s, stringsAsFactors=FALSE))
			names(out) <- sapply(strsplit(names(out), ".", fixed=TRUE), utils::tail, 1)
			out
		}
		
		# Function used to extracting the year, cruise code and ship name of a cruise series:
		getCruiseSeriesCruises_V2 <- function(x){
			# Get a data frame of each CS:
			out <- lapply(x$samples, nestedList2data.frame)
			# Merge the CSs:
			out <- do.call(rbind, out)
			row.names(out) <- NULL
			# Rename to comply with naming used in getNMDinfo():
			old <- c("sampleTime", "cruisenr", "shipName")
			new <- c("Year", "Cruise", "ShipName")
			names(out) <- replace(names(out), match(old, names(out)), new)
			
			# Order by year and then code (order inside year):
			o <- order(out$Year, out$code)
			out[o,]
		}
		#getCruiseSeriesCruises_V2 <- function(x){
		#	Year <- NAsapply(x$samples, function(y) y$sampleTime)
		#	Cruise <- NAsapply(x$samples, function(y) y$cruises$cruise$cruisenr)
		#	ShipName <- NAsapply(x$samples, function(y) y$cruises$cruise$shipName)
		#	data.frame(Year=Year, Cruise=Cruise, ShipName=ShipName, stringsAsFactors=FALSE)
		#}
		# Function used for extracting a data frame of the StoX projects used in a survey time series:
		getSurveyTimeSeriesProjects_V2 <- function(x, URLbase){
			sampleTime <- NAsapply(x$cruiseSeries$cruiseSeries$samples, function(y) y$sampleTime)
			stoxProjectId <- NAsapply(x$cruiseSeries$cruiseSeries$samples, function(y) y$stoxProject)
			STSCode <- x$code
			STSName <- x$name
			CSCode <- x$cruiseSeries$cruiseSeries$cruiseSeriesCode
			CSName <- x$cruiseSeries$cruiseSeries$cruiseSeries
			data.frame(sampleTime=sampleTime, stoxProjectId=stoxProjectId, STSCode=STSCode, STSName=STSName, CSCode=CSCode, CSName=CSName, stringsAsFactors=FALSE)
		}

		# Get the function to use for extracting the series info (one of getCruiseSeriesCruises() and getSurveyTimeSeriesProjects()):
		fun <- switch(type[1], 
			cruiseseries = getCruiseSeriesCruises_V2, 
			surveytimeseries = getSurveyTimeSeriesProjects_V2
		)

		# Get the list of cruise/survey time series. The name can be given exactly as the second element of 'type', in which case the list of series is reduced, but still a list (used in getNMDdata()):
		requestedSeries <- NULL
		if(length(type)==2){
			requestedSeries <- type[2]
			recursive <- TRUE
		}
		# Download the 'dataset' (the contents of the series):
		if(recursive){
			# Download the seies dataset:
			URLbase <- getURLbase(ver=ver, API=API, dataSource="reference", dataset=type[1])
			data <- downloadXML(URLbase, msg=msg)
			# Extract the names of the series:
			seriesNames <- sapply(data, "[[", "name")
			# Extract the series info:
			data <- lapply(data, fun)
			# Add the series names as names to the list, and also as attributes to each list element, so that this information is kept when extracting only one list element:
			names(data) <- seriesNames
			for(i in seq_along(data)){
				attr(data[[i]], "seriesName") <- seriesNames[[i]]
			}
			# Subset by the requested series:
			if(length(requestedSeries)){
				data <- data[requestedSeries]
			}
		}
		# Download the 'model' (the names of the series):
		else{
			# Download the seies model (only the names of the series):
			URLbase <- getURLbase(ver=ver, API=API, dataSource="reference", model=type[1])
			data <- downloadXML(URLbase, msg=msg)
			# Extract the names:
			data <- getElements(data, levels=list("row", c("name")), data.frame.out=FALSE)
		}
	}
	if(ver$API$reference > 2){
		warning("API version > 2 for reference has not been fully tested")
	}
	
	#else{
	#	stop("Invalid NMD API version. See getRstoxDef(\"NMD_API_versions\") for implemented APIs for the different data sources.")
	#}

	return(data)
}

# Function for checking for identical dates in valid from and to, and subtracting one day from the valid to with a warning:
corectEqualFromTo <- function(x, platformNumber, tz=""){
	# Check whether there are equal validFrom and validTo:
	equal <- which(x$validTo %in% x$validFrom)
	if(length(equal)){
		# Subtract one day from validTo:
		warning(paste0("Platform number ", platformNumber, " contained equal 'validFrom' and 'validTo' date (", paste(x$validTo[equal], collapse=", "), "). One day subtracted from 'validTo'."))
		temp <- as.POSIXct(x$validTo[equal], tz=tz, format="%Y-%m-%dT%H:%M:%S") - 86400
		temp <- format(temp, format="%Y-%m-%dT%H:%M:%OS3Z")
		x$validTo[equal] <- temp
	}
	x
}
# Split each line into multiple lines, for all date intervals that the line covers:
splitByValidFromValidTo <- function(x, uniqueValidFrom, uniqueValidTo){
	
	# Find all date intervals covered by the current interval (use only dates and not times, since the hour of registering may change (often 22:00 or 23:00)):
	#inside <- which(x$validFrom <= uniqueValidFrom  &  uniqueValidTo <= x$validTo)
	inside <- which(toDate(x$validFrom) <= toDate(uniqueValidFrom)  &  toDate(uniqueValidTo) <= toDate(x$validTo))
	numInside <- length(inside)

	# Repeat the data into a data frame with 'numInside' rows:
	x <- do.call("rbind", replicate(numInside, x, simplify=FALSE))
	# Save the original dates, since these can be used to order the rows in case of duplicated information in the same time interval:
	x$validFrom_Original <- x$validFrom
	x$validTo_Original <- x$validTo
	# Insert the dates of the intervals into which the data have been split:
	x$validFrom <- uniqueValidFrom[inside]
	x$validTo <- uniqueValidTo[inside]
	# Include the index of the time interval:
	x <- cbind(x, DateInt=inside)
	x
}

# Function for extracting reference data of the ordinary type (not platform or taxa):
getReference <- function(x, ver, element="element"){
	if(ver$API$reference == 1){
		if("element" %in% names(x)){
			if("text" %in% names(x[[1]])){
				x <- sapply(x[names(x)=="element"], "[[", "text")
			}
			else{
				x <- as.matrix_full(x[names(x)=="element"])
			}
		}
		# Special case for platform:
		else if("FixedCoastalstation" %in% names(x)){
			x <- getElements(x, levels=list("FixedCoastalstation", NA))
			x <- as.dataFrame_full(x)
			x
		}
		# Else do a basic simplification:
		else if(is.list(x[[1]])){
			x <- t(Reduce(cbind, x))
		}
		# Convert to data frame with numerics where this is appropriate:
		x <- as.numericDataFrame(x)
	}
	#else if(ver$API$reference==2){
	else if(ver$API$reference >= 2){
		x <- getElements(x, levels=list("row", NA))
		if(!is.data.frame(x)){
			x <- as.dataFrame_full(x)
			x <- as.numericDataFrame(x)
		}
	}
	if(ver$API$reference > 2){
		warning("API version > 2 for reference has not been fully tested")
	}
	#else{
	#	stop("Invalid NMD API version for reference. See getRstoxDef(\"NMD_API_versions\") for implemented APIs for the different data sources.")
	#}
	return(x)	
}

# Extract the relevant data from the platform reference data:
getPlatform <- function(x, ver){
	if(ver$API$reference == 1){
		warning("Requesting platform data from version 1 returns only the latest information and is deprecated.")
		getPlatformV1(x)
	}
	#else if(ver$API$reference==2){
	else if(ver$API$reference >= 2){
		getPlatformV2(x)
	}
	if(ver$API$reference > 2){
		warning("API version > 2 for reference has not been fully tested")
	}
	#else{
	#	stop("Invalid NMD API version for reference. See getRstoxDef(\"NMD_API_versions\") for implemented APIs for the different data sources.")
	#}
}
# Version 1 of the API:
platformExtract <- function(x){
	# Small function for extracting the platform code from an NMD platform:
	getPlatformCode <- function(xx){
		out <- xx["platformCode"]
		names(out) <- gsub(" ", "_", xx["sysName"])
		out
	}
	# Small function for extracting the from and to date from a NMD platform:
	getvalidFromTo <- function(xx){
		c(xx["validFrom"], xx["validTo"])
	}
	# Extract the relevant data from the platform reference data:
	platformExtractOne <- function(x){
		# Group the platformCodes according to dates:
		codes <- unlist(x[names(x)=="platformCodes"], recursive=FALSE, use.names=FALSE)
		dates <- NULL
		if(length(codes)){
			# Get the dates:
			dates <- t(sapply(codes, getvalidFromTo))
		
			# Get the paltform codes:
			codes <- lapply(codes, getPlatformCode)
			# Split the platform codes by the concatination of start and end date (converted to a factor with the original ordering):
			validFromTo <- apply(dates, 1, paste, collapse="")
			validFromTo <- factor(validFromTo, levels=unique(validFromTo))
			#codes <- split(codes, apply(dates, 1, paste, collapse=""))
			codes <- split(codes, validFromTo)
			# Get all names of each unique specification of valid dates:
			codes <- lapply(codes, unlist)
			# Convert to a matrix, adding NAs at missing fields:
			codes <- as.matrix_full(codes)
			# Refresh the dates:
			dates <- unique(dates)
		}
		# Column added on 2018-01-29 on request from Edvin:
		platformNumber <- x$.attrs["platformNumber"]
		out <- cbind(nation=unname(x$nation$.attrs), platformNumber=platformNumber, platformType=unname(x$platformType$.attrs), codes, dates)
		out <- as.data.frame(out, stringsAsFactors=FALSE)
		out
	}
	# Drop elements with length 1, indicating time stamps or similar:
	x <- x[sapply(x, length)>1]
	lapply(x, platformExtractOne)
}
#' @importFrom utils head tail
getPlatformV1 <- function(x){
	x <- platformExtract(x)
	# Changed to extracting all info from the latest velidTo:
	extractLatestValidTo <- function(x){
		if(length(x$validTo)==0){
			return(utils::head(x, 1))
		}
		latestValidTo <- utils::tail(sort(x$validTo), 1)
		equalToLatestValidTo <- x$validTo==latestValidTo
		as.data.frame(t(apply(x[equalToLatestValidTo,], 2, function(x) utils::head(x[!is.na(x)], 1))))
	}
	x <- lapply(x, extractLatestValidTo)
	x <- as.matrix_full(x)
	x <- as.numericDataFrame(x)
	x
}
# Version 2 of the API:
getPlatformOneV2 <- function(x){
	# Read the platformCode:
	if(length(x$platformCodes)){
		# Get the platform data of the current platform:
		codes <- getElements(x$platformCodes, levels=list("platformCode", NA), equalLen=paste("Platform number", x$platformNumber))
		
		# Check for identical dates in valid from and to, and subtract one day from the valid to with a warning:
		codes <- corectEqualFromTo(codes, platformNumber=x$platformNumber)
		
		# Save the ISO_8601 datetimes for use if there is only one interval:
		codes$validFrom_ISO_8601 <- codes$validFrom
		codes$validTo_ISO_8601 <- codes$validTo
		# Convert to UNIX time:
		#codes$validFrom <- unclass(as.POSIXct(codes$validFrom, tz="UTC", format="%Y-%m-%dT%H:%M:%S"))
		#codes$validTo <- unclass(as.POSIXct(codes$validTo, tz="UTC", format="%Y-%m-%dT%H:%M:%S"))
		codes$validFrom_POSIXct <- as.POSIXct(codes$validFrom, format="%Y-%m-%dT%H:%M:%S")
		codes$validTo_POSIXct <- as.POSIXct(codes$validTo, format="%Y-%m-%dT%H:%M:%S")
		codes$validFrom <- unclass(codes$validFrom_POSIXct)
		codes$validTo <- unclass(codes$validTo_POSIXct)
		
		# Get first unique validFrom times:
		uniqueValidFrom <- sort(unique(codes$validFrom))
		uniqueValidTo <- sort(unique(codes$validTo))
		# Then find any dates in validTo which are not equal to any of the dates in validFrom + 1 day:
		inValidTo <- which(!(toDate(uniqueValidTo) + 1) %in% toDate(uniqueValidFrom))
		# The last element of inValidTo is the day after the last validTo, and should not be included in the unique validFrom:
		inValidTo <- inValidTo[-length(inValidTo)]
		# Insert the missing validFrom times into 'uniqueValidFrom':
		uniqueValidFrom <- sort(unique(c(uniqueValidFrom, uniqueValidTo[inValidTo] + 86400)))
		
		# If only one time interval, simply create the output here:
		if(length(uniqueValidFrom)==1){
			#platform <- list(column2ilst(codes, "value", "sysname"), validFrom=codes$validFrom_ISO_8601[1], validTo=codes$validTo_ISO_8601[1])
			platform <- list(column2ilst(codes, col="value", colnames="sysname"), validFrom=codes$validFrom_POSIXct[1], validTo=codes$validTo_POSIXct[1])
		}
		else{
			# Set the unique validTo times, and convert both the unique validFrom and validTo to ISO_8601 for insertion into the output:
			uniqueValidTo <- c(uniqueValidFrom[-1] - 86400, max(codes$validTo))
			#uniqueValidFrom_ISO_8601 <- format(as.POSIXct(uniqueValidFrom, origin="1970-01-01", tz="UTC"), format="%Y-%m-%dT%H:%M:%OS3Z")
			#uniqueValidTo_ISO_8601 <- format(as.POSIXct(uniqueValidTo, origin="1970-01-01", tz="UTC"), format="%Y-%m-%dT%H:%M:%OS3Z")
			uniqueValidFrom_POSIXct <- as.POSIXct(uniqueValidFrom, origin="1970-01-01")
			uniqueValidTo_POSIXct <- as.POSIXct(uniqueValidTo, origin="1970-01-01")
			uniqueValidFrom_ISO_8601 <- format(uniqueValidFrom_POSIXct, format="%Y-%m-%dT%H:%M:%OS3Z")
			uniqueValidTo_ISO_8601 <- format(uniqueValidTo_POSIXct, format="%Y-%m-%dT%H:%M:%OS3Z")
			
			# Split the intercals of each column of 'codes', but split into a list of individual rows first to comply with lapply(). This avoids the problem that apply reduces the rows of a data frame to a simple vector, making the "$" notation inapropriate:
			codesList <- split(codes, seq_len(nrow(codes)))
			out <- lapply(codesList, splitByValidFromValidTo, uniqueValidFrom=uniqueValidFrom, uniqueValidTo=uniqueValidTo)
			# Rbind into a data frame:
			temp <- do.call(rbind, out)
			# Replace space ty underscore in sysname, peparing it for conversion to columns in a data frame:
			temp$sysname <- gsub(" ", "_", temp$sysname)
			# temp[order(temp$sysname, temp$validFrom),]
			temp <- temp[order(temp$sysname, temp$validFrom_Original, decreasing=TRUE), ]
			
			#if(raw.out){
			#	return(temp)
			#}
		
			# Define a data frame of NAs, with the the different values of 'sysname' as columns:
			numSysnames <- length(unique(temp$sysname))
			platform <- as.data.frame(array(NA, dim=c(max(temp$DateInt), numSysnames)))
			names(platform) <- unique(temp$sysname)
			#platform <- cbind(DateInt=seq_len(nrow(platform)), platform, validFrom=uniqueValidFrom_ISO_8601, validTo=uniqueValidTo_ISO_8601)
			#platform <- cbind(platform, validFrom=uniqueValidFrom_ISO_8601, validTo=uniqueValidTo_ISO_8601, stringsAsFactors=FALSE)
			platform <- cbind(platform, validFrom=uniqueValidFrom_POSIXct, validTo=uniqueValidTo_POSIXct, stringsAsFactors=FALSE)
		
			# Insert the data into the 'platform' data frame:
			indexMatrix <- cbind(temp$DateInt, match(temp$sysname, names(platform)))
			# Check for duplicated info for the same time interval:
			dup <- which(duplicated(indexMatrix))
			if(length(dup)){
				# Keep only the data with the latest validFrom:
				temp <- temp[-dup, , drop=FALSE]
				warning(paste0("Platform number ", x$platformNumber, " contained duplicated information in the same time interval. Removing ", length(dup), " row(s)."))
			}
		
			# Insert into the plarform data frame:
			indexMatrix <- cbind(temp$DateInt, match(temp$sysname, names(platform)))
			platform[indexMatrix] <- temp$value
		}
	}
	else{
		warning(paste0("Platform with platform number ", x$platformNumber, " contained no rows."))
		platform <- list()
	}
	
	# Add the data which is not in a list and not attributes:
	headerData <- !sapply(x, is.list) & names(x) != ".attrs"
	platform <- as.data.frame(c(x[headerData], platform), stringsAsFactors=FALSE)
	
	# Special care of the nation, which is stripped for trailing whitespace:
	platform$nation <- trimws(platform$nation, which=c("right"))
	
	return(platform)
}
getPlatformV2 <- function(x){
	cat("Creating platform data frame...\n")
	x <- papply(x, getPlatformOneV2)
	# Split into individual rows for as.matrix.full() to work:
	x <- lapply(x, function(DF) split(DF, seq_len(nrow(DF))))
	x <- unlist(x, recursive=FALSE, use.names=FALSE)
	x <- as.dataFrame_full(x)
	x <- as.numericDataFrame(x)
	x
}

# Extract the relevant data from the taxa reference data:
getTaxa <- function(x, ver){
	if(ver$API$reference == 1){
		warning("Requesting taxa data from version 1 is no longer available.")
		return(NULL)
		#getTaxaV1(x)
	}
	#else if(ver$API$reference==2){
	else if(ver$API$reference >= 2){
		getTaxaV2(x)
	}
	if(ver$API$reference > 2){
		warning("API version > 2 for reference has not been fully tested")
	}
	#else{
	#	stop("Invalid NMD API version for reference. See getRstoxDef(\"NMD_API_versions\") for implemented APIs for the different data sources.")
	#}
}
# Function used for simplifiying taxa data into a matrix:
gettaxaMatrix <- function(x, name=".attrs"){
	# Get the number of elements per taxa for the specified name:
	Ind <- sapply(x, function(x){ temp <- x[[name]]; if(is.list(temp)) length(temp) else 1 }) # Vector
	Ind <- rep(seq_along(Ind), Ind) # Vector

	# If the data are given as a list for each taxa, get the indices for the rows at which to insert the data in the matrix 'allMatrix':
	numFull <- unlist(lapply(x, function(x){ temp <- x[[name]]; if(is.list(temp)) unlist(lapply(temp, length)) else length(temp) })) # Vector
	# Get the indices to which taxa each element in all belong:
	rowInd <- rep(seq_along(numFull), numFull) # Vector

	# Get a vector of the data for each taxa, in a list:
	allList <- lapply(x, function(x) unlist(x[[name]])) # List
	# Flatten the data in one vector:
	all <- unlist(allList) # Vector
	# Get all names:
	allNames <- unlist(lapply(x, function(x){ temp <- x[[name]]; if(is.list(temp)) names(unlist(temp)) else names(temp) })) # Vector
	# Get the unique names 
	allUniqueNames <- unique(allNames)
	# Create a matrix of NAs to be filled with the data using the indices 'rowInd' and names 'allNames':
	allMatrix <- as.data.frame(array(NA, dim=c(max(rowInd), length(allUniqueNames))), stringsAsFactors=FALSE)
	names(allMatrix) <- allUniqueNames
	allMatrix[cbind(rowInd, match(allNames, allUniqueNames))] <- all
	cbind(Ind=Ind, allMatrix)
}
# Version 1 of the API:
getTaxaV1 <- function(x){
	attrs <- gettaxaMatrix(x, name=".attrs")
	synonyms <- gettaxaMatrix(x, name="taxaSynonyms")
	x <- merge(attrs, synonyms)
	x <- as.numericDataFrame(x)

	# Convert to one row per species, with the scientific, norwegian, english and russian name as columns:
	# Extract the preferred:
	x <- x[x$synonym.preferred==1, ]

	# Get the first row of each 'Ind':
	out <- x[!duplicated(x$Ind), !names(x) %in% c("synonym.language", "synonym.name", "synonym.preferred")]
	# Make sure that 'Ind' are the row indices:
	out <- out[order(out$Ind), ]

	# Get all present values of 'synonym.language':
	synonym.language <- unique(x$synonym.language)
	# Create a data frame of NAs for the synonym.language, and insert present values into this data frame:
	synonym.name <- as.data.frame(array("", dim=c(nrow(out), length(synonym.language))), stringsAsFactors=FALSE)
	names(synonym.name) <- synonym.language

	for(this in synonym.language){
		y <- x[x$synonym.language == this, ]
		synonym.name[y$Ind, this] <- y[, "synonym.name"]
	}

	# Insert the columns of scientific, norwegian, english and russian names:
	x <- cbind(out[, c("Ind", "tsn")], synonym.name, out[, !names(out) %in% c("Ind", "tsn")])
	x
}
# Version 2 of the API:
getTaxaOneV2 <- function(x){
	# Read the platformCode:
	if(length(x$TaxaSynonyms)){
		codes <- getElements(x$TaxaSynonyms, levels=list("synonym", NA))
		# Split by 'preferred'', and pick the latest:
		codes <- split(codes, codes$preferred)
		# Add the non-preferred (old names) in a string in the column OldNames:
		if(length(codes$false)){
			OldNames <- paste(apply(codes$false[, c("language", "name")], 1, paste, collapse=": "), collapse=", ")
		}
		else{
			OldNames <- NA
		}
		codes <- codes$true
		
		# Create a data frame added the validTo info:
		codes <- list(column2ilst(codes, col="name", colnames="language"), preferred=codes$preferred[1], OldNames=OldNames)
	}
	else{
		codes <- list()
	}
	
	# Add the data which is not in a list and not attributes:
	headerData <- !sapply(x, is.list) & names(x) != ".attrs"
	codes <- as.data.frame(c(x[headerData], codes), stringsAsFactors=FALSE)
	
	return(codes)
}
getTaxaV2 <- function(x){
	x <- lapply(x, getTaxaOneV2)
	#x <- as.dataFrame_full(x)
	x <- as.dataFrame_full(x)
	# Add the same Ind column as in version 1:
	x <- cbind(Ind = seq_len(nrow(x)), x)
	x <- as.numericDataFrame(x)
	x
}

getCruiseInfo <- function(ver, API, msg=FALSE){
	dataSource <- "biotic"
	if(ver$API[[dataSource]] < 2){
		stop("Listing all cruises is only available as of version 2 of the API used by biotic.")
	}
	URL <- addQuery(paste(API, dataSource, paste0("v", ver$API[[dataSource]]), sep="/"), type="ListAll")
	d <- downloadXML(URL, msg=msg)
	s <- lapply(d, function(x) column2ilst(getRowElementValueWithName(x), col="text", colnames=".attrs"))
	x <- as.dataFrame_full(s)
	x <- as.numericDataFrame(x)
	x
}
###############################################
##### <<<Internal functions of getNMDinfo #####
###############################################


###############################################
##### Internal functions of getNMDdata>>> #####
###############################################

##### Basic functions: #####
isSerialno <- function(cruise, ver){
	cs <- getNMDinfo("cs", recursive=FALSE, ver=ver)
	length(cruise)==1 && cruise %in% cs
}
isCS <- function(cruise, ver){
	cs <- getNMDinfo("cs", recursive=FALSE, ver=ver)
	length(cruise)==1 && cruise %in% cs
}
isSTS <- function(cruise, ver){
	sts <- getNMDinfo("sts", recursive=FALSE, ver=ver)
	length(cruise)==1 && cruise %in% sts
}
downloadFailedWarning <- function(x, downloadSuccess, type=c("file", "sts")){
	#warning(paste0("Downloading failed for the following ", if(type[1]=="file") "files" else "Survey Timeseries", ":\n\t", paste(x, collapse="\n\t"), "\nPossible reason: Timeout during downloading, in which case the timeout option could be increased (from the default value getOption(\"timeout\")) by, e.g., options(timeout=600) for UNIX systems, and options(download.file.method=\"internal\", timeout=600) for Windows systems, where the default download method does not repond to setting the timeout option from R)"))
	x <- x[which(!downloadSuccess)]
	if(length(x)){
		warning(paste0("Downloading failed for the following ", if(type[1]=="file") "files" else "Survey Timeseries", ":\n\t", paste(x, collapse="\n\t")))
	}
	invisible(x)
}


##### Functions for paths: #####
# Function for building a project path, possibly abbreviating words:
abbrevPath <- function(x, abbrev=FALSE){
	# Define first the file name:
	if(length(x$name) && !is.na(x$name)){
		filename <- abbrevWords(x$name, abbrev=abbrev)
	}
	else{
		filename <- NULL
	}
	
	# Add file base, such as "NMD" separated by "_":
	if(length(x$prefix) && !is.na(x$prefix)){
		filename <- paste(c(x$prefix, filename), collapse="_")
	}
	# Add the suffix, abbreviating stricter than the rest of the path, by sub=-1:	
	if(length(x$suffix) && !is.na(x$suffix)){
		filename <- paste(c(filename, abbrevWords(x$suffix, abbrev=abbrev, sub=-1, collapse="")), collapse="_")
	}
	# Add the file ext:	
	if(length(x$ext) && !is.na(x$ext)){
		filename <- paste(filename, x$ext, sep=".")
	}
	
	# Should a subdirectory be used?:
	if(length(x$subdir) && !is.na(x$subdir)){
		x$dir <- file.path(x$dir, abbrevWords(x$subdir, abbrev=abbrev))
	}
	# Get the path:
	projectPaths <- file.path(x$dir, filename)
	# Strip off any double slashes
	gsub("//", "/", projectPaths)
}
# Small function used to interpret the subdir parameter:
getSubdir <- function(subdir, name, nprojects=NA){
	# No subdir if there is only one project:
	if(!is.na(nprojects) && nprojects==1){
		NA
	}
	# TRUE imples using the provided 'name':
	else if(isTRUE(subdir)){
		name
	}
	# The subdir can be given as a path:
	else if(is.character(subdir)){
		subdir
	}
	# Any other specification implies no subdir:
	else{
		NA
	}
}
# Function for defining the default grouping. If only one cruise is requested, this is grouped by cruise, implying that it will have "CruiseNumber_THECRUISENUMBER" as suffix:
getGroup <- function(cruiseInfo, group){
	#if(is.na(group) || length(group)==0){
	if(any(is.na(group), length(group)==0, startsWith(tolower(group), "a"), na.rm=TRUE)){
		group <- "all"
	}
	# Apply the default, which is by cruise if only one cruise is requested, and by year if not:
	else if(startsWith(tolower(group), "d")){
		if(nrow(cruiseInfo)==1){
			group = "cruise"
		}
		else{
			group = "year"
		}
	}
	# Force grouping by cruise if only one cruise is requested, implying that these will be a suffix for cruise number and ship name in the StoX project name:
	else if(nrow(cruiseInfo)==1){
		group = "cruise"
	}
	
	cruiseInfo$group <- group
	cruiseInfo
}
# Function for constructing the suffix of a project name, given arbitrary inputs:
getSuffix <- function(...){
	l <- list(...)
	lens <- sapply(l, length)
	l <- l[lens > 0]
	if(length(l)==0){
		return(NULL)
	}
	
	#  Get a data frame of the suffix parts:
	out <- lapply(seq_along(l), function(i) paste(names(l)[i], l[[i]], sep="_"))
	out <- as.data.frame(out, stringsAsFactors=FALSE)
	out <- apply(out, 1, paste, collapse="_")
	out
}
# Function for getting StoX_data_sources given NMD dataSource:
getStoX_data_sourceFromNMD_data_source <- function(NMD_data_source){
	getRstoxDef("StoX_data_sources")[NMD_data_source]
}
# Function for adding all data sources of each row of 'cruiseInfo' in the columns StoX_data_source and NMD_data_source:
addDataSources <- function(cruiseInfo, dataSource){
	# Get the StoX data soruces correspoding to the (NMD) dataSource:
	StoX_data_source <- getRstoxDef("StoX_data_sources")[dataSource]
	dataSourceDF <- data.frame(
		StoX_data_source = StoX_data_source, 
		NMD_data_source = dataSource, stringsAsFactors=FALSE
	)
	
	# Merge with the cruiseInfo:
	merge(cruiseInfo, dataSourceDF)
}

# Function for constructing an xml file name from the NMD convension (dataSource_"cruiseNumber"_CruiseNumber_ShipName.xml) for all data sources 'dataSource':
#NMDFileName <- function(projectDir, dataSource, CruiseNumber, ShipName){
#	# Get the file name except the prefix denotimg data source:
#	filenameExptPrefix <- paste("cruiseNumber", CruiseNumber, ShipName, sep="_")
#	# Add file extension:
#	filenameExptPrefix <- paste0(filenameExptPrefix, ".xml")
#	
#	# Get the paths to the input data folders, added the requested data source :
#	StoX_data_sources <- getStoX_data_sourceFromNMD_data_source(dataSource)
#	projectDataPaths <- file.path(projectDir, "input", StoX_data_sources, dataSource)
#	# Add data sources:
#	out <- t(outer(projectDataPaths, filenameExptPrefix, paste, sep="_"))
#	# Convert to data frame and add names of the folders in StoX:
#	out <- as.data.frame(out, stringsAsFactors=FALSE)
#	names(out) <- StoX_data_sources
#	out
#	
#	## Get the NMD file name convension:
#	#out <- paste(dataSource, "cruiseNumber", CruiseNumber, ShipName, sep="_")
#	## Add file extension:
#	#out <- paste0(out, ".xml")
#	#out
#}
NMDFileName <- function(projectDir, cruiseInfo){
	# Get the file name except the prefix denotimg data source:
	filenameExptPrefix <- paste("cruiseNumber", cruiseInfo$Cruise, cruiseInfo$ShipName, sep="_")
	# Add file extension:
	filenameExptPrefix <- paste0(filenameExptPrefix, ".xml")
	
	# Get the paths to the input data folders, added the requested NMD data source as the prefix of the basename:
	projectDataPaths <- file.path(projectDir, "input", cruiseInfo$StoX_data_source, cruiseInfo$NMD_data_source)
	# Merge the two:
	out <- paste(projectDataPaths, filenameExptPrefix, sep="_")
	out
}
# Function for adding the project ID by which the cruises are grouped, as defeined by the 'group' parameter:
getProjectID <- function(cruiseInfo){
	# Get the group variable from the 'cruiseInfo':
	group <- cruiseInfo$group[1]
	if(nrow(cruiseInfo) == 1 || startsWith(tolower(group), "a")){
		cruiseInfo$ProjectID <- 1
	}
	else{
		if(startsWith(tolower(group), "y")){
			cruiseInfo$ProjectID <- cruiseInfo$Year
		}
		else{
			cruiseInfo$ProjectID <- cruiseInfo$Cruise
		}
		
	}
	# Add the group parameter for use when defining the suffix on the project names:
	#cruiseInfo$group <- group
	
	return(cruiseInfo)
}
# Function for getting parts of the project/file names:
getProjectPathElements <- function(dir, subdir=NA, prefix=NA, name=NA, suffix=NA, ext=NA){
	out <- data.frame(dir=dir, subdir=subdir, prefix=prefix, name=name, suffix=suffix, ext=ext, stringsAsFactors=FALSE)
	split(out, seq_len(nrow(out)))
	#as.data.frame(t(out))
}
# Function for constructing the paths of the StoX project(s), given the type of download. The output is a list of the elements projectPaths = an unnamed vector of paths to the individual projects, and either filePaths = a named list of file paths for downloadType=="serialno", or the cruiseInfo added file paths for downloadType %in% c("sts", "cs", "c"):
getPaths <- function(downloadType=c("serialno", "sts", "stszip", "cs", "c"), dir=NA, subdir=NA, name=NA, prefix=NA, year=NA, serialNumber=NA, tsn=NA, cruiseInfo=NA, abbrev=FALSE, dataSource=NULL, StoX_data_sources=NULL){
	##### Get project paths: #####
	# Remove prefix for cruise- and survey time series:
	if(tolower(downloadType[1]) %in% c("cs", "sts", "stszip")){
		prefix <- NA
		# Also set the subdir value, either as 'subdir' or as the name of the subdir, given in 'name':
		subdir <- getSubdir(subdir, name, nprojects=NA)	
	}
	
	# Get cruise number and ship name of cruise series or single cruises:
	if(tolower(downloadType[1]) %in% c("cs", "c")){
		# Get the first cruise number of each project, which coincides with all cruise numbers, since this is only used when group=="c" or tolower(downloadType[1]) == "c":
		CruiseNumber <- sapply(cruiseInfo, "[[", "Cruise")
		# Likewise with the ship name:
		ShipName <- sapply(cruiseInfo, "[[", "ShipName")
		# Pick out the first of the group values, since all these are identical:
		group <- cruiseInfo[[1]]$group[1]
	}
	
	# Different suffix definitions for each type of download:
	if(tolower(downloadType[1]) == "serialno"){
		# Get the ranges of the individual serial numbers and the overall range:
		serialNumberRange <- paste(range(serialNumber), collapse="-")
		serialnoIndividualRanges <- apply(serialNumber, 1, paste, collapse="-")
		# Generate suffix for the project and for each file:
		suffix <- getSuffix(Year=year, SerialNumber=serialNumberRange, TSN=tsn)
		suffixIndividual <- getSuffix(Year=year, SerialNumber=serialnoIndividualRanges, TSN=tsn)
		# Do no put the project into sub-folders:
		subdir <- NA
		# No name of the project, only suffix (name is for survey time- and cruise seres)
		name <- NA
	}
	else if(tolower(downloadType[1]) == "stszip"){
		suffix <- getSuffix(Year=cruiseInfo$sampleTime)
	}
	else if(tolower(downloadType[1]) == "sts"){
		# Pick out the years from each project:
		suffix <- sapply(cruiseInfo, function(x) x$Year[1])
	}
	else if(tolower(downloadType[1]) == "cs"){
		# Cruise series <ed by year gets Year in the suffix:
		if(startsWith(tolower(group), "y")){
			# Interpret the year from the cruise number:
			suffix <- getSuffix(Year=sapply(CruiseNumber, getYearFromCruiseNumber))
		}
		# Cruise series grouped by cruise gets the same suffix as single cruises:
		else if(startsWith(tolower(group), "c")){
			suffix <- getSuffix(CruiseNumber=CruiseNumber, ShipName=ShipName)
		}
		# Use "All" as suffix when all cruises are grouped together:
		else if(startsWith(tolower(group), "a")){
			suffix <- "Alldata"
			subdir <- NA
		}
	}
	else if(tolower(downloadType[1]) == "c"){
		suffix <- getSuffix(CruiseNumber=CruiseNumber, ShipName=ShipName)
		subdir <- NA
		# No name of the project, only suffix (name is for survey time- and cruise seres)
		name <- NA
	}
	
	# Paste and possibly abbreviate the paths:
	projectPathElements <- getProjectPathElements(
		dir = dir, 
		subdir = subdir, 
		prefix = prefix, 
		name = name, 
		suffix = suffix
	)
	projectPaths <- sapply(projectPathElements, abbrevPath, abbrev=abbrev)
	
	
	##### Get file paths: #####
	# Different file name for each type of download:
	if(tolower(downloadType[1]) == "serialno"){
		# Define the folder of the biotic files og serialno:
		projectDataPaths <- file.path(projectPaths, "input", "biotic")
		# Use the same suffix as in the project name, and add file extension:
		filePathElements <- getProjectPathElements(
			dir = projectDataPaths, 
			suffix = suffixIndividual, 
			ext = "xml"
		)
		# Get the file paths of the serialno search, but do not abbreviate the file names, since the serialno ranges are already abbreviating by paring only the min and max serialno of each file:
		filePaths <- sapply(filePathElements, abbrevPath, abbrev=FALSE)
		
		# Return both the project and file paths:
		list(projectPaths=projectPaths, filePaths=filePaths)
	}
	else if(tolower(downloadType[1]) == "stszip"){
		filePaths <- NULL
		
		# Return both the project and file paths:
		list(projectPaths=projectPaths)
	}
	else if(tolower(downloadType[1]) %in% c("sts", "cs", "c")){
		# Get the file names one data source at the time, and return a list of file names, named by the 'dataSource':
		getFileNamesForOneProject <- function(i, cruiseInfo, projectPaths, dataSource, StoX_data_sources){
			# Use the info of the current group of cruises (representing one StoX project), and build the file paths using the naming convension of NMD, see NMDFileName():
			FilePath <- NMDFileName(projectDir=projectPaths[i], cruiseInfo=cruiseInfo[[i]])
			# Add the file paths to the cruiseInfo, and return this data frame as the 'filePaths'. Then 'filePaths' is different for "serialno" (list) and "sts", "cs", "c" (data frame):
			cbind(cruiseInfo[[i]], FilePath=FilePath, stringsAsFactors=FALSE)
		}
		
		# Get a data frame of the file names for each project:
		cruiseInfo <- lapply(seq_along(cruiseInfo), getFileNamesForOneProject, cruiseInfo=cruiseInfo, projectPaths=projectPaths, dataSource=dataSource, StoX_data_sources=StoX_data_sources)
		
		# Return both the project and file paths:
		list(projectPaths=projectPaths, cruiseInfo=cruiseInfo)
	}
}

# Function for geting a subset of the cruises of a cruise series or years of a survey time series:
getSubset <- function(subset, nprojects, info){
	if(length(subset) == 0){
		subset = seq_len(nprojects)
	}
	else{
		# Check whether the subset is a STS sampleTime:
		if(all(nchar(subset) > 3) && any(subset %in% info[, "sampleTime"])){
			subset <- which(subset == info[, "sampleTime"])
		}
		# Check whether the subset is a year or cruise code:
		else if(all(nchar(subset) > 3) && any(subset %in% names(info))){
			subset <- which(subset == names(info))
		}
		# Otherwise, restrict 'subset' to the range of projects:
		else{
			subset = subset[subset>=1 & subset<=nprojects]
		}
		if(length(subset)==0){
			warning("The value of 'subset' excluded all projects (or for cruise series, years or cruises, or all data grouped in one project if group = NULL, NA or 'all')")
			return(NULL)
		}
	}
	subset
}


##### Serial number serialno: #####
# Function for converting a vector of serial numbers, which can be fully or partly sequenced (incriment of 1 between consecutive elements):
getSerialnoRanges <- function(x){
	d <- diff(c(x))
	starts <- c(1, which(d != 1)+1)
	ends <- c(which(d != 1), length(x))
	cbind(startSerialno=x[starts], endSerialno=x[ends])
}
# Function for getting the URL for serial number searches:
getURLBySerialno <- function(serialno, year, tsn=NULL, ver=getRstoxDef("ver"), API="http://tomcat7.imr.no:8080/apis/nmdapi"){
	
	# ### Version 1: ###
	# http://tomcat7.imr.no:8080/apis/nmdapi/biotic/v1/2016/1/100/serial
	# http://tomcat7.imr.no:8080/apis/nmdapi/biotic/v1/2016/1/2020/164712/serial
	# ### Version 2: ###
	# http://tomcat7.imr.no:8080/apis/nmdapi/biotic/v2/2016/1/100/serial?version=1.4
	# http://tomcat7.imr.no:8080/apis/nmdapi/biotic/v2/2016/1/2020/164712/serial?version=1.4
	# # Change is in v1/v2, in ?version=1.4, and in an error in the output for V2
	
	# Get the version and the URL:
	dataSource <- "biotic"
	version <- paste0("v", ver$API[[dataSource]])
	out <- paste(c(API, dataSource, version, year[1], serialno[1], serialno[2], tsn[1], "serial"), collapse="/")
	
	# Add the query of type version, but not in version 1 of the API for biotic (removing ver[[dataSource]]) to disable the query):
	if(ver$API[[dataSource]]==1){
		ver[[dataSource]] <- NULL
	}
	out <- addQuery(out, version=ver[[dataSource]])
	out
}
# Apply restrictions to the serialno, and request all serialno if year is given:
getSerialno <- function(serialno, year, maxSerialno=99999){
	if(length(serialno)==0 && length(year)){
		serialno <- seq(1, maxSerialno)
	}
	if(any(serialno > maxSerialno)){
		serialno <- serialno[serialno <= maxSerialno]
		warning(paste0("Maximum serialno is ", maxSerialno))
	}
	
	# Get the range of serialno:
	serialnoRange <- getSerialnoRanges(serialno)
	
	serialnoRange
}
# Function for downloading a serial number range:
downloadSerialno <- function(serialno, downloadType, year=NULL, tsn=NULL, prefix, dir, model, ow, ver, API, run, return.URL, msg, timeout, ...){
	if(length(year)==0){
		warning("'year' must be given when serial number is requested")
		return(NULL)
	}
	
	#serialnoRange <- paste(range(serialno), collapse="-")
	#serialno <- getSerialnoRanges(serialno)
	#serialnoRanges <- apply(serialno, 1, paste, collapse="-")
	temp <- getPaths(downloadType=downloadType, dir=dir, subdir=NA, name=NA, prefix=prefix, year=year, serialNumber=serialno, tsn=tsn, abbrev=FALSE)
	projectPaths <- temp$projectPaths
	filePaths <- temp$filePaths
	if(!run){
		return(projectPaths)
	}
	
	# Get and possibly return the URLs:
	URLs <-character(nrow(serialno))
	for(i in seq_len(nrow(serialno))){
		URLs[i] <- getURLBySerialno(serialno=serialno[i,], year=year[1], tsn=tsn, ver=ver, API=API)
	}
	if(return.URL){
		return(data.frame(year=year, serialno, URL=URLs, stringsAsFactors=FALSE))
	}
	
	
	# Create the project:
	projectName <- createProject(projectPaths, dir=dir, model=model, ow=ow, ...)

	# Download the files:
	for(i in seq_along(URLs)){
		downloadXML(URLs[i], msg=msg, list.out=FALSE, file=filePaths[i], timeout=timeout)
	}
	
	# Check whether the files were downloaded. This could have been done by use of the output from download.file (0 for sucsess and positive for failure), but instead we check the existence of the files, and the size:
	downloadSuccess <- file.exists(filePaths) & (file.info(filePaths)$size > 0) %in% TRUE
	# Warning if any downloads failed:
	downloadFailedWarning(filePaths, downloadSuccess)
	
	# Update and return the project:
	updateProject(projectName)
	return(projectName)
}


##### Survey time series: #####
# Function for getting the URLs of a survey time series:
getSurveyTimeSeriesZipURLs <- function(stsInfo, ver=getRstoxDef("ver"), API="http://tomcat7.imr.no:8080/apis/nmdapi", format="zip"){
	
	# ### Version 1: ###
	# http://tomcat7.imr.no:8080/apis/nmdapi/surveytimeseries/v1/'STS'/samples/'YEAR'?format=zip
	# ### Version 2: ###
	# http://tomcat7.imr.no:8080/apis/nmdapi/reference/v2/model/surveytimeseries/'STS_ID'/cruiseseries/'CS_ID'/samples/'YAER'/zip?format=zip&version=2.0
	# http://tomcat7.imr.no:8080/apis/nmdapi/reference/v2/model/surveytimeseries/1/cruiseseries/5/samples/1994/zip?format=zip&version=2.0

	# The number of stox projects:
	sts <- attr(stsInfo, "STSName")
	nsts <- nrow(stsInfo)
	URLs <- rep(NA, nsts)
	
	dataSource <- "reference"
	# Run through the projects and get URLs:
	for(i in seq_len(nsts)){
		if(ver$API[[dataSource]]==1){
			URL <- URLencode(paste(API, "surveytimeseries", paste0("v", ver$API[[dataSource]]), sts, "samples", stsInfo[i,"sampleTime"], sep="/"))
			# Add download type, but no version query for version 1:
			URL <- addQuery(URL, format=format)
		}
		else{
			URL <- URLencode(paste(API, dataSource, paste0("v", ver$API[[dataSource]]), "model/surveytimeseries", stsInfo$STSCode, "cruiseseries", stsInfo$CSCode, "samples", stsInfo[i,"sampleTime"], "zip", sep="/"))
			# Add download type:
			URL <- addQuery(URL, version=ver[[dataSource]], format=format)
		}
		
		URLs[i] <- URL
	}
	
	URLs
}
# Function for downloading a surveytimeseries:
getSurveyTimeSeriesZip <- function(stsInfo, dir, subdir, subset, cleanup, ow, abbrev, run, ver, msg, return.URL=FALSE){
	# Get the matrix of stoxProjectId and sampleTime (i.e., year), and the name of the survey time series (sts):
	sts <- attr(stsInfo, "seriesName")
	
	# Set the parts of the project name, one for each year:
	# See getSubdir(). If subdir==TRUE, this will become the survey time series name:
	projectPathElements <- getProjectPathElements(
		dir = dir, 
		subdir = getSubdir(subdir=subdir, name=sts), 
		prefix = NA, 
		name = sts, 
		suffix = stsInfo$sampleTime
	)
	nprojects <- length(projectPathElements)
	
	# Select all or some of the projects:
	subset <- getSubset(subset, nprojects=nprojects, info=stsInfo)
		
	projectPathElements <- projectPathElements[subset]
	stsInfo <- stsInfo[subset, , drop=FALSE]

	# Set original and abbreviated project names:
	projectPaths <- unlist(lapply(projectPathElements, abbrevPath, abbrev=abbrev))
	if(!run){
		return(projectPaths)
	}
	projectPathsOrig <- unlist(lapply(projectPathElements, abbrevPath, abbrev=FALSE))
	
	# The number of stox projects:
	nsts <- length(projectPaths)
	# Store the project names and the success of the downloads:
	downloadSuccess = logical(nsts)
	
	# Get the URLs (hard coded to use the zip download, since the preferred non-zip download will be incorporated as a cruise series):
	URLs <- getSurveyTimeSeriesZipURLs(stsInfo=stsInfo, ver=ver, format="zip")
	if(return.URL){
		return(data.frame(stsInfo, URL=URLs, stringsAsFactors=FALSE))
	}
	
	# Run through the projects and download:
	for(i in seq_len(nsts)){
		# Create the project directory:
		suppressWarnings(dir.create(dirname(projectPaths[i]), recursive=TRUE))

		# The following using onlyone=nsts==1 did not work, since typing "ya" did not continue the loop:
		temp <- downloadProjectZip(URL=URLs[i], projectName=projectPaths[i], cleanup=cleanup, msg=msg, ow=ow, onlyone=nsts==1)
		downloadSuccess[i] <- temp$downloadSuccess
		ow <- temp$ow
		#success[i] <- downloadProjectZip(URL=URLs[i], projectName=projectPaths[i], cleanup=cleanup, msg=msg, ow=ow)$success
	}
	
	# Warning if any downloads failed:
	downloadFailedWarning(projectPathsOrig, downloadSuccess, type="sts")
	
	# Report project names if abbreviated:
	if(!all(projectPaths==projectPathsOrig)){
		cat("Project names abbreviated:\n")
	}
	
	return(projectPaths)
}


##### Cruise series: #####
# Funciton to contruct the survey time series project name per year:
getSTSprojectName <- function(STSName, year){
	seriesName <- attr(getProjectXmlURLs(STSName), "seriesName")
	if(length(seriesName)){
		STSName <- seriesName
	}
	paste(STSName, year, sep="_")
}
# Functin for getting the project.XML URLs:
getProjectXmlURLs <- function(stsInfo){
	getProjectXmlURL <- function(year, stsInfo){
		stoxProjectId <- stsInfo$stoxProjectId[stsInfo$sampleTime == year]
		projectXmlURL <- paste0("http://tomcat7.imr.no:8080/apis/nmdapi/stox/v1/", stoxProjectId)
		projectXmlURL
	}
	stsInfo$projectXmlURL <- sapply(stsInfo$sampleTime, getProjectXmlURL, stsInfo=stsInfo)
	stsInfo
}
# Function for downloading all project.xml files to tempfiles:
downloadProjectXmlToTemp <- function(stsInfo, dir=NULL){
	# Function for downloading one project.xml file and returning the path to the downloaded file
	downloadProjectXmlToTempOne <- function(year, stsInfo, dir=NULL){
		# Use the tempdir if dir is not given:
		if(length(dir)==0){
			dir <- tempdir()
		}
		
		# Define the STSprojectName: 
		seriesName <- getSTSprojectName(stsInfo, year)
		# Save the project.xml file with that name and xml as file extension:
		projectXmlFile <- file.path(dir, seriesName, "project.xml")
		
		# Get the URL of the project.xml file of the requested year:
		projectXmlURL <- stsInfo$projectXmlURL[stsInfo$sampleTime == year]
		
		# Download the file:
		dir.create(dirname(projectXmlFile))
		download.file(URLencode(projectXmlURL), projectXmlFile, quiet=TRUE)
		
		# Return the local file name:
		projectXmlFile
	}
	
	# Get the URLs if missing:
	if(!"projectXmlURL" %in% names(stsInfo)){
		stsInfo <- getProjectXmlURLs(stsInfo)
	}
	
	# Return local file names of the project.xml files:
	stsInfo$projectXmlFile <- unlist(papply(stsInfo$sampleTime, downloadProjectXmlToTempOne, stsInfo=stsInfo, dir=dir, msg="Downloading project.xml files"))
	stsInfo
}
# Function for extracting ship name and cruise
#' @importFrom utils tail
extractCruiseAndShipame <- function(x){
	
	# Accept string input:
	if(is.character(x)){
		x <- list(FileName=x)
	}
	
	# Remove file ext:
	fileNamesSansExt <- tools::file_path_sans_ext(basename(x$FileName))
	
	# Get NMD_data_source as the everything before underscore in the file basenames:
	NMD_data_source <- sapply(strsplit(fileNamesSansExt, "[_]"), "[[", 1)
	
	# Get cruise number:
	cruiseNumberString <- "cruiseNumber_"
	atCruiseNumber <- regexpr(cruiseNumberString, fileNamesSansExt) + nchar(cruiseNumberString)
	# Extract the position of the last underscore:
	atUnderscore <- gregexpr("[_]", fileNamesSansExt)
	atUnderscore <- sapply(atUnderscore, utils::tail, 1)
	
	Cruise <- substr(fileNamesSansExt, atCruiseNumber, atUnderscore - 1)
	ShipName <- substring(fileNamesSansExt, atUnderscore + 1)
	
	# Return the cruise and ship name:
	out <- data.frame(Cruise=Cruise, ShipName=ShipName, NMD_data_sourceFromFileName=NMD_data_source, x, stringsAsFactors=FALSE)
	out	
}
# Function for extracting file names from the project.xml file, given the 
extractDataFileNames <- function(projectXML){
	
	# Function for extracting the file names for one StoX data source:
	extractDataFileNamesOneStoX_data_source <- function(ind, projectXMLParsed, ns){
		# Get the data source names of StoX and NMD (differing in the acoustic ~ echosounder):
		NMD_data_source <- getRstoxDef("NMD_data_sources")[ind]
		StoX_data_source <- getRstoxDef("StoX_data_sources")[ind]
		# Get the relevant StoX reading function for the current StoX_data_source:
		thisReadingProcess <- getRstoxDef("StoX_reading_processes")[ind]
		
		# Get the file names:
		path <- paste0("/x:project/x:model/x:process[@name='", thisReadingProcess, "']/x:parameter[contains(@name,'FileName')]")
		out <- sapply(XML::getNodeSet(projectXMLParsed, path, c("x"=ns)), XML::xmlValue)
		
		# Return a data frame with file names and dource types:
		if(length(out)){
			out <- data.frame(
				FileName = out, 
				NMD_data_source = NMD_data_source, 
				StoX_data_source = StoX_data_source, stringsAsFactors=FALSE
			)
		}
		out
	}

	# Parse the project.xml via the URL:
	projectXMLParsed <- XML::xmlParse(projectXML)
	nsDefs <- XML::xmlNamespaceDefinitions(projectXMLParsed)
	ns <- structure(sapply(nsDefs, function(x) x$uri), names = names(nsDefs))[[1]]
	
	# Get the data source names of StoX and NMD (differing in the acoustic ~ echosounder):
	StoX_data_sources <- getRstoxDef("StoX_data_sources")
	
	# Get the files for each StoX data source:
	out <- lapply(seq_along(StoX_data_sources), extractDataFileNamesOneStoX_data_source, projectXMLParsed=projectXMLParsed, ns=ns)
	
	# Combine into one data frame:
	out <- do.call(rbind, out)
	
	return(out)
}
# Function to add cruise, ship name and project.xml file URL to the info from getNMDinfo("sts"):
getCruiseInfoFromStsInfo <- function(stsInfo){
	
	# Function for getting the cruise and ship name from the output from getNMDinfo("sts") of one year:
	getCruiseInfoFromStsInfoOneYear <- function(year, stsInfo){
		
		# Extract the local path and URL of the project.xml:
		atYear <- stsInfo$sampleTime == year
		#projectXmlURL <- stsInfo$projectXmlURL[atYear]
		#projectXmlFile <- stsInfo$projectXmlFile[atYear]
		
		# Extract the data file names stored in the project.xml file:
		fileNames <- extractDataFileNames(stsInfo$projectXmlURL[atYear])
		# Extract the cruise and ship name from data file names:
		fileNames <- extractCruiseAndShipame(fileNames)
		
		# Add the year and the local path and URL of the project.xml:
		#fileNames <- cbind(Year=year, fileNames, projectXmlURL=projectXmlURL, projectXmlFile=projectXmlFile, stringsAsFactors=FALSE)
		fileNames <- cbind(Year=year, fileNames, stsInfo[atYear, ], stringsAsFactors=FALSE)
		
		fileNames
	}
	
	# Download first all project.xml files to tempfiles:
	stsInfo <- downloadProjectXmlToTemp(stsInfo, dir=NULL)
	
	# Get the cruise info of all years:
	out <- lapply(stsInfo$sampleTime, getCruiseInfoFromStsInfoOneYear, stsInfo)
	out <- do.call(rbind, out)
	out
}

###############################################
##### <<<Internal functions of getNMDdata #####
###############################################

# Function to replace the first occurrence of repeated characters 'char' in a string:
replaceFirstOfRepeatedChar <- function(x, pattern="+", replacement="."){
	reg <- paste0("[", pattern, "]+")
	at <- gregexpr(reg, x)[[1]]
	if(!identical(at, -1)){
		for(i in at){
			substr(x, i, i) <- replacement
		}
	}
	x
}
# Function for extracting URLs for a list of cruises. The conversion from the names given by 'dataSource' to the names given by 'StoX_data_sources' is needed to save the data in the StoX directory structure:
#getCruiseURLs <- function(x, dataSource, StoX_data_sources, ver, API="http://tomcat7.imr.no:8080/apis/nmdapi"){
#	# Use the search function in version 1:
#	# Pick out the first element of 'x', since a list is always returned from getNMDinfo():
#	#message("Searching for cruises...\n")
#	cruiseURL <- papply(dataSource, function(this) searchNMDCruise(cruisenr=x$Cruise, shipname=x$ShipName, dataSource=this, ver=ver), msg="Searching for cruises...")
#	# Convert to a matrix and add colnames:
#	cruiseURL <- do.call(cbind, cruiseURL)
#	colnames(cruiseURL) <- StoX_data_sources
#	# Combine with the cruise info:
#	out <- cbind(x, cruiseURL, stringsAsFactors=FALSE)
#	rownames(out) <- NULL
#	out
#}
getCruiseURLs <- function(cruiseInfo, ver=getRstoxDef("ver"), API="http://tomcat7.imr.no:8080/apis/nmdapi", checkURL=FALSE, return.URL=FALSE){
	# Pick out the first element of 'x', since a list is always returned from getNMDinfo():
	#message("Searching for cruises...\n")
	URL <- searchNMDCruise(cruisenr=cruiseInfo$Cruise, shipname=cruiseInfo$ShipName, dataSource=cruiseInfo$NMD_data_source, ver=ver, API=API, return.URL=return.URL)
	
	# Report files not found:
	areNA_URL <- which(is.na(URL))
	if(checkURL && length(areNA_URL)){
		warning("The following files were not found using the search function of NMD at the latest API for each data source (", paste(names(ver$API), ver$API, sep=": ", collapse=", "), "): ", paste0(areNA_URL, ". ", cruiseInfo$FileName[areNA_URL], collapse=", "))
	}
	
	# Combine with the cruise info:
	out <- data.frame(cruiseInfo, fileURL=URL, searchURL=names(URL), stringsAsFactors=FALSE)
	rownames(out) <- NULL
	out
}
# Function for downloading one cruise:
#downloadOneCruise <- function(i, filePaths, URLs, timeout){
#	downloadOneFile <- function(j, URL, file, timeout){
#		if(!is.na(URL[j])){
#			suppressWarnings(downloadXML(URL[j], msg=FALSE, list.out=FALSE, file=file[j], timeout=timeout))
#		}
#	}
#	
#	# Extract all URLs and file paths of the current cruise in the series:
#	allFilePaths <- filePaths[[i]]
#	allURLs <- URLs[[i]]
#	allFilePaths <- unlist(allFilePaths)
#	allURLs <- unlist(allURLs)
#	
#	# Download all files of the current project:
#	sapply(seq_along(allURLs), downloadOneFile, URL=allURLs, file=allFilePaths, timeout=timeout)
#}
downloadOneCruise <- function(x, timeout){
	downloadOneFile <- function(j, fileURL, file, timeout){
		if(!is.na(fileURL[j])){
			suppressWarnings(downloadXML(fileURL[j], msg=FALSE, list.out=FALSE, file=file[j], timeout=timeout))
		}
	}
	
	# Download all files of the current project:
	sapply(seq_along(x$fileURL), downloadOneFile, fileURL=x$fileURL, file=x$FilePath, timeout=timeout)
}
# Function for downloading cruises:
getCruises <- function(cruiseInfo, downloadType, cruise, StoX_data_sources=NULL, model="StationLengthDistTemplate", dir=NA, subdir=NA, subset=NULL, prefix=NA, year=NA, dataSource=NA, ow=NULL, abbrev=FALSE, timeout=NULL, run=TRUE, return.URL=FALSE, ...){
	
	# First split the cruiseInfo by project ID:
	cruiseInfo <- split(cruiseInfo, cruiseInfo$ProjectID)
	nprojects <- length(cruiseInfo)
	
	# Extract subset:
	subset <- getSubset(subset, nprojects=nprojects, info=cruiseInfo)
	cruiseInfo <- cruiseInfo[subset]
	if(return.URL){
		return(cruiseInfo)
	}
	
	# Define the project names (original and possibly abbreviated):
	projectPathsOrig <- getPaths(downloadType=downloadType, dir=dir, subdir=subdir, name=cruise, prefix=prefix, year=year, cruiseInfo=cruiseInfo, abbrev=FALSE, dataSource=dataSource, StoX_data_sources=StoX_data_sources)$projectPaths
	temp <- getPaths(downloadType=downloadType, dir=dir, subdir=subdir, name=cruise, prefix=prefix, year=year, cruiseInfo=cruiseInfo, abbrev=abbrev, dataSource=dataSource, StoX_data_sources=StoX_data_sources)
	projectPaths <- temp$projectPaths
	cruiseInfo <- temp$cruiseInfo
	if(!run){
		return(projectPaths)
	}
	
	# Create projects and control overwriting:
	# Here the project.xml file paths are looked for in the 'cruiseInfo', and if missing 'process' will be an empty list, resulting in no files added to the project through the 'files' argument of createProject():
	process <- lapply(cruiseInfo, function(x) list(process=x$projectXmlFile[1]))
	#process <- lapply(cruiseInfo, function(x) x$projectXmlFile[1])
	#names(process) <- rep("process", length(process))
	
	temp <- createProject(projectPaths, model=model, ow=ow, files=process, ...)
	suppressWarnings(toWrite <- which(!is.na(temp)))
	if(length(toWrite)==0){
		return()
	}
	projectPaths <- projectPaths[toWrite]
	projectPathsOrig <- projectPathsOrig[toWrite]
	cruiseInfo <- cruiseInfo[toWrite]
	
	# Download for all projects:
	#papply(seq_along(cruiseInfo), downloadOneCruise, filePaths=filePaths, URLs=URLs, timeout=timeout, msg="Downloading...")
	papply(cruiseInfo, downloadOneCruise, timeout=timeout, msg="Downloading...")
	
	# Check whether the files were downloaded. This could have been done by use of the output from download.file (0 for sucsess and positive for failure), but instead we check the existence of the files, and the size:
	checkDownloadedFiles <- function(cruiseInfo){
		valid <- !is.na(cruiseInfo$URL)
	}
	
	URLsFlat <- unlist(lapply(cruiseInfo, "[[", "fileURL"))
	filePathsFlat <- unlist(lapply(cruiseInfo, "[[", "FilePath"))
	valid <- !is.na(URLsFlat)
	filePathsFlat <- filePathsFlat[valid]
	URLsFlat <- URLsFlat[valid]
	
	# Report download failure in a warning:
	downloadSuccess <- file.exists(filePathsFlat) & (file.info(filePathsFlat)$size > 0) %in% TRUE
	# Warning if any downloads failed:
	downloadFailedWarning(URLsFlat, downloadSuccess)
	
	# Report project names if abbreviated:
	if(!all(projectPaths==projectPathsOrig)){
		cat("Project names abbreviated:\n")
	}
		
	# Update the projects if the project.xml file has not been downloaded (linking to the downloaded files) and return the paths:
	if(length(cruiseInfo[[1]]$projectXmlFile) == 0){
		lapply(projectPaths, updateProject)
	}
	
	# Close the projects:
	lapply(projectPaths, closeProject)
	
	return(projectPaths)
}
#' 
#' @importFrom XML xmlParse xmlToList
#' @export
#' @rdname getNMDinfo
#' 
downloadXML <- function(URL, msg=FALSE, list.out=TRUE, file=NULL, method="auto", timeout=NULL){
	failed <- FALSE
	if(file.exists(URL)){
		file <- URL
	}
	else{
		URL <- URLencode(URL)
		# Using rCurl there are recurring encoding problems, where the xml file is interpreted as some other than the UTF-8 encoding specified in the first line of the file (such as latin-1). Thus we test out downloading the file directly using download.file():
		# Download to the temporary file if 'file' is missing:
		if(length(file)==0){
			file <- tempfile()
		}
		# Set the timeout if given and on windows, in which case method needs to be "internal":
		if(.Platform$OS.type == "windows" && length(timeout)){
			old_timeout <- getOption("timeout")
			method <- "internal"
			options(timeout=timeout)
		}
		tryCatch(download.file(URL, destfile=file, quiet=!msg, method=method), error=function(...) failed<<-TRUE)
		# Reset timeout:
		if(.Platform$OS.type == "windows" && length(timeout)){
			options(timeout=old_timeout)
		}
		if(failed){
			warning(paste("Non-existent URL", URLdecode(URL)))
			return(NULL)
		}
	}
	# Convert to a list and output if requested:
	if(list.out){
		# Read the file:
		x <- readChar(file, file.info(file)$size)
		
		# Try to estimate the time left for converting to list, as a multiple of the download time, which is 
		if(msg){
			cat("Converting to list...\n")
			ncharx <- nchar(x)
			# 6 seems to give a closer estimate:
			minNchar <- 5e5
			if(ncharx > minNchar){
				cat("Time left (rough estimate at ", toString(Sys.time()), "): ", signif(4e-6 * ncharx, 2), " seconds\n", sep="")
			}
		}
		
		# Parse the file as XML:
		# 2018-06-04: Added encoding="UTF-8":
		#x <- tryCatch(xmlParse(x, asText=TRUE), error=function(...) failed<<-TRUE)
		x <- tryCatch(XML::xmlParse(x, asText=TRUE, encoding="UTF-8"), error=function(...) failed<<-TRUE)
		if(failed){
			warning(paste("URL" ,URLdecode(URL) ,"does not contain valid XML data (error in xmlParse())"))
			return(NULL)
		}
		else{
			# Convert to list:
			# There is a possibility to speed this process up by x <- xml2::read_xml(file); x <- xml2::as_list(x), which takes 30 vs 50 sec on platform data in NMD API version 2, but the output is a bit different, with .attrs as attributes, and lists even for single valiables, such as x[[1]][[3]]$platformCodes[[2]]$sysname.
			x <- XML::xmlToList(x, simplify=FALSE)
			if(length(x)==0){
				warning(paste("URL" ,URLdecode(URL) ,"does not contain data (xmlToList() returning NULL)"))
				return(NULL)
			}
			
			# Convert to a list in the rare occation that a vector was returned from XML::xmlToList:
			if(!is.list(x)){
				x <- as.list(x)
			}
			
			# New line added on 2016-08-12 after an issue with nordic characters being interpreted as latin1 by R on Windows. The problem is that xmlAttrs() has no parameter for encoding, and, in contrast with the rest of xmlToList(), fails to interpret the data as UTF-8. The solution is to convert all the data afterwards:
			# 2018-06-04: This line contained an error prior to this date (missing "x <- "), rendering the line ineffective:
			x <- rapply(x, function(xx) iconv(xx, "UTF-8", "UTF-8"), how="replace")
		}
		x
	}
	else{
		file
	}
}


#*********************************************
#*********************************************
#' Download a zipped StoX project to a specified project path.
#'
#' @param URL			The URL of the zipped project.
#' @param projectName   The name or full path of the project, a baseline object (as returned from \code{\link{getBaseline}} or \code{\link{runBaseline}}, og a project object (as returned from \code{\link{openProject}}).
#' @param projectRoot	The root directory of the project in which to save the downloaded files (set this if you wish to place the files in a project specified by its name, and not in the default root directory).
#' @param cleanup		Logical: if FALSE, the downloaded zip file is not deleted.
#' @param ow,msg		See \code{\link{getow}}.
#' @param onlyone   	Logical: If TRUE, only one project is checked (no for loop).
#'
#' @export
#' @keywords internal
#' @rdname downloadProjectZip
#'
downloadProjectZip <- function(URL, projectName=NULL, projectRoot=NULL, cleanup=TRUE, ow=TRUE, msg=TRUE, onlyone=TRUE){
	# Get the project path. If 'projectName' is not given, set this to a temporary name, and use the project name stored in the zip file. If the project path is given in 'projectName', all is good:
	if(length(projectName)==0){
		projectPath <- getProjectPaths(projectName="temporaryZipDownload", projectRoot=projectRoot)$projectPath
	}
	else{
		projectPath <- getProjectPaths(projectName=projectName, projectRoot=projectRoot)$projectPath
	}
	
	# Declare the output 'ow':
	output_ow <- ow
	
	# Define the path to the downloaded zip file:
	zipPath <- paste0(projectPath, ".zip")
	
	# Download the zip file, overwriting any existing file with the path 'zipPath'. Added mode="wb" to make the zip openable on Windows:
	# Treat overwriting before downloading if the projectName was given:
	if(length(projectName)){
		if(file.exists(projectPath)){
			temp <- getow(ow, projectPath, onlyone=onlyone, msg=msg)
			output_ow <- temp$ow
			# Return from the function if not overwriting:
			if(temp$jumpToNext){
				# Added appropriate return value as per notice from Ibrahim on 2018-02-05 (changed from FALSE to 1 (see the Value section of ?download.file) on 2018-03-01):
				return(list(downloadSuccess = FALSE, ow = output_ow))
			}
		}
	}
	# Download the file and record whether it was a success or failure by a logical variable for clearity (and not as an integer as returned by download.file()):
	downloadSuccess <- download.file(URL, zipPath, mode="wb") == 0

	# Get the path of the unzipped file:
	ziplist <- unzip(zipPath, list=TRUE)[,1]
	if(dirname(ziplist[1])!="."){
		unzipPath <- file.path(dirname(zipPath), dirname(ziplist[1]))
	}
	else{
		unzipPath <- file.path(dirname(zipPath), dirname(ziplist[2]))
	}
	# Rename the projectPath to the unzipdir if projectName was not given:
	if(length(projectName)==0){
		projectPath <- unzipPath
		# Treat overwriting:
		if(file.exists(projectPath)){
			temp <- getow(ow, projectPath, onlyone=onlyone, msg=msg)
			output_ow <- temp$ow
			# Return from the function if not overwriting:
			if(temp$jumpToNext){
				# Added appropriate return value as per notice from Ibrahim on 2018-02-05:
				return(list(downloadSuccess = FALSE, ow = output_ow))
			}
		}
	}
	
	# Unzip the downloaded zip file:
	unzip(zipPath, exdir=dirname(zipPath))
	
	# Delete zipPath, and if not equal, delete the projectPath and rename unzipPath:
	if(length(projectName) && !identical(projectPath, unzipPath)){
		# Delete the existing project:
		unlink(projectPath, recursive=TRUE)
		file.rename(unzipPath, projectPath)
	}
	if(cleanup){
		unlink(zipPath)
	}
	# Return download success:
	list(downloadSuccess=downloadSuccess, projectPath=projectPath, ow = output_ow)
}



# Function for adding the queries to an URL (starting with "?"):
addQuery <- function(URL, ...){
	l <- list(...)
	# Keep only those with positive length:
	l <- l[sapply(l, length) > 0]
	if(length(l)){
		query <- paste(names(l), l, sep="=", collapse="&")
		paste(URL, query, sep="?")
	}
	else{
		URL
	}
}
# Get the URL base string:
getURLbase <- function(ver, API, dataSource, model=NULL, dataset=NULL, unnamed=NULL){
	# Make sure that 'datasource' is character, to avoid confusion when selecting the API version below:
	dataSource <- as.character(dataSource)
	
	version <- paste0("v", ver$API[[dataSource]])
	query <- paste0("version=", ver[[dataSource]])
	if(length(model)){
		model <- paste("model", model, sep="/")
	}
	if(length(dataset)){
		dataset <- paste("dataset", dataset, sep="/")
	}
	out <- apply(cbind(API, dataSource, version, unnamed, model, dataset), 1, paste, collapse="/")
	# Add the query of dataSource version:
	if(length(ver)){
		out <- addQuery(out, version=ver[[dataSource]])
	}
	out
}
# Function to extract the variables given in the levels[[2]] in the list elements named by levels[[1]]
getElements <- function(data, levels=list("element", c("text", ".attrs")), data.frame.out=TRUE, equalLen=NULL){
	# Example from 'equipment', read using 
	#   eV2 <- downloadXML("http://tomcat7.imr.no:8080/apis/nmdapi/reference/v2/dataset/equipment?version=2.0", msg=TRUE)
	#   getElements(eV2, levels=list("row", NA))
	# :
		# $row
		# $row$code
		# [1] "3183"
    	# 
		# $row$name
		# [1] "Torsketrål 135"
    	# 
		# $row$area
		# [1] "0"
    	# 
		# $row$description
		# [1] "Torsketrål 135 mm maskevidde, uten rist."
    	# 
		# $row$.attrs
		#                       type                 deprecated                updatedTime                  updatedBy               insertedTime                 insertedBy 
		#     "EquipmentElementType"                    "false" "2018-06-11T07:16:20.072Z"                   "import" "2018-06-11T07:16:20.072Z"                   "import" 
		# attr(,"namespaces")
		# http://www.w3.org/2001/XMLSchema-instance                                                                                                                               
		#                                     "xsi"                                        ""                                        ""                                        "" 
	
	if(length(data)==0){
		return(data)
	}
	# Extract the elements given in the second element of 'levels' for all elements of 'data' named by the first element of 'levels':
	if(length(levels)==2){
		# The second element of 'levels' (the column specifications) can be given as NA, impying all columns:
		if(is.na(levels[[2]][1])){
			#levels[[2]] <- names(data[[levels[[1]]]])
			# Function for extracting one row of data, stored as a list where one of the list elements contain the attributes:
			getOneRow <- function(x){
				d <- x[names(x) != ".attrs"]
				d <- NAunlist(d)
				attrs <- unlist(x$.attrs)
				c(d, attrs)
			}
			
			# Extract the rows:
			data <- lapply(data[names(data)==levels[[1]]], getOneRow)
			# Any one of the rows has length differing from the rest, delete this row (hereby we require that all variables are filled):
			s <- sapply(data, length)
			equalLengths <- s==max(s)
			if(!all(equalLengths)){
				if(length(equalLen)){
					warning(paste0("Unequal lengths of the rows: ", equalLen))
					data <- data[equalLengths]
				}
				else{
					return(data)
				}
			}
			data <- as.data.frame(do.call(rbind, data), stringsAsFactors=FALSE)
		}
		else{
			# The variables can be given using '$', so we split those variable names here, "[[" recombines with '$' to get the requested elements:
			levels[[2]] <- lapply(levels[[2]], function(x) if(grepl("$", x, fixed=TRUE)) strsplit(x, "$", fixed=TRUE)[[1]] else x)
			# Extract the elements given the variables specified in level[[2]]:
			data <- lapply(levels[[2]], function(x) NAsapply(data[names(data)==levels[[1]]], "[[", x))
			# Make sure that if several elements were read into a matrix, these are transposed (since sapply() rbinds instead of cbind):
			areMatrices <- sapply(data, function(x) length(dim(x))==2)
			if(any(areMatrices)){
				data[areMatrices] <- lapply(data[areMatrices], t)
			}
		
			# Define the names of the columns of the data frame. Here we fill inn the colnames of the individual matrices (some data are read as matrices above):
			dataNames <- levels[[2]]
			if(any(areMatrices)){
				dataNames[areMatrices] <- lapply(data[areMatrices], colnames)
			}
			dataNames <- unlist(dataNames)
		
			# Combine into a data frame:
			data <- as.data.frame(data, stringsAsFactors=FALSE)
			names(data) <- dataNames
		}
		# Remove row names:
		rownames(data) <- NULL
		
		# Return a simple vector if data.frame.out is FALSE:
		if(!data.frame.out){
			data <- unlist(data, use.names=FALSE)
		}
	}
	else if(length(levels)==1){
		data <- NULL
	}
	data
}
# Function to extract the element named by 'value' and with name given by 'name', in the elements given in rows (row > element > value, with name row > element > name)
getRowElementValueWithName <- function(data, row="row", element="element", value="text", name=".attrs"){
	# Example from search in version 2, read by
	#   out <- downloadXML(URLencode("http://tomcat7.imr.no:8080/apis/nmdapi/biotic/v2/find?cruisenr=2016114&shipname=G.O.Sars"), msg=FALSE)
	# :
		# $row
		# $row$element
		# $row$element$text
		# [1] "LMEL"
    	# 
		# $row$element$.attrs
		#         name 
		# "callsignal" 
    	# 
    	# 
		# $row$element
		# $row$element$text
		# [1] "2016114"
    	# 
		# $row$element$.attrs
		#     name 
		# "cruise" 
	
	getOneRow <- function(x){
		out <- lapply(x[names(x)==element], "[[", value)
		names(out) <- lapply(x[names(x)==element], "[[", name)
		out
	}
	# Get the rows and convert to data.frame:
	out <- lapply(data[names(data)==row], getOneRow)
	out <- as.dataFrame_full(out)
	out
}
# Simple function for extracting the elements named by the input parameter 'value' from the elements named by the input parameter 'element':
getElementsValue <- function(data, element="element", value="text"){
	out <- lapply(data[names(data)==element], "[[", value)
	unlist(out, use.names=FALSE)
}
# Function extracting the year from a vector of cruise numbers:
getYearFromCruiseNumber <- function(x){
	if(any(grepl("_", x))){
		strsplit(x, "_")[[1]][2]
	}
	else{
		substr(x, 1, 4)
	}
}
# Function for converting from UNIX time to date:
toDate <- function(x, tz=""){
	if(is.numeric(x)){
		out <- as.POSIXct(as.vector(x), origin="1970-01-01", tz=tz)
	}
	as.Date(out, tz=tz)
}

# Versions of lapply() and unlist() where missing values are replaced by NA:
NAsapply <- function(y, fun, ...){
	out <- lapply(y, fun, ...)
	empty <- sapply(out, length)==0
	if(any(empty)){
		out[empty] <- as.list(rep(NA, sum(empty)))
	}
	sapply(out, function(x) x)	
}
NAunlist <- function(y){
	empty <- sapply(y, length) == 0
	if(any(empty)){
		y[empty] <- as.list(rep(NA, sum(empty)))
	}
	unlist(y)
}
# Function for converting a column to a list with names given by another column:
column2ilst <- function(x, col, colnames){
	out <- as.list(x[[col]])
	names(out) <- gsub(" ", "_", x[[colnames]])
	out
}
# Convert to a data frame:
as.numericDataFrame <- function(data){
	data <- as.data.frame(data, stringsAsFactors=FALSE)
	convertableToNumeric <- function(x, not=c("POSIXct", "POSIXt")){
		!class(x) %in% not && !any(is.na(as.numeric(x[!is.na(x)])))
	}
	# Convert all numeric columns to numeric, identified by no NAs when converting:
	#suppressWarnings(data <- lapply(data, function(x) if(!any(is.na(as.numeric(x[!is.na(x)])))) as.numeric(x) else x))
	suppressWarnings(data <- lapply(data, function(x) if(convertableToNumeric(x)) as.numeric(x) else x))
	data <- as.data.frame(data, stringsAsFactors=FALSE)
}





#*********************************************
#*********************************************
#' Search for a cruise given the cruise number and ship name.
#'
#' The NMD API enables searching for a cruise identifyer given the cruise number and ship name.
#'
#' @param cruisenrANDshipname	A vector of two elements, the first being the cruise number and the second the ship name.
#' @param dataSource				The type of data requested. Currently implemented are "echosunder" and "biotic", while "landing" and "ctd" are in the pipeline. dataSource=NULL (default) returns all possible data.
#' @param ver					The version of the API. As of 2015-05 only version 1 is available. Version 2 will include the possibility to return a list of all cruises.
#' @param API					The path to the API.
#'
#' @export
#' @importFrom tools file_ext
#' @keywords internal
#' 
searchNMDCruise <- function(cruisenr, shipname=NULL, dataSource="biotic", ver=getRstoxDef("ver"), API="http://tomcat7.imr.no:8080/apis/nmdapi", return.URL=FALSE){
	
	# Function for extacting the URL of the cruise. In version 1 this URL was given directly, whereas in version 2 it has to be constructed from the downloaded table:
	findCruiseURL <- function(ind, cruisenr, shipname, dataSource, API, ver=getRstoxDef("ver")){
		
		# Convenience function for building the search URL:
		getSearchURL <- function(ind, cruisenr, shipname, API, dataSource, ver){
			paste(
				API, 
				dataSource[ind], 
				paste0("v", ver$API[[dataSource[ind]]]), 
				paste0("find?cruisenr=", cruisenr[ind], "&shipname=", shipname[ind]), 
				sep="/"
			)
		}
		
		# Make sure that 'datasource' is character, to avoid confusion when selecting the API version below:
		dataSource <- as.character(dataSource)

		# Get the search URL:
		searchURL <- getSearchURL(ind=ind, cruisenr=cruisenr, shipname=shipname, API=API, dataSource=dataSource, ver=ver)
		#paste(
		#	API, 
		#	dataSource[ind], 
		#	paste0("v", ver$API[[dataSource[ind]]]), 
		#	paste0("find?cruisenr=", cruisenr[ind], "&shipname=", shipname[ind]), 
		#	sep="/"
		#)
		
		# Download the result from the search query:
		suppressWarnings(out <- downloadXML(URLencode(searchURL), msg=FALSE))
		
		# Insert . for any ship names containing "+", which is an indicator that the ship name originates from file names generated by NMD, in which spaces and dots are both replaced by "+". There are some occations of ". " replaced by "++" (e.g., "M. Ytterstad"), and even one "  " replaced by "++" ("H  Larsen"). So the following reaplacement of first occurrence of possibly repeated "+" by "." must happen after the search string has failed:
		# If version 2 or up try replaceing the first occurrences of possibly repeated "+" by ".":
		if(length(out)==0 && ver$API[[dataSource[ind]]] >= 2){
			shipname[ind] <- replaceFirstOfRepeatedChar(shipname[ind])
			searchURL <- getSearchURL(ind=ind, cruisenr=cruisenr, shipname=shipname, API=API, dataSource=dataSource, ver=ver)
			suppressWarnings(out <- downloadXML(URLencode(searchURL), msg=FALSE))
		}
		
		if(return.URL){
			return(searchURL)
		}
		
		if(length(out)==0){
			out <- NA
			names(out) <- searchURL
			return(out)
		}
		if(ver$API[[dataSource[ind]]] == 1){
			# Version 1 returns the cruise URL directly:
			out <- out$element$text
		}
		#else if(ver$API[[dataSource[ind]]] == 2){
		else if(ver$API[[dataSource[ind]]] >= 2){
			# In version 2 the elements of the cruise URL are given and must be combined to get the URL:
			g <- getRowElementValueWithName(out, row="row", value="text", name=".attrs")
			# Build the URL:
			relativePath <- g$path
			APIverString <- paste0("v", ver$API[[dataSource[ind]]])
			query <- paste0("version=", ver[[dataSource[ind]]])
			out <- paste(API, dataSource[ind], APIverString, relativePath, "dataset", sep="/")
			# Add dataSource version:
			out <- paste(out, query, sep="?")
		}
		else{
			stop("Invalid version")
		}
		
		names(out) <- searchURL
		out
	}
	
	ver <- getNMDver(ver)
	
	# If there are slashes in the cruisenr, assume it is a file name:
	if(grepl("/", cruisenr) || tolower(tools::file_ext(cruisenr)) == "xml"){
		x <- extractCruiseAndShipame(cruisenr)
		shipname <- x$ShipName
		dataSource <- x$NMD_data_source
		cruisenr <- x$Cruise
	}
	
	
	### # Add support for giving 'ver' as a single numeric, as was done prior to Rstox_1.10:
	### if(!is.list(ver)){
	### 	ver <- list(API=ver[1])
	### }
	
	if(length(cruisenr)==2 && length(shipname)==0){
		shipname <- cruisenr[2]
		cruisenr <- cruisenr[1]
	}
	
	# 2018-12-03: It was discrovered that the 
	
	
	# Get the URLs:
	out <- sapply(seq_along(cruisenr), findCruiseURL, cruisenr, shipname, dataSource, API, ver)

	out
}
#*********************************************
#*********************************************
#' Encodes and decodes NMD API strings.
#'
#' \code{getNMDinfo} converts, prints and optionally returns NMD reference information given a search string to the reference information. Used in StoX.URL(). \cr \cr
#' \code{getNMDdata} downloads data from specific cruises, cruise series ot survey time series from NMD. \cr \cr
#' \code{downloadXML} downloads xml data from an API, parses the xml data, and converts to a list (the latter is time consuming). \cr \cr
#'
#' @param URL	An URL.
#' 
#' @export
#' @keywords internal
#' 
NMDdecode <- function(URL){
	URL <- URLdecode(URL)
	URL <- strsplit(URL, "/")[[1]]
	# Remove empty strings past the first (the http://):
	URL <- URL[!(URL=="" & duplicated(URL))]
	# Extract the API and data type:
	API <- paste(URL[1:5], collapse="/")
	dataSource <- URL[6]
	ver <- URL[7]
	year <- NA
	vessel <- NA
	cruise <- NA
	cs <- NA
	sts <- NA
	type <- NA
	if(dataSource %in% c("echosounder", "biotic", "cruise")){
		missiontype <- URL[8]
		year <- URL[9]
		vessel <- URL[10]
		cruise <- URL[11]
	}
	else if(dataSource=="cruiseseries"){
		if(length(URL)==8){
			cs <- URL[8]
		}
	}
	else if(dataSource=="surveytimeseries"){
		if(length(URL)==8){
			sts <- URL[8]
		}
	}
	else if(dataSource=="reference"){
		if(length(URL)==8){
			type <- URL[8]
		}
	}
	list(API=API, dataSource=dataSource, ver=ver, year=year, vessel=vessel, cruise=cruise, cs=cs, sts=sts, type=type)
}


#*********************************************
#*********************************************
#' Abbreviates StoX project names.
#'
#' @param x			A vector of strings.
#' @param p			The exponent in the abbreviation function. The number of characters remaining (n) after possibly capital letters and numbers have been fixed in the abbreviated string, are reduced in number according to round(n^p).
#' @param collapse	The character to separate words by.
#' @param keep		Keywords for what to keep in the abbreviated strings. Including "punct" ensures that punctuation characters are left untouched, but still considered as separators between words.
#' @param sub		A value to add to the number of characters in a single abbreviated word. Set this to say -1 to remove wery whort words such as "in".
#' 
#' @examples
#' cs <- getNMDinfo("cs", recursive = FALSE)
#' sts <- getNMDinfo("sts", recursive = FALSE)
#' abbr <- list(cbind(cs, abbrevWords(cs)), cbind(sts, abbrevWords(sts)))
#' abbr
#' lapply(abbr, nchar)
#' 
#' @export
#' @keywords internal
#' 
abbrevWords <- function(x, p=1/2, collapse="_", keep=c("capital", "numeric", "punct"), sub=0, abbrev=TRUE){
	# Return immediately if abbrev=FALSE:
	if(!abbrev){
		return(x)
	}
	
	# Function for abbreviating one single word:
	abbrevSingle <- function(singleWord, keep=c("cap", "num"), p=1/2, sub=0){
		# Get capital letters and digits:
		tokeep <- NULL
		if(any(startsWith(tolower(keep), "c"))){
			tokeep <- c(tokeep, unlist(gregexpr("[A-Z]", singleWord, perl=TRUE)))
		}
		if(any(startsWith(tolower(keep), "n"))){
			tokeep <- c(tokeep, unlist(gregexpr("[[:digit:]]", singleWord, perl=TRUE)))
		}
		# Allow for no matches to capital letters and digits:
		tokeep <- tokeep[tokeep>0]
		# Get the number of characters to keep from the rest:
		all <- seq_len(nchar(singleWord))
		rest <- setdiff(all, tokeep)
		nrest <- length(rest)
		# Apply the exponent 'p', and restrict to (0, nrest):
		keeprest <- round(nrest^p) + sub
		keeprest <- max(0, keeprest)
		keeprest <- min(nrest, keeprest)
		tokeep <- sort(c(tokeep, rest[seq_len(keeprest)]))
		# Merge the characters to keep:
		paste0(strsplit(singleWord, "", fixed=TRUE)[[1]][tokeep], collapse="")
	}
	
	# Function for abbreviating a string of words separated by space or punctuation characters:
	abbrevString <- function(string, p = 1/2, collapse="_", keep=c("capital", "numeric", "punct"), sub=0){
		# First get the puntuation characters, and keep those:
		atpunkt <- gregexpr("[[:punct:]]", string, perl=TRUE)[[1]]
		punkt <- regmatches(string, list(atpunkt))[[1]]
		if(all(atpunkt == -1)){
			atpunkt <- NULL
		}
		atspace <- gregexpr("[[:space:]]", string, perl=TRUE)[[1]]
		if(all(atspace == -1)){
			atspace <- NULL
		}
		suppressWarnings(atall <- sort(c(atspace, atpunkt)))
		atall <- list(
			c(1, atall+1), 
			c(atall-1, nchar(string))) # substring deals well with values outside of seq_len(nchar(string)), returning "" at these positions
		
	
		# Define the collapsing characters, and insert the punktuation characters:
		collapse <- c(rep(collapse, length(atpunkt) + length(atspace)), "")
		if(any(startsWith(tolower(keep), "p")) && length(atpunkt)){
			atpunkt <- rank(c(atpunkt, atspace))[seq_along(atpunkt)]
			collapse[atpunkt] <- punkt
		}
	
		# Split by space and possibly punct.
		s <- substring(string, atall[[1]], atall[[2]])
		#s <- unlist(strsplit(string, "[[:space:]]"))
		#if(any(startsWith(tolower(keep), "p"))){
		#	s <- unlist(strsplit(s, "[[:punct:]]"))
		#}
	
		# Abbreviate each word:
		s <- sapply(s, abbrevSingle, keep=keep, p=p, sub=sub)
		# 
		#zeroLength <- nchar(s) == 0
		#if(any(zeroLength)){
		#	s <- s[!zeroLength]
		#	collapse <- collapse[!zeroLength]
		#}
		# Paste and return:
		paste0(s, collapse, collapse="")
	}
	
	# Abbreviate or return the input if 
	return(unlist(lapply(x, abbrevString, p=p, collapse=collapse, keep=keep, sub=sub), use.names=FALSE))
	### s <- strsplit(x, " ")[[1]]
	### n <- round(nchar(s)^p) + 
	### 	if(keepUpper) sapply(regmatches(s, gregexpr("[A-Z]", s, perl=TRUE)), length) + 
	### 	if(keepNumeric) sapply(regmatches(s, gregexpr("[0-9]", s, perl=TRUE)), length)
	### s <- unlist(lapply(seq_along(s), function(i) substr(s[i], 1, n[i])))
	### paste(s, collapse=collapse)
}

#' Composing a single year Survey Time Series from scratch
#'
#' \code{NMDcomposeSTS} returns the created StoX project path.
#'
#' @param STSname Name of the survey time series. Must be a character vector.
#' @param year Year of the survey time searies. Must be a character vector.
#' @param APIversion Placeholder, not used yet (see below TODO).
#' @param DATAversion Placeholder, not used yet (see below TODO).
#'
#' @return A character vector containing the newly created StoX project directory
#'
#' @examples
#' \dontrun{
#' NMDcomposeSTS("Barents Sea Beaked redfish and Sebastes sp in Subareas
#'	I and II bottom trawl index in winter", "2017")
#' }
#' @export
#' @importFrom utils tail
#' @importFrom XML xmlNamespaceDefinitions getNodeSet
NMDcomposeSTS <- function(STSname, year, APIversion = NULL, DATAversion = NULL) {
	# TODO: Versioning and dynamic API path and data version
 	# Getting the source cruise
	getSource <- function(stsRef, STSname) {
		return (stsRef[[STSname]]$cruiseSeries$cruiseSeries$cruiseSeries)
	}
 	# Extract ship name and cruise
	extract <- function(node, type) {
		fileName <- tools::file_path_sans_ext(basename(node))
		tmp <- utils::tail(strsplit(fileName, "[_]")[[1]], 2)
		url <- searchNMDCruise(tmp[[1]], tmp[[2]], type)[[1]]
		# For G.O. Sars, (G+O+Sars to G.O.Sars)
		if(is.na(url)) {
			tmp[[2]] <- gsub("[+]", ".", tmp[[2]])
			url <- searchNMDCruise(tmp[[1]], tmp[[2]], type)[[1]]
		}
		return(url)
	}
 	# Get STS ref
	stsRef <- xmlToList("http://tomcat7.imr.no:8080/apis/nmdapi/reference/v2/dataset/surveytimeseries/?version=2.0")
	names(stsRef) <- lapply(stsRef, function(x) x$name)
 	# Getting the name of the source cruise for the picked STS (NB: Not used as for now, it's not accurate?)
	sourceCruise <- getSource(stsRef, STSname)
 	# Getting the StoX project file
	stoxProjectName <- lapply(stsRef[[STSname]]$cruiseSeries$cruiseSeries$samples, function(x) if(x$sampleTime==year) return(x$stoxProject) else return(NULL))
	stoxProjectName <- unlist(stoxProjectName)
 	if(length(stoxProjectName) < 1) {
		message("Survey Time Series not found, please check the name and/or the year again.")
		return(NA)
	}
 	downloadStox <- paste0("http://tomcat7.imr.no:8080/apis/nmdapi/stox/v1/", stoxProjectName)
 	# Parse the project file to get the file names
	stoxProjectXML <- xmlParse(downloadStox)
	nsDefs = xmlNamespaceDefinitions(stoxProjectXML)
	ns = structure(sapply(nsDefs, function(x) x$uri), names = names(nsDefs))[[1]]
 	# Extract names
	bioticFiles <- sapply(getNodeSet(stoxProjectXML, "/x:project/x:model/x:process[@name='ReadBioticXML']/x:parameter[contains(@name,'FileName')]", c("x"=ns)), xmlValue)
	acousticFiles <- sapply(getNodeSet(stoxProjectXML, "/x:project/x:model/x:process[@name='ReadAcousticXML']/x:parameter[contains(@name,'FileName')]", c("x"=ns)), xmlValue)
 	# Get the URLs
	downloadBiotic <- sapply(bioticFiles, extract, "biotic")
	downloadAcoustic <- sapply(acousticFiles, extract, "echosounder")
 	# Create structure
	tmpDir <- tempfile(pattern="dir")
	dirStruct <- c("output", "process", "input/biotic", "input/acoustic")
	lapply(paste0(tmpDir, "/", dirStruct), dir.create, recursive = TRUE)
 	# Download files
	tmp <- t(data.frame(c(bioticFiles, acousticFiles)))
	tmp <- cbind(tmp, t(data.frame(c(downloadBiotic, downloadAcoustic))))
	for (i in 1:nrow(tmp)) {
		download.file(URLencode(tmp[i,2]), paste0(tmpDir, "/", tmp[i,1]))
	}
	download.file(URLencode(downloadStox), paste0(tmpDir, "/process/project.xml"))
	return(tmpDir)
}
