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
#' @param ver					The version of the API. As of 2015-05 only version 1 is available. Version 2 will include the possibility to return a list of all cruises.
#' @param API					The path to the API.
#' @param recursive				Logical, special for type \%in\% c("cs","sts"); if FALSE only the list of cruise series or survey time series is returned.
#' @param msg					Logical: if TRUE a message is printed to the consolle stating the estimated time left for the funciton.
#' @param simplify				Logical: if TRUE simplify the data into matrices instead of complicated lists in some cases like taxa.
#' @param cruise				Either the code of a cruise, such as "2015116", or the full or short name of a cruise series or survey time series. In the current version, if given as a cruise code, the parameter 'shipname' must be given as well, based on which the path to the cruise is searched for by functionallity provided by NMD. For cruises prior to the year 1995 several vessels can be linked to the same cruise code, and as of version 2 the user will by default be asked to specify which vessel(s) to specify the vessels when this occurs, instead of having to specify the cruise initially.
#' @param year					Used in conjunction with 'shipname' to get all cruises from one or more years from a specific ship.
#' @param shipname				Specifies the ship name WITHOUT call signal, e.g., "G.O.Sars" and not "G.O.Sars_LMEL" (see 'cruise' and 'year').
#' @param serialno				A vector of the requested serial numbers.
#' @param tsn					The species code for downloading a specific species. See the examples for how to get the \code{tsn} of a species.
#' @param datatype				The type of data requested. Currently implemented are "echosunder" and "biotic", while "landing" and "ctd" are in the pipeline. datatype=NULL (default) returns all possible data.
#' @param dir					The path to the directory in which to place the StoX project holding the downloaded data, or TRUE indicating that a sub directory should be created in which to put mulpitle with the name of the in which to put the downloaded projects
#' @param subdir				Either a name of the sub directory in which to put the StoX projects of downloaded data, or TRUE which puts all projects in a sub folder named after the cruise series or survey time series. 
#' @param group					Specifies how to gruop the data: (1) If given as "year", the data are split into years, and one StoX project is saved for each year; (2) if given as "cruise", one Stox project is generated for each cruise, and (3) group is NULL, all data are saved in one StoX project. The default "default" groups by years if several cruises are requested and by cruise otherwise.
#' @param abbrev				Logical: If TRUE, abbreviate the project names. PArticularly useful when downloading survey time series, which can have long names.
#' @param subset				An integer vector giving the subset of the cruises to download in a cruise series (1 meaning the first cruise and c(2,5) cruise nr 2 and 5).
#' @param filebase				The prefix to use in the names of the StoX projects to which the data are downloaded.
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
#' @export
#' @rdname getNMDinfo
#' 
getNMDinfo <- function(type=NULL, ver=list(API="2", reference="2.0"), API="http://tomcat7.imr.no:8080/apis/nmdapi", recursive=TRUE, msg=FALSE, simplify=TRUE){
	if((!is.list(ver) && ver==1) || (is.list(ver) && ver$API==1)){
		getNMDinfoV1(type=type, ver=1, API=API, recursive=recursive, msg=msg, simplify=simplify)
	}
	else if(is.list(ver) && ver$API==2){
		getNMDinfoV2(type=type, ver=ver, API=API, recursive=recursive, msg=msg, simplify=simplify)
	}
	else{
		warning("Version 2 of the NMD API is the latest available. Version 1 is no longer maintained (as of july 2018).")
		NULL
	}
}
#' 
#' @export
#' @rdname getNMDinfo
#' 
getNMDinfoV1 <- function(type=NULL, ver=1, API="http://tomcat7.imr.no:8080/apis/nmdapi", recursive=TRUE, msg=FALSE, simplify=TRUE){
	###############################
	##### Internal functions>>> #####
	# Function used for extracting a data frame of the cruises used in a cruise series:
	getCruiseSeriesCruises <- function(x, URLbase, name="Cruises", msg=FALSE){
		this <- downloadXML(paste(URLbase, x, sep="/"), msg=msg)$Sample
		# Get years and repeat by the number of cruises for each year
		years <- unname(sapply(this, "[[", ".attrs"))
		nCruisesPerYear <- sapply(this, function(xx) length(xx$Cruises))
		years <- rep(years, nCruisesPerYear)
		CruiseShipName <- as.matrix_full(unlist(lapply(this, "[[", name), use.names=FALSE, recursive=FALSE))
		as.data.frame(cbind(year=years, Cruise=CruiseShipName[,1], ShipName=CruiseShipName[,2]), stringsAsFactors=FALSE)
	}
	# Function used for extracting a data frame of the StoX projects used in a survey time series:
	getSurveyTimeSeriesProjects <- function(x, URLbase){
		this <- downloadXML(paste(URLbase, x, sep="/"), msg=msg)$Sample
		as.data.frame(as.matrix_full(this), stringsAsFactors=FALSE)
	}
	# Function for extracting the platform information from the NMD platform data structure:
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
	# Convert to a data frame:
	asNumericDataFrame <- function(data){
		data <- as.data.frame(data, stringsAsFactors=FALSE)
		# Convert all numeric columns to numeric, identified by no NAs when converting:
		suppressWarnings(data <- lapply(data, function(x) if(!any(is.na(as.numeric(x)))) as.numeric(x) else x))
		data <- as.data.frame(data, stringsAsFactors=FALSE)
	}
	##### <<<Internal functions #####
	###############################
	
	####################################################
	# Get the list of reference data types:
	if(length(type)==0){
		URLbase <- paste(API, "reference", paste0("v", ver), sep="/")
		# Get the list of cruise series:
		data <- downloadXML(URLbase, msg=msg)
		data <- unname(sapply(data[names(data)=="element"], "[[", "text"))
		return(data)
	}
	##### Treat the requested type of information>>> #####
	# A string requested to the API consists of the following parts:
	# 1. The API specific string: "http://tomcat7.imr.no:8080/apis/nmdapi"
	# 2. The datatype: "cruise", "reference", "biotic", "echosounder", "stox", "cruiseseries", "surveytimeseries", where the latter two may be moved to the reference data
	# 3. Version: Such as 1 (current as of 2016-05) or 2
	type[1] <- tolower(type[1])
	vesseltype <- FALSE
	if(length(intersect(type, c("v", "vessel")))){
		type <- "platform"
		vesseltype <- TRUE
	}
	#if(length(intersect(type, c("c", "cruises")))){
	#	datatype <- "echosounder"
	#	type <- NA
	#}
	#else if(length(intersect(type, c("v", "vessel")))){
	#	datatype <- "reference"
	#	type <- "platform"
	#	vesseltype <- TRUE
	#}
	#else{
	#	datatype <- "reference"
	#}
	##### <<<Treat the requested type of information #####
	####################################################
	
	################################
	##### Get the information>>> #####
	# Get the list of cruises:
	#else 
	if(type[1] %in% c("c", "cruises")){
		warning("Returning a list of cruises will be implemented in version 2")
	}
	# Get the list of cruises series with cruises for each series:
	else if(type[1] %in% c("cs", "cruiseseries")){
		URLbase <- paste(API, "cruiseseries", paste0("v", ver), sep="/")
		# Get the list of cruise series. The cruise series name can be given exactly as the second element of 'type':
		if(length(type)==2){
			data <- type[2]
			recursive <- TRUE
		}
		else{
			data <- downloadXML(URLbase, msg=msg)
			data <- unlist(data[names(data)=="element"], use.names=FALSE)
		}
		if(recursive){
			namesdata <- data
			data <- lapply(data, getCruiseSeriesCruises, URLbase=URLbase, msg=msg)
			names(data) <- namesdata
		}
	}
	# Get the list of survey time series with StoX projets for each series:
	else if(type[1] %in% c("sts", "surveytimeseries")){
		URLbase <- paste(API, "surveytimeseries", paste0("v", ver), sep="/")
		# Get the list of survey time series. The survey time series name can be given exactly as the second element of 'type':
		if(length(type)==2){
			data <- type[2]
			recursive <- TRUE
		}
		else{
			data <- downloadXML(URLbase, msg=msg)
			data <- unlist(data[names(data)=="element"], use.names=FALSE)
		}
		if(recursive){
			namesdata <- data
			data <- lapply(data, getSurveyTimeSeriesProjects, URLbase=URLbase)
			names(data) <- namesdata
			## Unlist if only one element is returned (occurs when only one survey time series exists or is requested by the used through a second string in 'type'):
			#if(length(data)==1){
			#	data <- data[[1]]
			#}
		}
	}
	else{
		# Get the available reference data types:
		URLbase <- paste(API, "reference", paste0("v", ver), sep="/")
		# Get the list of references:
		ref <- downloadXML(URLbase, msg=msg)
		ref <- unname(sapply(ref[names(ref)=="element"], "[[", "text"))
		if(length(type)==0){
			return(ref)
		}
		
		# Match the 'type' with the reference data available:
		type <- ref[tolower(ref) == type[1]]
		
		# Download the reference data:
		URLbase <- paste(API, "reference", paste0("v", ver), type[1], sep="/")
		data <- downloadXML(URLbase, msg=msg)
		
		# Simplify the data:
		if(simplify){
			if(length(data)){
				# Remove elements with length 1, indicating time stamps and the like:
				data <- data[sapply(data, length)>1]
				
				# 
				if("element" %in% names(data)){
					if("text" %in% names(data[[1]])){
						data <- sapply(data[names(data)=="element"], "[[", "text")
					}
					else{
						data <- as.matrix_full(data[names(data)=="element"])
					}
					data <- asNumericDataFrame(data)
				}
				# Special case for platform:
				else if("platform" %in% names(data)){
					data <- platformExtract(data)
					if(vesseltype){
						# Changed to extracting all info from the latest velidTo:
						extractLatestValidTo <- function(data){
							if(length(data$validTo)==0){
								return(head(data, 1))
							}
							latestValidTo <- tail(sort(data$validTo), 1)
							equalToLatestValidTo <- data$validTo==latestValidTo
							as.data.frame(t(apply(data[equalToLatestValidTo,], 2, function(x) head(x[!is.na(x)], 1))))
						}
						data <- lapply(data, extractLatestValidTo)
						data <- as.matrix_full(data)
						#data <- as.matrix_full(lapply(data, head, 1))
						data <- asNumericDataFrame(data)
					}
				}
				# Special case for taxa:
				else if("taxa" %in% names(data)){
					attrs <- gettaxaMatrix(data, name=".attrs")
					synonyms <- gettaxaMatrix(data, name="taxaSynonyms")
					data <- merge(attrs, synonyms)
					data <- asNumericDataFrame(data)
					
					# Convert to one row per species, with the scientific, norwegian, english and russian name as columns:
					# system.time(data <- getNMDinfo("taxa"))
					# Extract the preferred:
					data <- data[data$synonym.preferred==1, ]

					# Get the first row of each 'Ind':
					out <- data[!duplicated(data$Ind), !names(data) %in% c("synonym.language", "synonym.name", "synonym.preferred")]
					# Make sure that 'Ind' are the row indices:
					out <- out[order(out$Ind), ]

					# Get all present values of 'synonym.language':
					synonym.language <- unique(data$synonym.language)
					# Create a data frame of NAs for the synonym.language, and insert present values into this data frame:
					synonym.name <- as.data.frame(array("", dim=c(nrow(out), length(synonym.language))), stringsAsFactors=FALSE)
					names(synonym.name) <- synonym.language
					
					for(this in synonym.language){
						y <- data[data$synonym.language == this, ]
						synonym.name[y$Ind, this] <- y[, "synonym.name"]
					}

					# Insert the columns of scientific, norwegian, english and russian names:
					data <- cbind(out[, c("Ind", "tsn")], synonym.name, out[, !names(out) %in% c("Ind", "tsn")])
				}
				# Else do a basic simplification:
				else if(is.list(data[[1]])){
					data <- t(Reduce(cbind, data))
					data <- asNumericDataFrame(data)
				}
				
				
			}
		}
	}
	##### <<<Get the information #####
	################################
	data
}
#' 
##' @export
##' @rdname getNMDinfo
##' 
# Function for adding the query of type version:
addQuery <- function(URL, ...){
	l <- list(...)
	query <- paste(names(l), l, sep="=", collapse="&")
	#query <- paste0("version=", ver[[type]])
	paste(URL, query, sep="?")
}
# Get the URL base string:
getURLbase <- function(API, type, model=NULL, dataset=NULL, ver){
	version <- paste0("v", ver$API)
	query <- paste0("version=", ver[[type]])
	if(length(model)){
		model <- paste("model", model, sep="/")
	}
	if(length(dataset)){
		dataset <- paste("dataset", dataset, sep="/")
	}
	out <- apply(cbind(API, type, version, model, dataset), 1, paste, collapse="/")
	# Add the query of type version:
	if(length(ver)){
		out <- addQuery(out, version=ver[[type]])
	}
	#out <- addQuery(out, ver=ver, type=type)
	#out <- paste(out, query, sep="?")
	out
}
# Function to extract the variables given in the levels[[2]] in the list elements named by levels[[1]]
getElements <- function(data, levels=list("element", c("text", ".attrs")), data.frame.out=TRUE){
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
			# Any one of the rows has length differing from the rest, delete this row (herevy we require that all variables are filled):
			s <- sapply(data, length)
			equalLengths <- s==max(s)
			if(!all(equalLengths)){
				data <- data[equalLengths]
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
			data <- unname(unlist(data))
		}
	}
	else if(length(levels)==1){
		data <- NULL
	}
	data
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

#' 
#' @export
#' @rdname getNMDinfo
#' 
getNMDinfoV2 <- function(type=NULL, ver=list(API="2", biotic="1.4", reference="2.0"), API="http://tomcat7.imr.no:8080/apis/nmdapi", recursive=TRUE, msg=FALSE, simplify=TRUE){
	
	# Assure that the API version is 2:
	if(ver$API != 2){
		warning("This function returns reference info from NMD API version 2. Selecting a different version is not recommended.")
	}
	
	##### Internal functions>>> #####
	
	# Convert to a data frame:
	asNumericDataFrame <- function(data){
		data <- as.data.frame(data, stringsAsFactors=FALSE)
		convertableToNumeric <- function(x, not=c("POSIXct", "POSIXt")){
			!class(x) %in% not && !any(is.na(as.numeric(x[!is.na(x)])))
		}
		# Convert all numeric columns to numeric, identified by no NAs when converting:
		#suppressWarnings(data <- lapply(data, function(x) if(!any(is.na(as.numeric(x[!is.na(x)])))) as.numeric(x) else x))
		suppressWarnings(data <- lapply(data, function(x) if(convertableToNumeric(x)) as.numeric(x) else x))
		data <- as.data.frame(data, stringsAsFactors=FALSE)
	}
	
	# Detect platform or taxa reference data:
	isPlatform <- function(data){
		any(names(data[[1]]) == "platformCodes")
	}
	isTaxa <- function(data){
		any(names(data[[1]]) == "TaxaSynonyms")
	}
	
	# Function for extracting the list of reference names:
	getReferenceList <- function(API, ver, msg=FALSE){
		# V1: http://tomcat7.imr.no:8080/apis/nmdapi/reference/v1
		# V1: http://tomcat7.imr.no:8080/apis/nmdapi/reference/v1?version=1.0
		# V2: http://tomcat7.imr.no:8080/apis/nmdapi/reference/v2?version=2.0
		URLbase <- getURLbase(API=API, type="reference", ver=ver)
		# Get the list of cruise series:
		data <- downloadXML(URLbase, msg=msg)
		data <- getElements(data, levels=list("row", c("name")), data.frame.out=FALSE)
		data
	}
	
	# Function used to extracting the year, cruise code and ship name of a cruise series:
	getCruiseSeriesCruises <- function(x){
		year <- NAsapply(x$samples, function(y) y$sampleTime)
		Cruise <- NAsapply(x$samples, function(y) y$cruises$cruise$cruisenr)
		ShipName <- NAsapply(x$samples, function(y) y$cruises$cruise$shipName)
		data.frame(year=year, Cruise=Cruise, ShipName=ShipName, stringsAsFactors=FALSE)
	}
	# Function used for extracting a data frame of the StoX projects used in a survey time series:
	#getSurveyTimeSeriesProjects <- function(x, URLbase){
	#	this <- downloadXML(paste(URLbase, x, sep="/"), msg=msg)$Sample
	#	as.data.frame(as.matrix_full(this), stringsAsFactors=FALSE)
	#}
	getSurveyTimeSeriesProjects <- function(x, URLbase){
		sampleTime <- NAsapply(x$cruiseSeries$cruiseSeries$samples, function(y) y$sampleTime)
		stoxProjectId <- NAsapply(x$cruiseSeries$cruiseSeries$samples, function(y) y$stoxProject)
		STSCode <- x$code
		STSName <- x$name
		CSCode <- x$cruiseSeries$cruiseSeries$cruiseSeriesCode
		CSName <- x$cruiseSeries$cruiseSeries$cruiseSeries
		data.frame(sampleTime=sampleTime, stoxProjectId=stoxProjectId, STSCode=STSCode, STSName=STSName, CSCode=CSCode, CSName=CSName, stringsAsFactors=FALSE)
	}
	
	# Function used for applying either getCruiseSeriesCruises() or getSurveyTimeSeriesProjects() for all years:
	getSeriesInfo <- function(API, ver, type){
		# V1: http://tomcat7.imr.no:8080/apis/nmdapi/cruiseseries/v1
		# V2: http://tomcat7.imr.no:8080/apis/nmdapi/reference/v2/model/cruiseseries?version=2.0
		# V2: http://tomcat7.imr.no:8080/apis/nmdapi/reference/v2/dataset/cruiseseries?version=2.0
		# V1: http://tomcat7.imr.no:8080/apis/nmdapi/surveytimeseries/v1
		# V2: http://tomcat7.imr.no:8080/apis/nmdapi/reference/v2/model/surveytimeseries?version=2.0
		# V2: http://tomcat7.imr.no:8080/apis/nmdapi/reference/v2/dataset/surveytimeseries?version=2.0
		
		# Get the function to use for extracting the series info (one of getCruiseSeriesCruises() and getSurveyTimeSeriesProjects()):
		fun <- switch(type[1], 
			cruiseseries = getCruiseSeriesCruises, 
			surveytimeseries = getSurveyTimeSeriesProjects)
		
		# Get the list of cruise/survey time series. The name can be given exactly as the second element of 'type', in which case the list of series is reduced, but still a list (used in getNMDdata()):
		requestedSeries <- NULL
		if(length(type)==2){
			requestedSeries <- type[2]
			recursive <- TRUE
		}
		# Download either the 'model' (the names of the elements of the series) or the 'dataset' (the contents of the series):
		if(recursive){
			# Download the seies dataset:
			URLbase <- getURLbase(API=API, type="reference", dataset=type[1], ver=ver)
			data <- downloadXML(URLbase, msg=msg)
			# Extract the names of the series:
			seriesNames <- sapply(data, "[[", "name")
			# Extract the series info:
			data <- lapply(data, fun)
			names(data) <- seriesNames
			if(length(requestedSeries)){
				data <- data[requestedSeries]
			}
		}
		else{
			# Download the seies model (only the names of the series):
			URLbase <- getURLbase(API=API, type="reference", model=type[1], ver=ver)
			data <- downloadXML(URLbase, msg=msg)
			# Extract the names:
			data <- getElements(data, levels=list("row", c("name")), data.frame.out=FALSE)
		}
		data
	}
	
	# Function for converting a column to a list with names given by another column:
	column2ilst <- function(x, col, colnames){
		out <- as.list(x[[col]])
		names(out) <- gsub(" ", "_", x[[colnames]])
		out
	}
	
	## Extract the relevant data from the platform reference data:
	#getPlatformOne_old <- function(x){
	#	# Read the platformCode:
	#	if(length(x$platformCodes)){
	#		codes <- getElements(x$platformCodes, levels=list("platformCode", NA), data.frame.out=TRUE)
	#		
	#		# Split by validTo date, and pick the latest:
	#		codes <- split(codes, codes$validTo)
	#		codes <- codes[[length(codes)]]
	#		
	#		# If there are duplicated vessel names for the latest 'validTo', order these with decreasing 'validFrom', and remove the duplicates:
	#		duplicatedSysname <- duplicated(codes$sysname)
	#		if(any(duplicatedSysname)){
	#			codes <- codes[order(codes$validFrom, decreasing=TRUE), ]
	#			duplicatedSysname <- duplicated(codes$sysname)
	#			codes <- codes[!duplicatedSysname, ]
	#		}
	#		
	#		# Create a data frame added the validTo info:
	#		codes <- list(column2ilst(codes, "value", "sysname"), validTo=codes$validTo[1])
	#	}
	#	else{
	#		codes <- list()
	#	}
	#	
	#	# Add the data which is not in a list and not attributes:
	#	headerData <- !sapply(x, is.list) & names(x) != ".attrs"
	#	codes <- as.data.frame(c(x[headerData], codes), stringsAsFactors=FALSE)
	#	
	#	# Special care of the nation, which is stripped for trailing whitespace:
	#	codes$nation <- trimws(codes$nation, which=c("right"))
	#	
	#	return(codes)
	#}
	#getPlatform_old <- function(x){
	#	x <- lapply(x, getPlatformOne)
	#	x <- as.matrix_full(x)
	#	x <- asNumericDataFrame(x)
	#	x
	#}
	
	# Function for converting from UNIX time to date:
	toDate <- function(x, tz=""){
		if(is.numeric(x)){
			out <- as.POSIXct(as.vector(x), origin="1970-01-01", tz=tz)
		}
		as.Date(out, tz=tz)
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

	# Extract the relevant data from the platform reference data:
	#getPlatformOne <- function(x, raw.out=FALSE){
	getPlatformOne <- function(x){
		# Read the platformCode:
		if(length(x$platformCodes)){
			# Get the platform data of the current platform:
			codes <- getElements(x$platformCodes, levels=list("platformCode", NA), data.frame.out=TRUE)
			
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
				platform <- list(column2ilst(codes, "value", "sysname"), validFrom=codes$validFrom_POSIXct[1], validTo=codes$validTo_POSIXct[1])
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
	getPlatform <- function(x, cores=1){
		cat("Converting platform data to data frame...\n")
		x <- papply(x, getPlatformOne, cores=cores)
		# Split into individual rows for as.matrix.full() to work:
		x <- lapply(x, function(DF) split(DF, seq_len(nrow(DF))))
		x <- unlist(x, recursive=FALSE, use.names=FALSE)
		x <- as.dataFrame_full(x)
		x <- asNumericDataFrame(x)
		x
	}
	
	# Extract the relevant data from the platform reference data:
	getTaxaOne <- function(x){
		# Read the platformCode:
		if(length(x$TaxaSynonyms)){
			codes <- getElements(x$TaxaSynonyms, levels=list("synonym", NA), data.frame.out=TRUE)
			# Split by 'preferred'', and pick the latest:
			codes <- split(codes, codes$preferred)
			codes <- codes[[length(codes)]]
			
			# Create a data frame added the validTo info:
			codes <- list(column2ilst(codes, "name", "language"), preferred=codes$preferred[1])
		}
		else{
			codes <- list()
		}
		
		# Add the data which is not in a list and not attributes:
		headerData <- !sapply(x, is.list) & names(x) != ".attrs"
		codes <- as.data.frame(c(x[headerData], codes), stringsAsFactors=FALSE)
		
		return(codes)
	}
	getTaxa <- function(x){
		x <- lapply(x, getTaxaOne)
		#x <- as.dataFrame_full(x)
		x <- as.dataFrame_full(x)
		# Add the same Ind column as in version 1:
		x <- cbind(Ind = seq_len(nrow(x)), x)
		x <- asNumericDataFrame(x)
		x
	}
	
	getCruiseInfo <- function(API, ver, msg=FALSE){
		URL <- addQuery(paste(API, "biotic", paste0("v", ver$API), sep="/"), type="ListAll")
		d <- downloadXML(URL, msg=msg)
		s <- lapply(d, function(x) column2ilst(getElements(x), "text", ".attrs"))
		x <- as.dataFrame_full(s)
		x <- asNumericDataFrame(x)
		x
	}
	
	##### <<<Internal functions #####
	###############################
	
	
	# Get the list of reference data types:
	if(length(type)==0){
		data <- getReferenceList(API, ver, msg=FALSE)
		return(data)
	}
	
	# Treat the requested type of information:
	type[1] <- tolower(type[1])
	#vesseltype <- FALSE
	if(length(intersect(type, c("v", "vessel")))){
		type <- "platform"
		simplify <- TRUE
		#vesseltype <- TRUE
	}
	
	# Get the list of cruises:
	if(type[1] %in% c("c", "cruises")){
		warning("Returning a list of cruises will be implemented in version 2")
	}
	# Get the list of cruises series with cruises for each series:
	else if(type[1] %in% c("cs", "cruiseseries")){
		# Set the full name of the type:
		type[1] <- "cruiseseries"
		data <- getSeriesInfo(API=API, ver=ver, type=type)
	}
	# Get the list of survey time series with StoX projets for each series:
	else if(type[1] %in% c("sts", "surveytimeseries")){
		# Set the full name of the type:
		type[1] <- "surveytimeseries"
		data <- getSeriesInfo(API=API, ver=ver, type=type)
	}
	else if(type[1] %in% c("cruise", "cruises")){
		data <- getCruiseInfo(API=API, ver=ver, msg=msg)
	}
	else{
		# Get the available reference data types:
		URLbase <- getURLbase(API=API, type="reference", ver=ver)
		# Get the list of references:
		ref <- getReferenceList(API, ver, msg=FALSE)
		
		# Match the 'type' with the reference data available:
		type <- ref[tolower(ref) == type[1]]
		
		# Download the reference data:
		URLbase <- getURLbase(API=API, type="reference", dataset=type[1], ver=ver)
		data <- downloadXML(URLbase, msg=msg)
		# Simplify the data:
		if(simplify){
			if(length(data)){
				# If the reference data contains a list of data, it will in API version 2 be either 'platform' or 'taxa', which requires more elaborated extraction. If not using getElements() will suffice, since each "row" of the data represents one row in the matrix:
				arelists <- sapply(data[[1]], is.list)
				if(!any(arelists)){
					data <- getElements(data, levels=list("row", NA), data.frame.out=TRUE)
				}
				else{
					if(isPlatform(data)){
						data <- getPlatform(data)
						
					}
					else if(isTaxa(data)){
						data <- getTaxa(data)
					}
					else{
						stop(paste0("The reference data ", type[1], " is not implemented in version 2."))
					}
				}
			}
		}
	}
	
	return(data)
}
##' 
#' @export
#' @rdname getNMDinfo
#' 
getNMDdata <- function(cruise=NULL, year=NULL, shipname=NULL, serialno=NULL, tsn=NULL, datatype=NULL, dir=NULL, subdir=FALSE, group="default", abbrev=FALSE, subset=NULL, filebase="NMD", ver=list(API="2", biotic="1.4", reference="2.0"), API="http://tomcat7.imr.no:8080/apis/nmdapi", cleanup=TRUE, model="StationLengthDistTemplate", msg=TRUE, ow=NULL, return.URL=FALSE, run=TRUE, timeout=NULL, ...){
	
	if((!is.list(ver) && ver==1) || (is.list(ver) && ver$API==1)){
		getNMDdataV1(cruise=cruise, year=year, shipname=shipname, serialno=serialno, tsn=tsn, datatype=datatype, dir=dir, subdir=subdir, group=group, abbrev=abbrev, subset=subset, filebase=filebase, ver=1, API=API, cleanup=cleanup, model=model, msg=msg, ow=ow, return.URL=return.URL, run=run, timeout=timeout, ...)
	}
	else if(is.list(ver) && ver$API==2){
		getNMDdataV2(cruise=cruise, year=year, shipname=shipname, serialno=serialno, tsn=tsn, datatype=datatype, dir=dir, subdir=subdir, group=group, abbrev=abbrev, subset=subset, filebase=filebase, ver=ver, API=API, cleanup=cleanup, model=model, msg=msg, ow=ow, return.URL=return.URL, run=run, timeout=timeout, ...)
	}
	else{
		warning("Version 2 of the NMD API is the latest available. Version 1 is no longer maintained (as of july 2018).")
		NULL
	}
}
#'
#' @export
#' @rdname getNMDinfo
#' 
getNMDdataV1 <- function(cruise=NULL, year=NULL, shipname=NULL, serialno=NULL, tsn=NULL, datatype=NULL, dir=NULL, subdir=FALSE, group="default", abbrev=FALSE, subset=NULL, filebase="NMD", ver=1, API="http://tomcat7.imr.no:8080/apis/nmdapi", cleanup=TRUE, model="StationLengthDistTemplate", msg=TRUE, ow=NULL, return.URL=FALSE, run=TRUE, timeout=NULL, ...){
	
	##### Internal functions: #####
	downloadFailedWarning <- function(x, type=c("file", "sts")){
		#warning(paste0("Downloading failed for the following ", if(type[1]=="file") "files" else "Survey Timeseries", ":\n\t", paste(x, collapse="\n\t"), "\nPossible reason: Timeout during downloading, in which case the timeout option could be increased (from the default value getOption(\"timeout\")) by, e.g., options(timeout=600) for UNIX systems, and options(download.file.method=\"internal\", timeout=600) for Windows systems, where the default download method does not repond to setting the timeout option from R)"))
		warning(paste0("Downloading failed for the following ", if(type[1]=="file") "files" else "Survey Timeseries", ":\n\t", paste(x, collapse="\n\t")))
	}
	
	# Function for converting a vector of serial numbers, which can be fully or partly sequenced (incriment of 1 between consecutive elements):
	getSerialnoRanges <- function(x){
		d <- diff(c(x))
		starts <- c(1, which(d != 1)+1)
		ends <- c(which(d != 1), length(x))
		cbind(x[starts], x[ends])
	}
	# Function for getting the URL for serial number searches:
	getURLBySerialno <- function(serialno, year, tsn=NULL, API="http://tomcat7.imr.no:8080/apis/nmdapi", ver=1){
		paste(API, "biotic", paste0("v", ver), year[1], serialno[1], serialno[2], tsn[1], "serial", sep="/")
	}
	# Function for building a project path, possibly abbreviating words:
	abbrevPath <- function(x, abbrev=FALSE){
		# Define first the file name:
		filename <- abbrevWords(x$name, abbrev=abbrev)
		# Add file base, such as "NMD" separated by "_":
		if(length(x$filebase)){
			filename <- paste(c(x$filebase, filename), collapse="_")
		}
		# Add the suffix, abbreviating stricter than the rest of the path, by sub=-1:	
		if(length(x$suffix)){
			filename <- paste(c(filename, abbrevWords(x$suffix, abbrev=abbrev, sub=-1, collapse="")), collapse="_")
		}
		
		# Should a subdirectory be used?:
		if(length(x$subdir)){
			x$dir <- file.path(x$dir, abbrevWords(x$subdir, abbrev=abbrev))
		}
		# Get the path:
		projectPaths <- file.path(x$dir, filename)
		# Strip off any double slashes
		gsub("//", "/", projectPaths)
	}
	# Function for downloading a stox project in a surveytimeseries:
	getSurveyTimeSeriesStoXProjects <- function(sts, stsInfo, projectParts, dir, cleanup=TRUE, downloadtype="?format=zip", abbrev=FALSE, ow=NULL, run=TRUE){
		# Set original and abbreviated project names:
		projectPaths <- unlist(lapply(projectParts, abbrevPath, abbrev=abbrev))
		if(!run){
			return(projectPaths)
		}
		projectPathsOrig <- unlist(lapply(projectParts, abbrevPath, abbrev=FALSE))
		
		# The number of stox projects:
		nsts <- length(projectParts)
		# Store the project names and the success of the downloads:
		success = logical(nsts)
		
		# Run through the projects and download:
		for(i in seq_len(nsts)){
			# Create the project directory:
			suppressWarnings(dir.create(dirname(projectPaths[i]), recursive=TRUE))
			URL <- URLencode(paste(API, "surveytimeseries", paste0("v", ver), sts, "samples", stsInfo[i,"sampleTime"], sep="/"))
			# Add download type:
			URL = paste0(URL, downloadtype)
			#projectPaths[i] <- file.path(dir, paste0(abbrevWords(sts), "_", stsInfo[i,"sampleTime"]))
			
			# The following using onlyone=nsts==1 did not work, since typing "ya" did not continue the loop:
			#success[i] <- downloadProjectZip(URL=URL, projectName=projectPaths[i], cleanup=cleanup, msg=msg, ow=ow, onlyone=nsts==1)$success
			success[i] <- downloadProjectZip(URL=URL, projectName=projectPaths[i], cleanup=cleanup, msg=msg, ow=ow)$success
		}
		
		# Warning if any downloads failed:
		if(any(!success)){
			downloadFailedWarning(projectPathsOrig[!success], type="sts")
			#warning(paste0("Downloading failed for the following Survey Timeseries:\n\t", paste(projectPathsOrig[!success], collapse="\n\t"), "\nPossible reason: Timeout during downloading, in which case the timeout option could be increased (from the default value getOption(\"timeout\")) by, e.g., options(timeout=600) for UNIX systems, and options(download.file.method=\"internal\", timeout=600) for Windows systems, where the default download method does not repond to setting the timeout option from R)"))
		}
		
		# Report project names if abbreviated:
		if(!all(projectPaths==projectPathsOrig)){
			cat("Project names abbreviated:\n")
			print(
				data.frame(
					projectPaths=projectPaths,
					projectPathsOrig=projectPathsOrig
				)
			)
		}
		# Return the projectPaths, where those not downloaded are represented by NA:
		invisible(projectPaths)
	}
	# Function for extracting URLs to a list of cruises. The conversion from the names given by 'datatype' to the names given by 'StoX_data_types' is needed to save the data in the StoX directory structure:
	getCruiseStrings <- function(csInfo, datatype, StoX_data_types, ver, API="http://tomcat7.imr.no:8080/apis/nmdapi"){
		# Use the search function in version 1:
		if(ver==1){
			# Pick out the first element of 'csInfo', since a list is always returned from getNMDinfo():
			#cruiseURL <- apply(csInfo[[1]][,c("Cruise", "ShipName"), drop=FALSE], 1, searchNMDCruise, datatype=datatype[1])
			cat("Searching for cruises...\n")
			cruiseURL <- t(apply(csInfo[,c("Cruise", "ShipName"), drop=FALSE], 1, searchNMDCruise, datatype=datatype, ver=ver, API=API))
			#cruiseURL <- sapply(datatype, function(xx) sub(datatype[1], xx, cruiseURL))
			#if(length(dim(cruiseURL))==0){
			#	dim(cruiseURL) <- c(1, length(cruiseURL))
			#}
			colnames(cruiseURL) <- StoX_data_types
			cbind(csInfo, cruiseURL, stringsAsFactors=FALSE)
		}
		else{
			warning("Version 1 is currently the latest verison")
		}
	}
	# Small function used to interpret the subdir parameter:
	getSubdir <- function(subdir, name){
		if(isTRUE(subdir)) name else if(is.character(subdir)) subdir else NULL
	}
	# Function for downloading cruises:
	getCruises <- function(projectParts, cruiseMatrixSplit, StoX_data_types, model="StationLengthDistTemplate", ow=NULL, abbrev=FALSE, run=TRUE, ...){
		
		# Get project names and create the projects:
		projectPaths <- unlist(lapply(projectParts, abbrevPath, abbrev=abbrev))
		if(!run){
			return(projectPaths)
		}
		projectPathsOrig <- unlist(lapply(projectParts, abbrevPath, abbrev=FALSE))
		# projectPaths <- unlist(lapply(projectPaths, createProject, model=model, ow=ow, ...)) # Here we should implement some way of setting ow=TRUE interactively at first prompt
		##### temp <- unlist(lapply(projectPaths, createProject, model=model, ow=ow, ...)) # Here we should implement some way of setting ow=TRUE interactively at first prompt
		
		
		temp <- createProject(projectPaths, model=model, ow=ow, ...)
		suppressWarnings(toWrite <- which(!is.na(temp)))
		if(length(toWrite)==0){
			return()
		}
		projectPaths <- projectPaths[toWrite]
		projectPathsOrig <- projectPathsOrig[toWrite]
	
		# Plot a time bar showing the progress of the reading and plotting:
		if(msg){
			infostring <- "Downloading files from NMD:"
			cat(infostring,"\n",sep="")
			#totalsteps <- length(cruiseMatrixSplit)
			totalsteps <- sum(sapply(cruiseMatrixSplit[toWrite], nrow) * length(StoX_data_types))
			stepfact <- nchar(infostring)/totalsteps
			oldvalue <- 0
			index <- 0
		}
		for(i in toWrite){
			xmlfiles <- matrix(NA, nrow=nrow(cruiseMatrixSplit[[i]]), ncol=length(StoX_data_types))
			colnames(xmlfiles) <- paste0("file_", StoX_data_types)
			for(j in seq_along(StoX_data_types)){
				for(k in seq_len(nrow(cruiseMatrixSplit[[i]]))){
					# Print a dot if the floor of the new value exceeds the old value:
					if(msg){
						index <- index + 1
						thisvalue = floor(index*stepfact)
						if(thisvalue > oldvalue){
							cat(rep(".",thisvalue-oldvalue),if(index == totalsteps) "\n", sep="")
							oldvalue = thisvalue
							}
						}
					# Get the current URL:
					URL <- cruiseMatrixSplit[[i]][k, StoX_data_types[j]]
					if(!is.na(URL)){
						# Use the naming convention that NMD uses, which is 'datatype'_cruiseNumber_'cruiseNumber'_'ShipName'
						cruise_shipname <- paste(NMD_data_types[j], "cruiseNumber", cruiseMatrixSplit[[i]][k,"Cruise"], cruiseMatrixSplit[[i]][k,"ShipName"], sep="_")
						xmlfiles[k,j] <- file.path(projectPaths[i], "input", StoX_data_types[j], paste0(cruise_shipname, ".xml"))
						suppressWarnings(downloadXML(URL, msg=FALSE, list.out=FALSE, file=xmlfiles[k,j], timeout=timeout))
					}
				}
			}
			# Check whether the files were downloaded. This could have been done by use of the output from download.file (0 for sucsess and positive for failure), but instead we check the existence of the files, and the size:
			valid <- !is.na(xmlfiles)
			URLs <- cruiseMatrixSplit[[i]][, StoX_data_types][valid]
			success <- file.exists(xmlfiles[valid]) & (file.info(xmlfiles[valid])$size > 0) %in% TRUE
			# Warning if any downloads failed:
			if(any(!success)){
				downloadFailedWarning(URLs[!success])
				#warning(paste0("Downloading failed for the following files:\n", paste(URLs[!success], collapse="\n\t")))
				#warning(paste0("Downloading failed for the following files:\n\t", paste(URLs[!success], collapse="\n\t"), "\nPossible reason: Timeout during downloading, in which case the timeout option could be increased (from the default value getOption(\"timeout\")) by, e.g., options(timeout=600) for UNIX systems, and options(download.file.method=\"internal\", timeout=600) for Windows systems, where the default download method does not repond to setting the timeout option from R)"))
			}
			
			cruiseMatrixSplit[[i]] <- cbind(cruiseMatrixSplit[[i]], xmlfiles)
		}
		
		# Report project names if abbreviated:
		if(!all(projectPaths==projectPathsOrig)){
			cat("Project names abbreviated:\n")
			print(
				data.frame(
					projectPaths = projectPaths,
					projectPathsOrig = projectPathsOrig
				)
			)
		}
			
		# Update the projects (linking to the downloaded files) and return the paths:
		lapply(projectPaths, updateProject)
		return(projectPaths)
	}
	
	##########
	# Define the valid types:
	NMD_data_types <- getRstoxDef("NMD_data_types")
	StoX_data_types <- getRstoxDef("StoX_data_types")
	if(length(datatype)==0){
		datatype <- NMD_data_types
	}
	else{
		StoX_data_types <- StoX_data_types[NMD_data_types %in% datatype]
	}
	
	# Get URLs to the cruises:
	# Get the available cruise series and survey time series (StoX) in order to recognize the inputs:
	cs <- getNMDinfo("cs", recursive=FALSE)
	sts <- getNMDinfo("sts", recursive=FALSE)
	#######################################
	
	dir <- getProjectPaths(projectName="", projectRoot=dir)$projectRoot
	
	########################################
	########## (1) Serial number: ##########
	########################################
	maxSerialno <- 99999
	if(length(serialno)==0 && length(year)){
		serialno <- seq(1, maxSerialno)
	}
	if(length(serialno)){
		if(length(year)==0){
			warning("'year' must be given when serial number is requested")
			return(NULL)
		}
		if(any(serialno > maxSerialno)){
			serialno <- serialno[serialno <= maxSerialno]
			warning(paste0("Maximum serialno is ", maxSerialno))
		}
		serialno <- getSerialnoRanges(serialno)
		serialnoStrings <- apply(serialno, 1, paste, collapse="-")
		serialnoStrings <- paste0("serialno", "_", serialnoStrings)
		#serialnoString <- paste("serialno", serialnoRanges, sep="_", collapse="_")
		if(length(tsn)){
			tsnString <- paste("tsn", tsn, sep="_")
		}
		else{
			tsnString <- NULL
		}
		
		serialnoRangeString <- paste0("serialno_", paste(range(serialno), collapse="-"), "_")
		projectName <- paste(c(filebase, serialnoRangeString, tsnString, "year", year[1]), collapse="_")
		projectName <- gsub("__", "_", projectName)
		# Abbreviate:
		projectName <- abbrevWords(projectName, abbrev=abbrev, sub=-1)
		if(!run){
			return(projectName)
		}
		# Set the directory of the project specfied by serial number:
		projectPath <- createProject(projectName, dir=dir, model=model, ow=ow, ...)
	
		xmlfiles <- rep(NA, nrow(serialno))
		for(i in seq_along(xmlfiles)){
			URL <- getURLBySerialno(serialno=serialno[i,], year=year[1], tsn=tsn, API=API, ver=ver)
			xmlfiles[i] <- file.path(projectPath, "input", "biotic", paste0(serialnoStrings[i], ".xml"))
			downloadXML(URL, msg=msg, list.out=FALSE, file=xmlfiles[i], timeout=timeout)
		}
		
		# Check whether the files were downloaded. This could have been done by use of the output from download.file (0 for sucsess and positive for failure), but instead we check the existence of the files, and the size:
		success <- file.exists(xmlfiles) & (file.info(xmlfiles)$size > 0) %in% TRUE
		# Warning if any downloads failed:
		if(any(!success)){
			downloadFailedWarning(xmlfiles[!success])
			#warning(paste0("Downloading failed for the following files:\n", paste(xmlfiles[!success], collapse="\n\t")))
			#warning(paste0("Downloading failed for the following files:\n\t", paste(xmlfiles[!success], collapse="\n\t"), "\nPossible reason: Timeout during downloading, in which case the timeout option could be increased (from the default value getOption(\"timeout\")) by, e.g., options(timeout=600) for UNIX systems, and options(download.file.method=\"internal\", timeout=600) for Windows systems, where the default download method does not repond to setting the timeout option from R)"))
			
			
		}
		
		updateProject(projectName)
		return(projectName)
	}
	########################################
	
	
	#############################################
	########## (2) Survey time series: ##########
	#############################################
	if(length(cruise)==1 && cruise %in% sts){
		# Get the matrix of stoxProjectId and sampleTime (i.e., year), and the name of the survey time series (sts):
		sts <- getNMDinfo(c("sts", cruise))
		stsInfo <- sts[[1]]
		sts <- names(sts)
		
		# Set the parts of the project name, one for each year:
		nprojects <- nrow(stsInfo)
		projectParts <- vector("list", nprojects)
		for(i in seq_len(nprojects)){
			projectParts[[i]] <- list(
				dir = dir, # The default workspace or 'dir' if this is given
				subdir = getSubdir(subdir=subdir, name=sts), # See getSubdir(). If subdir==TRUE, this will become the survey time series name
				filebase = NULL, 
				name = sts, # The survey time series name
				suffix = stsInfo[i, "sampleTime"]
			)
		}
		
		# Select all or some of the projects:
		if(length(subset)==0){
			subset = seq_len(nprojects)
		}
		else{
			if(all(nchar(subset) > 3) && any(subset %in% stsInfo[, "sampleTime"])){
				subset <- which(subset == stsInfo[, "sampleTime"])
			}
			# Otherwise, restrict 'subset' to the range of projects:
			else{
				subset = subset[subset>=1 & subset<=nprojects]
			}
			if(length(subset)==0){
				warning("The value of 'subset' excluded all years")
				return(NULL)
			}
			#subset = subset[subset>=1 & subset<=nprojects]
		}
		projectParts <- projectParts[subset]
		stsInfo <- stsInfo[subset, , drop=FALSE]
	
		# Download and unzip all StoX projects of the survey time series:
		projectNames <- getSurveyTimeSeriesStoXProjects(sts=sts, stsInfo=stsInfo, projectParts=projectParts, dir=dir, cleanup=cleanup, ow=ow, abbrev=abbrev, downloadtype="?format=zip", run=run)
		
		###lapply(stsInfo[,"sampleTime"], getSurveyTimeSeriesStoXProject, sts=sts, dir=dir, cleanup=cleanup, downloadtype="?format=zip")
		return(projectNames)
	}
	#############################################
	
	########################################
	########## (3) Cruise series: ##########
	########################################
	else if(length(cruise)==1 && cruise %in% cs){
		# Get the matrix of stoxProjectId and sampleTime (i.e., year), and the name of the survey time series (sts):
		cs <- getNMDinfo(c("cs", cruise))
		csInfo <- cs[[1]]
		cs <- names(cs)
		cruiseMatrix <- getCruiseStrings(csInfo=csInfo, datatype=datatype, StoX_data_types=StoX_data_types, ver=ver, API=API)
		# Discard any filebase, which is a prefix on the StoX project name:
		filebase <- NULL
		# Set the name of the cruise series, which will be used in the project names, appended suffixes of year and ship name:
		name <- cs
	}
	########################################
	
	##################################
	########## (4) Cruises: ##########
	##################################
	else{
		# Cruises are downloaded to the top directory, and are not grouped by year or otherwise:
		group = "c"
		if(subdir){
			warning("subdir = TRUE will be ignored when downloading cruises, which will all be placed in the download directory specified by 'dir'")
		}
		subdir <- NULL
		# No name for the projects. The project names are built from the 'filebase' and the suffixes of year/Cruise and ship name:
		name <- NULL
		
		if(length(shipname)==0){
			warning("Under the current version (version 1) 'shipname' must be specified alongside 'cruise'")
			cruiseMatrix <- NULL
		}
		else{
			cruiseMatrix <- getCruiseStrings(data.frame(Cruise=cruise, ShipName=shipname), datatype=datatype, StoX_data_types=StoX_data_types, ver=ver, API=API)
			yearbase <- cruiseMatrix[,StoX_data_types]
			yearbase <- yearbase[!is.na(yearbase)]
			if(length(yearbase)==0){
				warning("No data downloaded")
				return(cruiseMatrix)
			}
			cruiseMatrix <- cbind(year=NMDdecode(yearbase[1])$year, cruiseMatrix, stringsAsFactors=FALSE)
		}
	}
	##################################
	
	
	###########################################
	########## (5) Download cruises: ##########
	###########################################
	# Split into groups:
	if(length(group)==0){
		# All data in one project if 'group' is empty:
		cruiseMatrixSplit <- list(cruiseMatrix)
		
		# Wrap in a list to indicate the numer of projects to generate:
		projectParts <- list(
			list(
				dir = dir, # The default workspace or 'dir' if this is given
				subdir = NULL, # See getSubdir(). If subdir==TRUE, this will become the survey time series name
				filebase = filebase, 
				name = cs, # The survey time series name
				suffix = paste("year", paste(range(cruiseMatrix[,"year"]), collapse="-"), sep="_")
				)
			)
	}
	else{
		if(tolower(substr(group, 1, 1))=="d"){
			if(nrow(cruiseMatrix)==1){
				group = "c"
			}
			else{
				group = "y"
			}
		}
		if(tolower(substr(group, 1, 1))=="y"){
			splitnames <- cruiseMatrix[,"year"]
			splitvec <- paste("year", splitnames, sep="_")
		}
		else{
			splitnames <- cruiseMatrix[,"Cruise"]
			splitvec <- paste("Cruise", splitnames, sep="_")
		}
		# This adds the 'splitvec' as names of the 'cruiseMatrixSplit':
		cruiseMatrixSplit <- split(cruiseMatrix, splitvec)
		names(cruiseMatrixSplit) <- unique(splitnames)
		
		# Discard cells with no data:
		cruiseMatrixSplit <- cruiseMatrixSplit[sapply(cruiseMatrixSplit, function(xx) any(!is.na(xx[, StoX_data_types])))]
		
		# define the project names:
		nprojects = length(cruiseMatrixSplit)
		projectParts <- vector("list", nprojects)
		
		for(i in seq_len(nprojects)){
			
			ship <- unique(cruiseMatrixSplit[[i]][,"ShipName"])
			if(length(ship)>2){
				ship <- paste(range(ship), collapse="--")
			}
			else{
				ship <- paste(ship, collapse="-")
			}
			
			projectParts[[i]] <- list(
				dir = dir, # The default workspace or 'dir' if this is given
				subdir = getSubdir(subdir=if(nprojects==1) NULL else subdir, name=cs),
				filebase = filebase, 
				name = name, # The survey time series name
				suffix = paste(names(cruiseMatrixSplit)[i], paste("ShipName", ship, sep="_"), sep="_")
			)
		}
	}
	
	# Select all or some of the projects:
	if(length(subset)==0){
		subset = seq_len(nprojects)
	}
	else{
		# Check whether the subset is a year or cruise code:
		if(all(nchar(subset) > 3) && any(subset %in% names(cruiseMatrixSplit))){
			subset <- which(subset == names(cruiseMatrixSplit))
		}
		# Otherwise, restrict 'subset' to the range of projects:
		else{
			subset = subset[subset>=1 & subset<=nprojects]
		}
		if(length(subset)==0){
			warning("The value of 'subset' excluded all projects")
			return(NULL)
		}
	}
	cruiseMatrixSplit <- cruiseMatrixSplit[subset]
	projectParts <- projectParts[subset]
	#if(msg){
	#	print(cruiseMatrixSplit)
	#}
	if(return.URL){
		return(cruiseMatrixSplit)
	}
	
	# Download the cruises:
	projectNames <- getCruises(projectParts, cruiseMatrixSplit, StoX_data_types, model=model, ow=ow, abbrev=abbrev, run=run, ...)
	###########################################
	
		
	# Point to the downloaded files in each project:
	#lapply(projectNames, pointToStoXFiles) before 2016-11-04
	# The following line was disabled on 2017-12-13 and moved to inside getCruises() to make the code more robust:
	# lapply(projectNames, updateProject)
	#getURIAsynchronous
	
	return(projectNames)
}
#'
#' @export
#' @rdname getNMDinfo
#' 
getNMDdataV2 <- function(cruise=NULL, year=NULL, shipname=NULL, serialno=NULL, tsn=NULL, datatype=NULL, dir=NULL, subdir=FALSE, group="default", abbrev=FALSE, subset=NULL, filebase="NMD", ver=list(API="2", biotic="1.4", reference="2.0"), API="http://tomcat7.imr.no:8080/apis/nmdapi", cleanup=TRUE, model="StationLengthDistTemplate", msg=TRUE, ow=NULL, return.URL=FALSE, run=TRUE, timeout=NULL, ...){
	
	# Assure that the API version is 2:
	if(ver$API != 2){
		warning("This function returns reference info from NMD API version 2. Selecting a different version is not recommended.")
	}
	
	##### Internal functions: #####
	downloadFailedWarning <- function(x, type=c("file", "sts")){
		#warning(paste0("Downloading failed for the following ", if(type[1]=="file") "files" else "Survey Timeseries", ":\n\t", paste(x, collapse="\n\t"), "\nPossible reason: Timeout during downloading, in which case the timeout option could be increased (from the default value getOption(\"timeout\")) by, e.g., options(timeout=600) for UNIX systems, and options(download.file.method=\"internal\", timeout=600) for Windows systems, where the default download method does not repond to setting the timeout option from R)"))
		warning(paste0("Downloading failed for the following ", if(type[1]=="file") "files" else "Survey Timeseries", ":\n\t", paste(x, collapse="\n\t")))
	}
	
	# Function for converting a vector of serial numbers, which can be fully or partly sequenced (incriment of 1 between consecutive elements):
	getSerialnoRanges <- function(x){
		d <- diff(c(x))
		starts <- c(1, which(d != 1)+1)
		ends <- c(which(d != 1), length(x))
		cbind(x[starts], x[ends])
	}
	# Function for getting the URL for serial number searches:
	getURLBySerialno <- function(serialno, year, tsn=NULL, API="http://tomcat7.imr.no:8080/apis/nmdapi", ver=list(API="2", biotic="1.4", reference="2.0")){
		version <- paste0("v", ver$API)
		#query <- paste0("version=", ver$biotic)
		out <- paste(c(API, "biotic", version, year[1], serialno[1], serialno[2], tsn[1], "serial"), collapse="/")
		# Add the query of type version:
		out <- addQuery(out, version=ver$biotic)
		#out <- addQuery(out, ver=ver, type="biotic")
		out
	}
	# Function for building a project path, possibly abbreviating words:
	abbrevPath <- function(x, abbrev=FALSE){
		# Define first the file name:
		filename <- abbrevWords(x$name, abbrev=abbrev)
		# Add file base, such as "NMD" separated by "_":
		if(length(x$filebase)){
			filename <- paste(c(x$filebase, filename), collapse="_")
		}
		# Add the suffix, abbreviating stricter than the rest of the path, by sub=-1:	
		if(length(x$suffix)){
			filename <- paste(c(filename, abbrevWords(x$suffix, abbrev=abbrev, sub=-1, collapse="")), collapse="_")
		}
		
		# Should a subdirectory be used?:
		if(length(x$subdir)){
			x$dir <- file.path(x$dir, abbrevWords(x$subdir, abbrev=abbrev))
		}
		# Get the path:
		projectPaths <- file.path(x$dir, filename)
		# Strip off any double slashes
		gsub("//", "/", projectPaths)
	}
	# Function for downloading a stox project in a surveytimeseries:
	getSurveyTimeSeriesStoXProjects <- function(sts, stsInfo, projectParts, dir, cleanup=TRUE, format="zip", abbrev=FALSE, ow=NULL, run=TRUE, ver=list(API="2", biotic="1.4", reference="2.0")){
		# Set original and abbreviated project names:
		projectPaths <- unlist(lapply(projectParts, abbrevPath, abbrev=abbrev))
		if(!run){
			return(projectPaths)
		}
		projectPathsOrig <- unlist(lapply(projectParts, abbrevPath, abbrev=FALSE))
		
		# The number of stox projects:
		nsts <- length(projectParts)
		# Store the project names and the success of the downloads:
		success = logical(nsts)
		
		# Run through the projects and download:
		for(i in seq_len(nsts)){
			# Create the project directory:
			suppressWarnings(dir.create(dirname(projectPaths[i]), recursive=TRUE))
			
			
			
			
			
			
			
			
		
		
		
		
			
			#http://tomcat7.imr.no:8080/apis/nmdapi/reference/v2/model/surveytimeseries/'STS_ID'/cruiseseries/'CS_ID'/samples/'YAER'/zip?format=zip&version=2.0
			
			#http://tomcat7.imr.no:8080/apis/nmdapi/reference/v2/model/surveytimeseries/1/cruiseseries/5/samples/1994/zip?format=zip&version=2.0
			
			#http://tomcat7.imr.no:8080/apis/nmdapi/reference/v2/model/surveytimeseries/1/cruiseseries/5/samples/1994/zip?version=2.0&format=zip
			
			URL <- URLencode(paste(API, "reference", paste0("v", ver$API), "model/surveytimeseries", stsInfo$STSCode, "cruiseseries", stsInfo$CSCode, "samples", stsInfo[i,"sampleTime"], "zip", sep="/"))
			
			
			
			
			
			#URL <- URLencode(paste(API, "surveytimeseries", paste0("v", ver$API), sts, "samples", stsInfo[i,"sampleTime"], sep="/"))
			# Add download type:
			URL <- addQuery(URL, version=ver$reference, format=format)
			#URL = paste0(URL, downloadtype)
			#projectPaths[i] <- file.path(dir, paste0(abbrevWords(sts), "_", stsInfo[i,"sampleTime"]))
			
			# The following using onlyone=nsts==1 did not work, since typing "ya" did not continue the loop:
			#success[i] <- downloadProjectZip(URL=URL, projectName=projectPaths[i], cleanup=cleanup, msg=msg, ow=ow, onlyone=nsts==1)$success
			success[i] <- downloadProjectZip(URL=URL, projectName=projectPaths[i], cleanup=cleanup, msg=msg, ow=ow)$success
		}
		
		# Warning if any downloads failed:
		if(any(!success)){
			downloadFailedWarning(projectPathsOrig[!success], type="sts")
			#warning(paste0("Downloading failed for the following Survey Timeseries:\n\t", paste(projectPathsOrig[!success], collapse="\n\t"), "\nPossible reason: Timeout during downloading, in which case the timeout option could be increased (from the default value getOption(\"timeout\")) by, e.g., options(timeout=600) for UNIX systems, and options(download.file.method=\"internal\", timeout=600) for Windows systems, where the default download method does not repond to setting the timeout option from R)"))
		}
		
		# Report project names if abbreviated:
		if(!all(projectPaths==projectPathsOrig)){
			cat("Project names abbreviated:\n")
			print(
				data.frame(
					projectPaths=projectPaths,
					projectPathsOrig=projectPathsOrig
				)
			)
		}
		# Return the projectPaths, where those not downloaded are represented by NA:
		invisible(projectPaths)
	}
	# Function for extracting URLs to a list of cruises. The conversion from the names given by 'datatype' to the names given by 'StoX_data_types' is needed to save the data in the StoX directory structure:
	getCruiseStrings <- function(csInfo, datatype, StoX_data_types, ver, API="http://tomcat7.imr.no:8080/apis/nmdapi"){
		# Use the search function in version 1:
		if(ver$API==2){
			# Pick out the first element of 'csInfo', since a list is always returned from getNMDinfo():
			#cruiseURL <- apply(csInfo[[1]][,c("Cruise", "ShipName"), drop=FALSE], 1, searchNMDCruise, datatype=datatype[1])
			cat("Searching for cruises...\n")
			cruiseURL <- t(apply(csInfo[,c("Cruise", "ShipName"), drop=FALSE], 1, searchNMDCruise, datatype=datatype, ver=ver, API=API))
			#cruiseURL <- sapply(datatype, function(xx) sub(datatype[1], xx, cruiseURL))
			#if(length(dim(cruiseURL))==0){
			#	dim(cruiseURL) <- c(1, length(cruiseURL))
			#}
			colnames(cruiseURL) <- StoX_data_types
			cbind(csInfo, cruiseURL, stringsAsFactors=FALSE)
		}
		else{
			warning("Version 1 is currently the latest verison")
		}
	}
	# Small function used to interpret the subdir parameter:
	getSubdir <- function(subdir, name){
		if(isTRUE(subdir)) name else if(is.character(subdir)) subdir else NULL
	}
	# Function for downloading cruises:
	getCruises <- function(projectParts, cruiseMatrixSplit, StoX_data_types, model="StationLengthDistTemplate", ow=NULL, abbrev=FALSE, run=TRUE, ...){
		
		# Get project names and create the projects:
		projectPaths <- unlist(lapply(projectParts, abbrevPath, abbrev=abbrev))
		if(!run){
			return(projectPaths)
		}
		projectPathsOrig <- unlist(lapply(projectParts, abbrevPath, abbrev=FALSE))
		# projectPaths <- unlist(lapply(projectPaths, createProject, model=model, ow=ow, ...)) # Here we should implement some way of setting ow=TRUE interactively at first prompt
		##### temp <- unlist(lapply(projectPaths, createProject, model=model, ow=ow, ...)) # Here we should implement some way of setting ow=TRUE interactively at first prompt
		
		
		temp <- createProject(projectPaths, model=model, ow=ow, ...)
		suppressWarnings(toWrite <- which(!is.na(temp)))
		if(length(toWrite)==0){
			return()
		}
		projectPaths <- projectPaths[toWrite]
		projectPathsOrig <- projectPathsOrig[toWrite]
	
		# Plot a time bar showing the progress of the reading and plotting:
		if(msg){
			infostring <- "Downloading files from NMD:"
			cat(infostring,"\n",sep="")
			#totalsteps <- length(cruiseMatrixSplit)
			totalsteps <- sum(sapply(cruiseMatrixSplit[toWrite], nrow) * length(StoX_data_types))
			stepfact <- nchar(infostring)/totalsteps
			oldvalue <- 0
			index <- 0
		}
		for(i in toWrite){
			xmlfiles <- matrix(NA, nrow=nrow(cruiseMatrixSplit[[i]]), ncol=length(StoX_data_types))
			colnames(xmlfiles) <- paste0("file_", StoX_data_types)
			for(j in seq_along(StoX_data_types)){
				for(k in seq_len(nrow(cruiseMatrixSplit[[i]]))){
					# Print a dot if the floor of the new value exceeds the old value:
					if(msg){
						index <- index + 1
						thisvalue = floor(index*stepfact)
						if(thisvalue > oldvalue){
							cat(rep(".",thisvalue-oldvalue),if(index == totalsteps) "\n", sep="")
							oldvalue = thisvalue
							}
						}
					# Get the current URL:
					URL <- cruiseMatrixSplit[[i]][k, StoX_data_types[j]]
					if(!is.na(URL)){
						# Use the naming convention that NMD uses, which is 'datatype'_cruiseNumber_'cruiseNumber'_'ShipName'
						cruise_shipname <- paste(NMD_data_types[j], "cruiseNumber", cruiseMatrixSplit[[i]][k,"Cruise"], cruiseMatrixSplit[[i]][k,"ShipName"], sep="_")
						xmlfiles[k,j] <- file.path(projectPaths[i], "input", StoX_data_types[j], paste0(cruise_shipname, ".xml"))
						suppressWarnings(downloadXML(URL, msg=FALSE, list.out=FALSE, file=xmlfiles[k,j], timeout=timeout))
					}
				}
			}
			# Check whether the files were downloaded. This could have been done by use of the output from download.file (0 for sucsess and positive for failure), but instead we check the existence of the files, and the size:
			valid <- !is.na(xmlfiles)
			URLs <- cruiseMatrixSplit[[i]][, StoX_data_types][valid]
			success <- file.exists(xmlfiles[valid]) & (file.info(xmlfiles[valid])$size > 0) %in% TRUE
			# Warning if any downloads failed:
			if(any(!success)){
				downloadFailedWarning(URLs[!success])
				#warning(paste0("Downloading failed for the following files:\n", paste(URLs[!success], collapse="\n\t")))
				#warning(paste0("Downloading failed for the following files:\n\t", paste(URLs[!success], collapse="\n\t"), "\nPossible reason: Timeout during downloading, in which case the timeout option could be increased (from the default value getOption(\"timeout\")) by, e.g., options(timeout=600) for UNIX systems, and options(download.file.method=\"internal\", timeout=600) for Windows systems, where the default download method does not repond to setting the timeout option from R)"))
			}
			
			cruiseMatrixSplit[[i]] <- cbind(cruiseMatrixSplit[[i]], xmlfiles)
		}
		
		# Report project names if abbreviated:
		if(!all(projectPaths==projectPathsOrig)){
			cat("Project names abbreviated:\n")
			print(
				data.frame(
					projectPaths = projectPaths,
					projectPathsOrig = projectPathsOrig
				)
			)
		}
			
		# Update the projects (linking to the downloaded files) and return the paths:
		lapply(projectPaths, updateProject)
		return(projectPaths)
	}
	
	##########
	# Define the valid types:
	NMD_data_types <- getRstoxDef("NMD_data_types")
	StoX_data_types <- getRstoxDef("StoX_data_types")
	if(length(datatype)==0){
		datatype <- NMD_data_types
	}
	else{
		StoX_data_types <- StoX_data_types[NMD_data_types %in% datatype]
	}
	
	# Get URLs to the cruises:
	# Get the available cruise series and survey time series (StoX) in order to recognize the inputs:
	cs <- getNMDinfoV2("cs", recursive=FALSE)
	sts <- getNMDinfoV2("sts", recursive=FALSE)
	#######################################
	
	dir <- getProjectPaths(projectName="", projectRoot=dir)$projectRoot
	
	########################################
	########## (1) Serial number: ##########
	########################################
	maxSerialno <- 99999
	if(length(serialno)==0 && length(year)){
		serialno <- seq(1, maxSerialno)
	}
	if(length(serialno)){
		if(length(year)==0){
			warning("'year' must be given when serial number is requested")
			return(NULL)
		}
		if(any(serialno > maxSerialno)){
			serialno <- serialno[serialno <= maxSerialno]
			warning(paste0("Maximum serialno is ", maxSerialno))
		}
		serialno <- getSerialnoRanges(serialno)
		serialnoStrings <- apply(serialno, 1, paste, collapse="-")
		serialnoStrings <- paste0("serialno", "_", serialnoStrings)
		#serialnoString <- paste("serialno", serialnoRanges, sep="_", collapse="_")
		if(length(tsn)){
			tsnString <- paste("tsn", tsn, sep="_")
		}
		else{
			tsnString <- NULL
		}
		
		serialnoRangeString <- paste0("serialno_", paste(range(serialno), collapse="-"), "_")
		projectName <- paste(c(filebase, serialnoRangeString, tsnString, "year", year[1]), collapse="_")
		projectName <- gsub("__", "_", projectName)
		# Abbreviate:
		projectName <- abbrevWords(projectName, abbrev=abbrev, sub=-1)
		if(!run){
			return(projectName)
		}
		# Set the directory of the project specfied by serial number:
		projectPath <- createProject(projectName, dir=dir, model=model, ow=ow, ...)
	
		xmlfiles <- rep(NA, nrow(serialno))
		for(i in seq_along(xmlfiles)){
			URL <- getURLBySerialno(serialno=serialno[i,], year=year[1], tsn=tsn, API=API, ver=ver)
			xmlfiles[i] <- file.path(projectPath, "input", "biotic", paste0(serialnoStrings[i], ".xml"))
			downloadXML(URL, msg=msg, list.out=FALSE, file=xmlfiles[i], timeout=timeout)
		}
		
		# Check whether the files were downloaded. This could have been done by use of the output from download.file (0 for sucsess and positive for failure), but instead we check the existence of the files, and the size:
		success <- file.exists(xmlfiles) & (file.info(xmlfiles)$size > 0) %in% TRUE
		# Warning if any downloads failed:
		if(any(!success)){
			downloadFailedWarning(xmlfiles[!success])
			#warning(paste0("Downloading failed for the following files:\n", paste(xmlfiles[!success], collapse="\n\t")))
			#warning(paste0("Downloading failed for the following files:\n\t", paste(xmlfiles[!success], collapse="\n\t"), "\nPossible reason: Timeout during downloading, in which case the timeout option could be increased (from the default value getOption(\"timeout\")) by, e.g., options(timeout=600) for UNIX systems, and options(download.file.method=\"internal\", timeout=600) for Windows systems, where the default download method does not repond to setting the timeout option from R)"))
			
			
		}
		
		updateProject(projectName)
		return(projectName)
	}
	########################################
	
	
	#############################################
	########## (2) Survey time series: ##########
	#############################################
	if(length(cruise)==1 && cruise %in% sts){
		# Get the matrix of stoxProjectId and sampleTime (i.e., year), and the name of the survey time series (sts):
		sts <- getNMDinfoV2(c("sts", cruise))
		stsInfo <- sts[[1]]
		#sts <- names(sts)
		sts <- cruise
		
		# Set the parts of the project name, one for each year:
		nprojects <- nrow(stsInfo)
		projectParts <- vector("list", nprojects)
		for(i in seq_len(nprojects)){
			projectParts[[i]] <- list(
				dir = dir, # The default workspace or 'dir' if this is given
				subdir = getSubdir(subdir=subdir, name=sts), # See getSubdir(). If subdir==TRUE, this will become the survey time series name
				filebase = NULL, 
				name = sts, # The survey time series name
				suffix = stsInfo[i, "sampleTime"]
			)
		}
		
		# Select all or some of the projects:
		if(length(subset)==0){
			subset = seq_len(nprojects)
		}
		else{
			if(all(nchar(subset) > 3) && any(subset %in% stsInfo[, "sampleTime"])){
				subset <- which(subset == stsInfo[, "sampleTime"])
			}
			# Otherwise, restrict 'subset' to the range of projects:
			else{
				subset = subset[subset>=1 & subset<=nprojects]
			}
			if(length(subset)==0){
				warning("The value of 'subset' excluded all years")
				return(NULL)
			}
			#subset = subset[subset>=1 & subset<=nprojects]
		}
		projectParts <- projectParts[subset]
		stsInfo <- stsInfo[subset, , drop=FALSE]
	
		# Download and unzip all StoX projects of the survey time series:
		projectNames <- getSurveyTimeSeriesStoXProjects(sts=sts, stsInfo=stsInfo, projectParts=projectParts, dir=dir, cleanup=cleanup, ow=ow, abbrev=abbrev, format="zip", run=run,  ver=ver)
		
		###lapply(stsInfo[,"sampleTime"], getSurveyTimeSeriesStoXProject, sts=sts, dir=dir, cleanup=cleanup, downloadtype="?format=zip")
		return(projectNames)
	}
	#############################################
	
	########################################
	########## (3) Cruise series: ##########
	########################################
	else if(length(cruise)==1 && cruise %in% cs){
		# Get the matrix of stoxProjectId and sampleTime (i.e., year), and the name of the survey time series (sts):
		cs <- getNMDinfoV2(c("cs", cruise))
		csInfo <- cs[[1]]
		cs <- names(cs)
		print("cs")
		browser()
		cruiseMatrix <- getCruiseStrings(csInfo=csInfo, datatype=datatype, StoX_data_types=StoX_data_types, ver=ver, API=API)
		# Discard any filebase, which is a prefix on the StoX project name:
		filebase <- NULL
		# Set the name of the cruise series, which will be used in the project names, appended suffixes of year and ship name:
		name <- cs
	}
	########################################
	
	##################################
	########## (4) Cruises: ##########
	##################################
	else{
		# Cruises are downloaded to the top directory, and are not grouped by year or otherwise:
		group = "c"
		if(subdir){
			warning("subdir = TRUE will be ignored when downloading cruises, which will all be placed in the download directory specified by 'dir'")
		}
		subdir <- NULL
		# No name for the projects. The project names are built from the 'filebase' and the suffixes of year/Cruise and ship name:
		name <- NULL
		
		if(length(shipname)==0){
			warning("Under the current version (version 1) 'shipname' must be specified alongside 'cruise'")
			cruiseMatrix <- NULL
		}
		else{
			cruiseMatrix <- getCruiseStrings(data.frame(Cruise=cruise, ShipName=shipname), datatype=datatype, StoX_data_types=StoX_data_types, ver=ver$API, API=API)
			yearbase <- cruiseMatrix[,StoX_data_types]
			yearbase <- yearbase[!is.na(yearbase)]
			if(length(yearbase)==0){
				warning("No data downloaded")
				return(cruiseMatrix)
			}
			cruiseMatrix <- cbind(year=NMDdecode(yearbase[1])$year, cruiseMatrix, stringsAsFactors=FALSE)
		}
	}
	##################################
	
	
	###########################################
	########## (5) Download cruises: ##########
	###########################################
	# Split into groups:
	if(length(group)==0){
		# All data in one project if 'group' is empty:
		cruiseMatrixSplit <- list(cruiseMatrix)
		
		# Wrap in a list to indicate the numer of projects to generate:
		projectParts <- list(
			list(
				dir = dir, # The default workspace or 'dir' if this is given
				subdir = NULL, # See getSubdir(). If subdir==TRUE, this will become the survey time series name
				filebase = filebase, 
				name = cs, # The survey time series name
				suffix = paste("year", paste(range(cruiseMatrix[,"year"]), collapse="-"), sep="_")
				)
			)
	}
	else{
		if(tolower(substr(group, 1, 1))=="d"){
			if(nrow(cruiseMatrix)==1){
				group = "c"
			}
			else{
				group = "y"
			}
		}
		if(tolower(substr(group, 1, 1))=="y"){
			splitnames <- cruiseMatrix[,"year"]
			splitvec <- paste("year", splitnames, sep="_")
		}
		else{
			splitnames <- cruiseMatrix[,"Cruise"]
			splitvec <- paste("Cruise", splitnames, sep="_")
		}
		# This adds the 'splitvec' as names of the 'cruiseMatrixSplit':
		cruiseMatrixSplit <- split(cruiseMatrix, splitvec)
		names(cruiseMatrixSplit) <- unique(splitnames)
		
		# Discard cells with no data:
		cruiseMatrixSplit <- cruiseMatrixSplit[sapply(cruiseMatrixSplit, function(xx) any(!is.na(xx[, StoX_data_types])))]
		
		# define the project names:
		nprojects = length(cruiseMatrixSplit)
		projectParts <- vector("list", nprojects)
		
		for(i in seq_len(nprojects)){
			
			ship <- unique(cruiseMatrixSplit[[i]][,"ShipName"])
			if(length(ship)>2){
				ship <- paste(range(ship), collapse="--")
			}
			else{
				ship <- paste(ship, collapse="-")
			}
			
			projectParts[[i]] <- list(
				dir = dir, # The default workspace or 'dir' if this is given
				subdir = getSubdir(subdir=if(nprojects==1) NULL else subdir, name=cs),
				filebase = filebase, 
				name = name, # The survey time series name
				suffix = paste(names(cruiseMatrixSplit)[i], paste("ShipName", ship, sep="_"), sep="_")
			)
		}
	}
	
	# Select all or some of the projects:
	if(length(subset)==0){
		subset = seq_len(nprojects)
	}
	else{
		# Check whether the subset is a year or cruise code:
		if(all(nchar(subset) > 3) && any(subset %in% names(cruiseMatrixSplit))){
			subset <- which(subset == names(cruiseMatrixSplit))
		}
		# Otherwise, restrict 'subset' to the range of projects:
		else{
			subset = subset[subset>=1 & subset<=nprojects]
		}
		if(length(subset)==0){
			warning("The value of 'subset' excluded all projects")
			return(NULL)
		}
	}
	cruiseMatrixSplit <- cruiseMatrixSplit[subset]
	projectParts <- projectParts[subset]
	#if(msg){
	#	print(cruiseMatrixSplit)
	#}
	if(return.URL){
		return(cruiseMatrixSplit)
	}
	
	# Download the cruises:
	projectNames <- getCruises(projectParts, cruiseMatrixSplit, StoX_data_types, model=model, ow=ow, abbrev=abbrev, run=run, ...)
	###########################################
	
		
	# Point to the downloaded files in each project:
	#lapply(projectNames, pointToStoXFiles) before 2016-11-04
	# The following line was disabled on 2017-12-13 and moved to inside getCruises() to make the code more robust:
	# lapply(projectNames, updateProject)
	#getURIAsynchronous
	
	return(projectNames)
}
#' 
#' @importFrom XML xmlParse xmlToList
#' @export
#' @rdname getNMDinfo
#' 
downloadXML <- function(URL, msg=FALSE, list.out=TRUE, file=NULL, method="auto", timeout=NULL){
	if(file.exists(URL)){
		file <- URL
	}
	else{
		URL <- URLencode(URL)
		#if(msg){
		#	cat("Downloading", URL, "\n")
		#}
		failed <- FALSE
		if(msg){
			used <- proc.time()[3]
		}
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
		if(msg){
			used <- proc.time()[3] - used
			cat("Converting to list...\n")
			#cat("Time left (rough estimate at ", toString(Sys.time()), "): ", signif(8*used, 2), " seconds\n", sep="")
			# 6 seems to give a closer estimate:
			cat("Time left (rough estimate at ", toString(Sys.time()), "): ", signif(6 * used, 2), " seconds\n", sep="")
		}
		# Read the file:
		x <- readChar(file, file.info(file)$size)
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
			x <- XML::xmlToList(x)
			if(length(x)==0){
				warning(paste("URL" ,URLdecode(URL) ,"does not contain data (xmlToList() returning NULL)"))
				return(NULL)
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
#' Search for a cruise given the cruise number and ship name.
#'
#' The NMD API enables searching for a cruise identifyer given the cruise number and ship name.
#'
#' @param cruisenrANDshipname	A vector of two elements, the first being the cruise number and the second the ship name.
#' @param datatype				The type of data requested. Currently implemented are "echosunder" and "biotic", while "landing" and "ctd" are in the pipeline. datatype=NULL (default) returns all possible data.
#' @param ver					The version of the API. As of 2015-05 only version 1 is available. Version 2 will include the possibility to return a list of all cruises.
#' @param API					The path to the API.
#'
#' 
#' @export
#' @keywords internal
#' 
searchNMDCruise <- function(cruisenrANDshipname, datatype, ver=list(API="2", biotic="1.4", reference="2.0"), API="http://tomcat7.imr.no:8080/apis/nmdapi"){
	
	# Function for extacting the URL of the cruise. In version 1 this URL was given directly, whereas in version 2 it has to be constructed from the downloaded table:
	getCruiseString <- function(x, ver=list(API="2", biotic="1.4", reference="2.0")){
		# Download the result from the search query:
		out <- suppressWarnings(downloadXML(URLencode(x), msg=FALSE))
		if(ver$API == 1){
			# Version 1 returns the cruise URL directly:
			out <- out$element$text
		}
		else if(ver$API == 2){
			# In version 2 the elements of the cruise URL are given and must be combined to get the URL:
			g <- getElements(out, levels=list("element", NA))
			gg <- as.list(g$text)
			names(gg) <- g$name
			# Build the URL:
			relativePath <- gg$path
			APIverString <- paste0("v", ver$API)
			query <- paste0("version=", ver[[datatype]])
			out <- paste(API, datatype, APIverString, relativePath, "dataset", sep="/")
			# Add datatype version:
			out <- paste(out, query, sep="?")
		}
		else{
			stop("Invalid version")
		}
		out
	}
	
	# Add support for giving 'ver' as a single numeric, as was done prior to Rstox_1.10:
	if(!is.list(ver)){
		ver <- list(API=ver[1])
	}
	
	# Get the search URL:
	searchURL <- paste(API, datatype, paste0("v", ver$API), paste0("find?cruisenr=", cruisenrANDshipname[1], "&shipname=", cruisenrANDshipname[2]), sep="/")
	out <- rep(NA, length(searchURL))
	for(i in seq_along(out)){
		# Insert the cruise URL:
		temp <- getCruiseString(x=searchURL[i], ver=ver)
		if(length(temp)){
			out[i] <- temp
		}
	}
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
	datatype <- URL[6]
	ver <- URL[7]
	year=NA
	vessel=NA
	cruise=NA
	cs=NA
	sts=NA
	type=NA
	if(datatype %in% c("echosounder","biotic", "cruise")){
	missiontype <- URL[8]
	year <- URL[9]
	vessel <- URL[10]
	cruise <- URL[11]
	}
	else if(datatype=="cruiseseries"){
	if(length(URL)==8){
		cs <- URL[8]
	}
	}
	else if(datatype=="surveytimeseries"){
	if(length(URL)==8){
		sts <- URL[8]
	}
	}
	else if(datatype=="reference"){
	if(length(URL)==8){
		type <- URL[8]
	}
	}
	list(API=API, datatype=datatype, ver=ver, year=year, vessel=vessel, cruise=cruise, cs=cs, sts=sts, type=type)
}
#NMDencode <- function(URLbase=NULL, datatype="echosounder", ver=1, API="http://tomcat7.imr.no:8080/apis/nmdapi", missiontype="Forskningsfartøy", year="2015", vessel="G O Sars-LMEL", cruise="2015106", cs=NULL, sts=NULL, type=NULL){
#	if(length(URLbase)==0){
#	URLbase <- paste(API, datatype, paste0("v", ver), sep="/")
#	}
#	if(datatype %in% c("echosounder","biotic", "cruise")){
#	out <- paste(URLbase, missiontype, year, vessel, cruise, sep="/")
#	}
#	else if(datatype=="cruiseseries"){
#	out <- paste(URLbase, cs, "samples", year, sep="/")
#	}
#	else if(datatype=="surveytimeseries"){
#	out <- paste(URLbase, sts, "samples", year, sep="/")
#	}
#	else if(datatype=="reference"){
#	out <- paste(URLbase, type, sep="/")
#	}
#	URLencode(out)
#}
#############################################
########## After meeting with NMD: ##########
#############################################
#http://tomcat7.imr.no:8080/DatasetExplorer/v1/html/main.html (Dataset Explorer gir en oversikt over dataene som er i API’et og de toktseriene og survey tidsseriene som finnes der).

#http://tomcat7.imr.no:8080/apis/nmdapi/cruise/v1/ (API for tokt informasjon,				 http://tomcat7.imr.no:8080/apis/nmdapi/cruise/v1/Forskningsfart%C3%B8y/2015/G%20O%20Sars-LMEL/2015106 eksempel på et gitt tokt)
#http://tomcat7.imr.no:8080/apis/nmdapi/biotic/v1/ (API for biologisk informasjon (fiskedata) http://tomcat7.imr.no:8080/apis/nmdapi/biotic/v1/Forskningsfart%C3%B8y/2015/G%20O%20Sars-LMEL/2015106 eksempel på et gitt tokt)
#http://tomcat7.imr.no:8080/apis/nmdapi/echosounder/v1/ (API for akustiske data http://tomcat7.imr.no:8080/apis/nmdapi/echosounder/v1/Forskningsfart%C3%B8y/2015/G%20O%20Sars-LMEL/2015106 eksempel på et gitt tokt)
#http://tomcat7.imr.no:8080/apis/nmdapi/cruiseseries/v1 (toktserie som er definert, legg til verdien i en av <result> taggene for å få ut innholdet til en toktserie)
#http://tomcat7.imr.no:8080/apis/nmdapi/surveytimeseries/v1 (survey timeserie, brukes på samme måte som cruiseseries)
#http://tomcat7.imr.no:8080/apis/nmdapi/stox/v1 (stox prosjekt filer)
#http://tomcat7.imr.no:8080/apis/nmdapi/reference/v1 (referanse data)

#For å søke mot API’et etter et tokt kan du bruke find komandoen på denne måten:
#http://tomcat7.imr.no:8080/apis/nmdapi/echosounder/v1/find?cruisenr=2015106&shipname=G%20O%20Sars


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
