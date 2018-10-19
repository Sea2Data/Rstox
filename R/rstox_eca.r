#' Extract the covaraite definitions
#' @keywords internal
getCovparam <- function(projectName, parameter) {
  # Get the project object:
  project <- openProject(projectName, out="project")
  s <- project$getProcessData()$asTable("covparam")
  if(nchar(s)>0){
    out <- read.csv(textConnection(s), sep='\t', row.names=NULL, stringsAsFactors=FALSE, na.strings="-", encoding="UTF-8")
    if (!(parameter %in% out$Parameter)){
      stop(paste0("Parameter ", parameter, " not found in covparam"))
    }
    out <- out[out$Parameter==parameter, c("CovariateTable", "Value")]
    l <- as.list(out$Value)
    names(l) <- out$CovariateTable
    return(l)
  }
  else{
    stop(paste0("Table \"", "covparam", "\" missing in the project.xml file"))
  }
}

#*********************************************
#*********************************************
#' Prepare data from a project to the ECA model
#' 
#' This function reads data from a baseline run and converts to a list of data used by the ECA model in the Reca package.
#' 
#' @param projectName   The name or full path of the project, a baseline object (as returned from \code{\link{getBaseline}} or \code{\link{runBaseline}}, og a project object (as returned from \code{\link{openProject}}).
#' @param biotic		The process from which the biotic data are extracted, conventionally the BioticCovData process.
#' @param landing		The process from which the landing data are extracted, conventionally the LandingCovData process.
#' @param temporal		Optional definition of the temporal covariate (not yet implemented).
#' @param gearfactor	Optional definition of the gearfactor covariate (not yet implemented).
#' @param spatial		Optional definition of the spatial covariate (not yet implemented).
#' @param ...			Parameters passed to \code{\link{getBaseline}}.
#'
#' @return A reference to the StoX Java baseline object
#'
#' @export
#' @rdname baseline2eca
#'
baseline2eca <- function(projectName, biotic="BioticCovData", landing="LandingCovData", temporal=NULL, gearfactor=NULL, spatial=NULL, ...){
	# Function that retreives year, month, day, yearday:
	addYearday <- function(x, datecar="startdate", tz="UTC", format="%d/%m/%Y"){
		x[[datecar]] <- as.POSIXlt(x[[datecar]], tz=tz, format=format)
		if(length(x$year)==0){
			x$year <- x[[datecar]]$year + 1900
		}
		x$month <- x[[datecar]]$mon + 1
		x$monthday <- x[[datecar]]$mday
		x$yearday <- x[[datecar]]$yday + 1
		x
	}
	# Function used for extracting covariate definitions:
	getCovDef <- function(x){
		# If the covariate is given as a comma separated string, split into a vector:
		if(length(grep(",", x[[3]]))){
			x[[3]] = strsplit(x[[3]], ",")
			x[[3]] = lapply(x[[3]], as.numeric)
		}
			#biotic <- as.data.frame(sapply(x, "[", x$CovariateSourceType=="Biotic", drop=FALSE))
		biotic <- x[x$CovariateSourceType=="Biotic", , drop=FALSE]
			#landing <- as.data.frame(lapply(x, "[", x$CovariateSourceType=="Landing", drop=FALSE))
		landing <- x[x$CovariateSourceType=="Landing", , drop=FALSE]
		list(biotic=biotic, landing=landing)
	}
	# Function for aggregating landing data in each covariate cell:
	aggregateLanding <- function(x, names, covariateDefinition){
		# Replace missing values by "-" to include these levels in the aggregation, and change back to NA at the end. This is because by() does not like NAs in the indices. We should find a better way later...:
		x[names][is.na(x[names])] <- "-"
		# Convert to tonnes from the hg definition in the landing data:
		weight_hektogram = by(x$rundvekt, x[,names], sum, na.rm=TRUE)
			weight_tonnes <- weight_hektogram/10000
			covariates <- attributes(weight_tonnes)$dimnames
			suppressWarnings(covariates <- lapply(covariates, as.numeric))
			covariates <- expand.grid(covariates)
			#covariates = expand.grid(attributes(weight_tonnes)$dimnames)
			covariatesFactor <- lapply(seq_along(names), function(xx) covariateDefinition[[xx]]$biotic[covariates[[xx]], "Covariate"])
			names(covariatesFactor) <- names(covariateDefinition)
			# Aggregate by the covariates:
		out = data.frame(covariates, covariatesFactor, weight_tonnes=c(weight_tonnes), stringsAsFactors=FALSE)
		# Remove empty cells and order:
		out = out[!is.na(out$weight_tonnes),,drop=FALSE]
		out = out[do.call(order, c(out[names], list(na.last=FALSE))),,drop=FALSE]
		out[out=="-"] <- NA
		rownames(out) <- seq_len(nrow(out))
			out
	}
	
	# Define covariate processes and returned process data:
	# covariateProcessesData <- c("temporal", "season", "gearfactor", "spatial") # Changed on 2018-08-28 according to Jira STOX-153:
	# covariateProcessesData <- c("temporal", "gearfactor", "spatial") # This is not used anywhere....
	
	# Get the baseline output:
	### baselineOutput <- getBaseline(projectName, input=c("par", "proc"), fun=c(biotic, landing))
	baselineOutput <- getBaseline(projectName, input=c("par", "proc"), proc=c(biotic, landing), ...)
	
	# Run if both biotic and landing data are present:
	if(all(c(biotic[1], landing[1]) %in% names(baselineOutput$out))){
	
		#####################################
		##### (1) Get raw landing data: #####
		#####################################
		
		# (1a) Get the data and convert variable names to lower case:
		landing <- baselineOutput$outputData[[landing[1]]]
		names(landing) <- tolower(names(landing))
		
		# 2018-08-28: Changed to using 'sistefangstdato' as per comment from Edvin:
		#landing <- addYearday(landing, datecar="formulardato", tz="UTC", format="%d/%m/%Y")
		landing <- addYearday(landing, datecar="sistefangstdato", tz="UTC", format="%d/%m/%Y")
		#####################################
	
		############################################################
		##### (2) Get raw biotic data with some modifications: #####
		############################################################
		# (2a) Get the data and convert variable names to lower case:
		biotic <- baselineOutput$outputData[[biotic[1]]]
		names(biotic) <- tolower(names(biotic))
	
		# Detect whether temporal is defined with seasons, and add year and season and remove temporal in the process data:
		# This caused error with cod and could be solved with Jira STOX-153:
		
		# Disabled on 2018-08-28, according to Jira STOX-153, where now 'temporal' is the only temporal covariate (no longer 'year' and 'season'):
		### if(any(baselineOutput$out$LandingCovData$Season %in% TRUE)){
		### 	baselineOutput$proc$season <- baselineOutput$proc$temporal
		### 	years <- range(biotic$year, landing$year)
		### 	years <- seq(years[1], years[2])
		### 	#baselineOutput$proc$year <- data.frame(CovariateSourceType=rep(c("Biotic","Landing"), each=length(years)), Covariate=rep(years, ,2), Value=rep(years, ,2), stringsAsFactors=FALSE)
		### 	baselineOutput$proc$year <- data.frame(CovariateSourceType=rep(c("Biotic","Landing"), each=length(years)), Covariate=rep(years, ,2), Definition=rep(years, ,2), stringsAsFactors=FALSE)
		### 	baselineOutput$proc$temporal <- NULL
		### }
	
		# (2b) Define the present covariate names, which are some but not all of the following:
		# Changed on 2018-08-28 according to Jira STOX-153:
		# implementedCovariateNames <- c("year", "season", "gearfactor", "spatial")
		#implementedCovariateDescriptions <- c("The year covariate, used in conjunction with 'season'", "The season covariate defining seasons throughout a year", "The gear covariate given as groups of gear codes", "The spatial covariate giving polygons or locations")
		#implementedCovariateProcesses <- c("DefineTemporalLanding", "DefineTemporalLanding", "DefineGearLanding", "DefineSpatialLanding")
		#implementedCovariateNames <- c("temporal", "gearfactor", "spatial")
		#implementedCovariateDescriptions <- c("The temporal covariate", "The gear covariate given as groups of gear codes", "The spatial covariate giving polygons or locations")
		#implementedCovariateProcesses <- c("DefineTemporalLanding", "DefineGearLanding", "DefineSpatialLanding")
		ECACovariates <- getRstoxDef("ECACovariates")
		configuredCovariates <- names(getCovparam(projectName, "CovariateType"))
		present <- which(ECACovariates$Name %in% configuredCovariates)
		covariateNames <- ECACovariates$Name[present]
		covariateDescriptions <- ECACovariates$Description[present]
		covariateProcesses <- ECACovariates$Processe[present]
		
		# (2c) Add yearday, year and month:
		biotic <- addYearday(biotic, datecar="startdate", tz="UTC", format="%d/%m/%Y")
	 
		# (2d) Hard code the lengthunits (from the sampling handbook). This must be changed in the future, so that lengthunitmeters is present in the biotic file:
		# TO BE IMPLEMENTED: Read the lengthresolution from NMD, which here is named by lengthunitmeters and then renamed to lengthresM in getGlobalParameters():
		# TO BE IMPLEMENTED: lengthresolution <- getNMDinfo("lengthresolution")
		lengthcode <- 1:7
		# Length unit codes in the reference tables:
		lengthmeters <- c(1, 5, 10, 30, 50, 0.5, 0.1)/1000
		biotic$lengthunitmeters <- lengthmeters[match(biotic$lengthunit, lengthcode)]
		
		
		# This section was removed with approval from Hanne and David. It was included for historical reasons, and the freqency column was supported in prevoius ECA versions, but we decided that there is no need for compressing the input data in this way anymore, given the assiciated complication of the input data:
		# (2e) Aggregate by lines without weight, but with equal length:
		#duplines = duplicated(biotic[,c("serialno", "length", "weight")]) & is.na(biotic$age)
		#if(any(duplines)){
		#	frequency = by(biotic$frequency, paste(biotic$serialno, biotic$length), sum)
		#	biotic = biotic[!duplines,]
		#	biotic$frequency = frequency
		#}
		############################################################
	
		#######################################################
		### (3) Get covariate definitions and change names: ###
		#######################################################
		covariateDefinition <- lapply(baselineOutput$proc[covariateNames], getCovDef)
		# Add year covariate definitions if present:
		if("year" %in% covariateNames){
			year <- unique(c(landing$year, biotic$year))
			#yearBiotic = data.frame(CovariateSourceType="Biotic", Covariate=year, Value=year, stringsAsFactors=FALSE)
			#yearLanding = data.frame(CovariateSourceType="Biotic", Covariate=year, Value=year, stringsAsFactors=FALSE)
			yearBiotic = data.frame(CovariateSourceType="Biotic", Covariate=year, Definition=year, stringsAsFactors=FALSE)
			yearLanding = data.frame(CovariateSourceType="Biotic", Covariate=year, Definition=year, stringsAsFactors=FALSE)
			covariateDefinition$year <- list(biotic=yearBiotic, landing=yearLanding)
		}
	 
		# Extract the covariates from biotic and landing in two separate matrices, and convert to integers using match():
		allLevels <- lapply(covariateNames, function(x) sort(unique(c(biotic[[x]], landing[[x]]))))
		names(allLevels) <- covariateNames
		Nlevels <- sapply(allLevels, length)
		
		# Function for converting the covariates present in biotic or landing to integer:
		getCovariateMatrix <- function(data, covariateNames, allLevels){
			# Get the covariate names present in the data:
			covariateNames_present <- intersect(covariateNames, names(data))
			# Convert each covariate to integer (index in allLevels):
			out <- sapply(covariateNames_present, function(this) match(data[[this]], allLevels[[this]]))
			# Convert to data frame and add colnames:
			out <- as.data.frame(out, stringsAsFactors=FALSE)
			colnames(out) <- covariateNames_present
			out
		}
		covariateMatrixBiotic <- getCovariateMatrix(biotic, covariateNames=covariateNames, allLevels=allLevels)
		covariateMatrixLanding <- getCovariateMatrix(landing, covariateNames=covariateNames, allLevels=allLevels)
		
		#covariateMatrixBiotic <- sapply(seq_along(covariateNames), function(i) match(biotic[[covariateNames[i]]], allLevels[[i]]))
		#covariateMatrixLanding <- sapply(seq_along(covariateNames), function(i) match(landing[[covariateNames[i]]], allLevels[[i]]))
		#covariateMatrixBiotic <- as.data.frame(covariateMatrixBiotic, stringsAsFactors=FALSE)
		#covariateMatrixLanding <- as.data.frame(covariateMatrixLanding, stringsAsFactors=FALSE)
		#colnames(covariateMatrixBiotic) <- covariateNames
		#colnames(covariateMatrixLanding) <- covariateNames
		
		
		# Match the levels of each covariate with the unique values of the union of biotic and landing:
		matchToBioticAndLanding <- function(i, allLevels, covariateDefinition){
			allValues <- sort(unique(union(covariateDefinition[[i]]$biotic[,2], covariateDefinition[[i]]$landing[,2])))
			link <- match(allLevels[[i]], allValues)
			data.frame(Numeric=seq_along(link), Covariate=allValues[link], stringsAsFactors=FALSE)
		}
		covariateLink <- lapply(seq_along(allLevels), matchToBioticAndLanding, allLevels=allLevels, covariateDefinition=covariateDefinition)
		names(covariateLink) <- names(covariateDefinition)
		
		#covariateLink <- lapply(seq_along(allLevels), function(i) match(allLevels[[i]], covariateDefinition[[i]]$biotic[,2]))
		#covariateLink <- lapply(seq_along(allLevels), function(i) data.frame(Numeric=seq_along(allLevels[[i]]), Covariate=covariateDefinition[[i]]$biotic[covariateLink[[i]], 2], stringsAsFactors=FALSE))
		#names(covariateLink) <- names(covariateDefinition)
		
		
		#############################################
		##### (6) Get aggreagated landing data: #####
		#############################################
		# Change added on 2018-09-31:
		# Stop the function if there are any missing values in 'rundvekt' or in any of the covariates:
		if(any(is.na(landing$rundvekt))){
			stop("Missing values in landing$rundvekt.")
		}
		testCovariate <- function(name, covariateMatrixLanding){
			if(any(is.na(covariateMatrixLanding[[name]]))){
				stop(paste0("Missing values in covariate ", name, "."))
			}
		}
		lapply(names(covariateMatrixLanding), testCovariate, covariateMatrixLanding=covariateMatrixLanding)
		
		#landingAggOrig <- aggregateLanding(landing, covariateNames, covariateDefinition=covariateDefinition)
		#landingAgg <- aggregateLanding(landing, covariateNames, covariateDefinition=covariateDefinition)
		# Aggregate the rundvekt by covariates:
		landingAggregated <- by(landing$rundvekt, as.data.frame(covariateMatrixLanding, stringsAsFactors=FALSE), sum)
		# Combine into a data frame with covariates and the rundvekt in the last column:
		# Convert the dimnames to integer
		landingAggregated <- cbind(expand.grid(lapply(dimnames(landingAggregated), as.integer)), rundvekt=c(landingAggregated))
		# Discard empty covariate combinations:
		landingAggregated <- landingAggregated[!is.na(landingAggregated$rundvekt),]
		# Order by the covariates
		landingAggregated <- landingAggregated[do.call("order", landingAggregated[,-ncol(landingAggregated)]),]
		#############################################
	
	
		# Extract the hierarchy matrix from StoX (not implemented on 2017-02-23):
		
	
	
		###########################################
		##### (7) Covariate meta information: #####
		###########################################
		
		# Changes on 2018-10-03 / 2018-10-11:
		# As of Rstox 1.10 the covariate info (covType and CAR) are stored as a process data output generated by the function bioticCovData (maybe to be changed to landingCovData?), as the data frame 'covparam':
	

		# Add a data frame with meta information about the covariates:
		covType <- unlist(lapply(covariateNames, function(xx) getCovparam(projectName, "CovariateType")[[xx]]))
		CAR <- rep(NA, length(covType))
		# This process assigns TRUE to CAR only if the parameter 'ConditionalAutoRegression' exists and is equal to the string "true". All other values except empty values (NULL) implies FALSE. If the parameter 'ConditionalAutoRegression' is not present, NA is used:
		temp <- lapply(covariateNames, function(xx) getCovparam(projectName, "ConditionalAutoRegression")[[xx]] %in% TRUE)
		CAR[unlist(lapply(temp, length))>0] <- unlist(temp)
		 	# Make sure that CAR is a logical:
		CAR[is.na(CAR)] <- FALSE
		
		# Combine into the covariate info data frame:
		covariateInfo <- data.frame(
			Nlevels = Nlevels, 
			covType = covType, 
			CAR = CAR, 
			name = covariateNames, 
			description = covariateDescriptions, stringsAsFactors=FALSE
		)
		
		###########################################
	
		################################################
		##### (8) Fish age vs length-error matrix: #####
		################################################
		ageErrorData <- baselineOutput$proc$ageerror
		# Expand the AgeLength data to a sparse matrix:
		maxAge <- max(ageErrorData[,1:2])+1
		ageErrorMatrix <- matrix(0, ncol=maxAge, nrow=maxAge)
		ageErrorMatrix[as.matrix(ageErrorData[,1:2])+1] <- ageErrorData[,3]
		rownames(ageErrorMatrix) <- seq_len(maxAge)-1
		colnames(ageErrorMatrix) <- rownames(ageErrorMatrix)
		################################################
	
		############################################
		##### (9) Adjacent strata definitions: #####
		############################################
		# Get the stratum neighbours and convert to a list named with the strata names, and convert each element into a vector of stratum names:
		stratumNeighbour <- baselineOutput$proc$stratumneighbour
		stratumNeighbourList <- as.list(stratumNeighbour[,2])
		names(stratumNeighbourList) <- stratumNeighbour[,1]
		stratumNeighbourList <- lapply(stratumNeighbourList, function(xx) as.numeric(unlist(strsplit(xx, ","))))
		
		# Extract only the strata present in the data:
		stratumNeighbourList <- stratumNeighbourList[names(stratumNeighbourList) %in% covariateLink$spatial[,2]]
		
		# Remove also the neighbours that are not present:
		stratumNeighbourList <- lapply(stratumNeighbourList, function(x) x[x %in% names(stratumNeighbourList)])
		numNeighbours <- sapply(stratumNeighbourList, length)
		if(any(numNeighbours==0)){
			warning(paste0("Some strata have no neighbours present in the data (stratum ", paste(names(stratumNeighbourList)[numNeighbours==0], collapse=", "), ") Please add neighbors to these strata. The present neighbours are:\n", paste(covariateLink$spatial[,2], collapse=", ")))
		}
		
		
		
		### # Extract only the areas present in the data:
		### # 2018-02-01 Fixed bug when extracting only the spatial areas present in the project, where before the indices covariateLink$spatial[,2] were selected, but now we do a match:
		### #stratumNeighbourList <- stratumNeighbour[covariateLink$spatial[,2],]
		### stratumNeighbourList <- stratumNeighbour[stratumNeighbour[,1] %in% covariateLink$spatial[,2],,drop=FALSE]
		### # Change introduced on 2018-02-01: 
		### # Remove also the neighbours that are not present:
		### extractPresentAreasFromCommaSeparated <- function(x, valid){
		### 	if(is.character(x)){
		### 		x <- as.numeric(strsplit(x, ",")[[1]])
		### 		x <- intersect(x, valid)
		### 		paste(x, collapse=",")
		### 	}
		### 	else{
		### 		warning("The input must be a string of comma separated integers")
		### 		x
		### 	}
		### }
		### stratumNeighbourList[,2] <- sapply(stratumNeighbourList[,2], extractPresentAreasFromCommaSeparated, covariateLink$spatial[,2])
		### if(any(nchar(stratumNeighbourList[,2])==0)){
		### 	warning(paste0("Some strata have no neighbours present in the data (stratum ", paste(which(nchar(stratumNeighbourList[,2])==0), collapse=", "), ") Please add neighbors to these strata. The present neighbours are the following: ", paste(covariateLink$spatial[,2], collapse=",")))
		### }
		#############################################
		
		# Return all data in a list
		list(
			biotic = biotic, 
			landing = landing, 
			landingAggregated = landingAggregated, 
			covariateMatrixBiotic = covariateMatrixBiotic, 
			covariateMatrixLanding = covariateMatrixLanding, 
			ageError = ageErrorMatrix, # Moved from resources on 2018-01-25
			stratumNeighbour = stratumNeighbourList,  # Moved from resources on 2018-01-25
			resources = list(
				covariateInfo=covariateInfo, 
				covariateDefinition=covariateDefinition, 
				covariateLink=covariateLink
				)
			)
	}
	else{
		warning(paste0("The processes ", paste(c(biotic[1], landing[1]), collapse=" and "), "does not exist in the baseline model"))
		invisible(NULL)
	}
}


#
# Functions for coverting data from stox-structure to ECA structure
#

#' Function used for combining hard coded parameter values and user defeined parameter values
#' @keywords internal
getHardCoded <- function(info){
  
  hardcoded <- as.data.frame(matrix(
    c(
      #add constant
      "random", "constant", 0, 
      # Include slope for the constant:
      "in.slopeModel", "constant", 1
    ), 
    byrow=TRUE, ncol=3), stringsAsFactors=FALSE)
  hardcoded[,3] <- as.numeric(hardcoded[,3])
  hardcoded <- hardcoded[hardcoded[,2] %in% rownames(info),]
  
  for(i in seq_len(nrow(hardcoded))){
    info[hardcoded[i,2], hardcoded[i,1]] <- hardcoded[i,3]
  }
  
  return(info)
}

#' Function for setting getting appropriate value for the midSeason column in Landings
#' @keywords internal
getMidSeason <- function(x, tz="UTC", format="%d/%m/%Y"){
  x <- as.Date(strsplit(x, "-")[[1]], "%d/%m")
  x <- as.POSIXlt(x, tz=tz, format=format)
  yearday <- x$yday
  # Trick to get one day for "01/01-02/01"
  yearday[1] <- yearday[1] + 1
  mean(yearday)
}

#' Function used for extracting the correct covariate value from the inidces used in the covariate matrix passed to ECA:
#' @keywords internal
getCovariateValue <- function(index, eca, cov="season", type="biotic"){
  # Get first the covariate symbolic value (such as Q3 for "01/07-30/09"):
  posInLinkMatrix <- match(index, eca$resources$covariateLink[[cov]]$Numeric)
  symbol <- eca$resources$covariateLink[[cov]]$Covariate[posInLinkMatrix]
  # Then link to the value:
  posInDefinitionMatrix <- match(symbol, eca$resources$covariateDefinition[[cov]][[type]]$Covariate)
  value <- eca$resources$covariateDefinition[[cov]][[type]]$Value[posInDefinitionMatrix]
  value
}

#' Function for getting the mode of a vector:
#' @keywords internal
getMode <- function(x){
  as.numeric(names(table(x))[which.max(table(x))])
}

#' Function for extracting the GlobalParameters object
#' @keywords internal
getGlobalParameters <- function (eca, ecaParameters){
  #serialno is there only to enforce return type for getVar
  getnames <- c("lengthunitmeters", "serialno")
  usenames <- c("lengthresM", "samplingID")
  DataMatrix <- getVar(eca$biotic, getnames)
  names(DataMatrix) <- usenames
  
  lengthresM <- getMode(DataMatrix$lengthresM)
  lengthresCM <- lengthresM*100
  if(!all(DataMatrix$lengthresM == head(DataMatrix$lengthresM, 1))){
    warning(paste0("Several length resolusions applied in the data (", paste(table(DataMatrix$lengthresCM), collapse=", "), "). The mode (", lengthresCM,") used in the ECA"))
  }
  
  # Convert to centimeters in the lengthunit:
  Gparams <- list(lengthresCM=lengthresCM, 
                  resultdir=ecaParameters$resultdir, 
                  maxlength=ecaParameters$maxlength, 
                  minage=ecaParameters$minage, 
                  maxage=ecaParameters$maxage,
                  delta.age=ecaParameters$delta.age)
  
  return(Gparams)
}

#' Function for extracting the Landings object
#' @keywords internal
getLandings <- function(eca, ecaParameters){
  
  ### landingAggregated: ###
  numDaysOfYear <- 365
  warning("Re-implement setting of midseason once NR updates documentation.")
  landingAggregated <- cbind(constant=1, eca$landingAggregated, midseason=sapply(getCovariateValue(eca$landingAggregated$temporal, eca, cov="temporal", type="landing"), getMidSeason))
  landingAggregated$midseason <- landingAggregated$midseason / numDaysOfYear
  
  weight <- landingAggregated$rundvekt
  landingAggregated$rundvekt <- NULL
  landingAgeLength <- landingAggregated
  landingWeightLength <- landingAggregated
  
  ### Return a list of the data: ###
  landings <- list(AgeLengthCov=landingAgeLength, WeightLengthCov=landingWeightLength, LiveWeightKG=weight)
  return(landings)
}


#' Function for extracting the DataMatrix for the given variable ("age" or "weight", length is requested in both):
#' @keywords internal
getDataMatrixANDCovariateMatrix <- function(eca, vars=c("age", "yearday"), ecaParameters){
  
  #partcount
  
  # Define variables to include in the DataMatrix, where the variable specified in the input 'var' is included:
  getnames <- c("length", "serialno", "samplenumber", "lengthsamplecount", "lengthsampleweight", "catchweight", "otolithtype")
  usenames <- c("lengthCM", "samplingID", "partnumber", "samplecount", "sampleweight", "catchweight", "otolithtype")
  getnames <- c(vars, getnames)
  usenames <- c(vars, usenames)
  
  # Extract the data matrix:
  DataMatrix <- getVar(eca$biotic, getnames)
  names(DataMatrix) <- usenames
  
  #Estimate catch sample number: partcount
  DataMatrix$partcount <- DataMatrix$catchweight * DataMatrix$samplecount / DataMatrix$sampleweight
  #Drop columns only used for estimating partcount
  DataMatrix <- DataMatrix[, !(names(DataMatrix) %in% c("catchweight", "samplecount", "sampleweight"))]
  
  # Add first the samplingID to the object eca$covariateMatrixBiotic:
  CovariateMatrix <- eca$covariateMatrixBiotic
  # Add samplingID, which will be removed at the end:
  CovariateMatrix$samplingID <- DataMatrix$samplingID
  CovariateMatrix <- CovariateMatrix[!duplicated(CovariateMatrix$samplingID),]
  
  # Add the first column, which is only of ones:
  CovariateMatrix <- cbind(constant=1, CovariateMatrix)
  
  # Convert to 1, 2, 3, and implement an automatic function for this later:
  DataMatrix$samplingID <- match(DataMatrix$samplingID, CovariateMatrix$samplingID)
  CovariateMatrix$samplingID <- NULL
  
  return(list(DataMatrix=DataMatrix, CovariateMatrix=CovariateMatrix))
}

#' Function for extracting the CARNeighbours and info:
#' @keywords internal
getInfo <- function(eca, CovariateMatrix, ecaParameters){
  ### 3. info: ###
  ncov <- length(names(CovariateMatrix))
  Infonames <- c("random", "CAR", "continuous", "in.landings", "nlev", "interaction", "in.slopeModel")
  nInfo <- length(Infonames)
  info <- array(0L, dim=c(ncov, nInfo))
  #info <- info + seq_along(info)
  colnames(info) <- Infonames
  rownames(info) <- names(CovariateMatrix)
  
  # 3.1. random: 
  info[eca$resources$covariateInfo$name, "random"] <- eca$resources$covariateInfo$covType=="Random"
  
  # 3.2. CAR:
  # Make sure the neighbours are ordered according to the 1:n values in the covariateLink:
  ind <- match(as.numeric(names(eca$stratumNeighbour)), eca$resources$covariateLink$spatial[,2])
  if (!all(sort(ind)==ind)){
    stop("covariate values are ordered differently in stratumneighbour and covariatelink spatial")
  }
  names(eca$stratumNeighbour) <- eca$resources$covariateLink$spatial[ind,1]
  numNeighbours <- unlist(lapply(eca$stratumNeighbour, length), use.names = F)
  idNeighbours <- unlist(eca$stratumNeighbour, use.names=F)
  idNeighbours <- eca$resources$covariateLink$spatial[match(idNeighbours, eca$resources$covariateLink$spatial[, 2]), 1]
  CARNeighbours <- list(numNeighbours=numNeighbours, idNeighbours=idNeighbours)
  
  info[eca$resources$covariateInfo$name, "CAR"] <- eca$resources$covariateInfo$CAR
  
  # 3.3. continuous:
  if(length(ecaParameters$continuous)){
    info[names(ecaParameters$continuous), "continuous"] <- unlist(ecaParameters$continuous)
  }
  
  # 3.4. in.landings:
  info[rownames(info), "in.landings"] <- as.integer(rownames(info) %in% names(eca$landingAggregated))
  info["constant", "in.landings"] <- 1
  
  # 3.5. interaction:
  if(length(ecaParameters$interaction)){
    info[names(ecaParameters$interaction), "interaction"] <- unlist(ecaParameters$interaction)
  }
  
  # 3.6. include.slope:
  if(length(ecaParameters$in.slopeModel)){
    info[names(ecaParameters$in.slopeModel), "in.slopeModel"] <- unlist(ecaParameters$in.slopeModel)
  }
  
  info <- getHardCoded(info)
  # 3.7. nlev:
  info[rownames(info), "nlev"] <- apply(CovariateMatrix, 2, function(x) max(x))
  # Continuous covariates should have only one level:
  info[info[, "continuous"]==1, "nlev"] <- 1
  
  # random covariates should have levels equal to max of landing and max of observations (not sure if the latter is necessary
  if (sum(info[, "random"]==1 & info[, "in.landings"]==1)>0){
    for (n in rownames(info)){
      if (info[n, "random"]==1 & info[n, "in.landings"]==1){
        info[n,"nlev"] <- max(eca$landingAggregated[[n]], CovariateMatrix[[n]])				
      }
    }
  }
  
  return(
    list(
      info = info, 
      CARNeighbours = CARNeighbours
    )
  )
}

#' Function for converting to the input format required by ECA (this is the main function):
#' @keywords internal
getLengthGivenAge_Biotic <- function(eca, ecaParameters){
  
  # Extract the non-NAs:
  var <- "age"
  # Remove missing values from the DataMatrix and from the eca$covariateMatrixBiotic:
  valid <- !is.na(eca$biotic[[var]])
  eca$biotic <- eca$biotic[valid, , drop=FALSE]
  eca$covariateMatrixBiotic <- eca$covariateMatrixBiotic[valid, , drop=FALSE]
  
  ### 1. DataMatrix: ###
  temp <- getDataMatrixANDCovariateMatrix(eca, vars=c("age", "yearday"), ecaParameters)
  DataMatrix <- temp$DataMatrix
  CovariateMatrix <- temp$CovariateMatrix
  
  
  #DataMatrix <- getDataMatrix(eca, var=var, ecaParameters)
  
  # Estimate the real age by use of the hatchDaySlashMonth:
  numDaysOfYear <- 365
  DataMatrix$realage <- DataMatrix$age + (DataMatrix$yearday - getMidSeason(ecaParameters$hatchDaySlashMonth)) / numDaysOfYear
  DataMatrix$yearday <- NULL
  DataMatrix$part.year <- DataMatrix$realage - DataMatrix$age
  
  ### 2. CovariateMatrix: ###
  #CovariateMatrix <- getCovariateMatrix(eca, DataMatrix, ecaParameters)
  
  ### 3. info: ### 
  info <- getInfo(eca, CovariateMatrix, ecaParameters)
  
  
  ### Return a list of the data: ###
  out <- list(
    DataMatrix = DataMatrix, 
    CovariateMatrix = CovariateMatrix, 
    CARNeighbours = info$CARNeighbours, 
    AgeErrorMatrix = eca$ageError, 
    info = info$info, 
    resources = eca$resources
  )
  
  out$ClassificationErrorVector <- eca$otholiterror
  return(out)
}

#' Function for converting to the input format required by ECA (this is the main function):
#' @keywords internal
getWeightGivenLength_Biotic <- function(eca, ecaParameters){
  
  # Extract the non-NAs:
  var <- "weight"
  # Remove missing values from the DataMatrix and from the eca$covariateMatrixBiotic:
  valid <- !is.na(eca$biotic[[var]])
  eca$biotic <- eca$biotic[valid, , drop=FALSE]
  eca$covariateMatrixBiotic <- eca$covariateMatrixBiotic[valid, , drop=FALSE]
  
  ### 1. DataMatrix: ###
  temp <- getDataMatrixANDCovariateMatrix(eca, vars=var, ecaParameters)
  DataMatrix <- temp$DataMatrix
  CovariateMatrix <- temp$CovariateMatrix
  
  
  #DataMatrix <- getDataMatrix(eca, var=var, ecaParameters)
  # Hard code the weight to KG, since it is in grams in StoX:
  weightunit <- 1e-3
  DataMatrix <- cbind(weightKG=eca$biotic$weight * weightunit, DataMatrix)
  
  ### 2. CovariateMatrix: ###
  #CovariateMatrix <- getCovariateMatrix(eca, DataMatrix, ecaParameters)
  
  ### 3. info: ### 
  info <- getInfo(eca, CovariateMatrix, ecaParameters)
  
  ### Return a list of the data: ###
  out <- list(
    DataMatrix = DataMatrix, 
    CovariateMatrix = CovariateMatrix, 
    CARNeighbours = info$CARNeighbours, 
    #AgeErrorMatrix = eca$ageError, # This is not needed for WeightGivenLength
    info = info$info, 
    resources = eca$resources
  )
    out$ClassificationErrorVector <- eca$otholiterror
  return(out)
}

get_default_result_dir <- function(projectName, location=getProjectPaths(projectName)$RDataDir){
  return(file.path(location, "reca"))
}


#
# Functions for running RECA
#


#' @title Prepare data for RECA
#' @description Convert data to exported from stox to eca format and run tests on model configuration. Save results to project data 'prepareRECA'
#' @details Most parameters to this funciton are set as named members of a list which is passed as argument GlobalParameters to \code{\link[eca]{eca.estimate}}
#' @param projectName name of stox project
#' @param minage see specification for GlobalParameters in \code{\link[eca]{eca.estimate}}
#' @param maxage see specification for GlobalParameters in \code{\link[eca]{eca.estimate}}
#' @param delta.age see specification for GlobalParameters in \code{\link[eca]{eca.estimate}}
#' @param maxlength maximum length of fish in the data set in cm. If null the value will be extracted from the data.
#' @param hatchDaySlashMonth reference day for assumed spawning time of fish, formatted as day / month
#' @param resultdir location where R-ECA will store temporal files. Defaults (if null) to a subdirectory of getProjectPaths(projectName)$RDataDir called `reca` whcih will be created if it does not already exist
#' @export
prepareRECA <- function(projectName, resultdir=NULL, minage=1, maxage=20, delta.age=0.001, maxlength=NULL, hatchDaySlashMonth="01/01", use_otolithtype = NULL){
  warning("use_otolithtype is deprecated and has no effect.")
  if (is.null(resultdir)){
    resultdir <- get_default_result_dir(projectName)
    if(!(file.exists(resultdir))){
      dir.create(resultdir, recursive=T)
    }
  }
  if(!(file.exists(resultdir))){
    stop(paste("Directory", resultdir, "does not exist."))
  }
  warning("checking filepath char comp")
  if (grepl(" ", resultdir)){
    stop(paste("Please make ecadir", "(current:", resultdir, ") contain no spaces."))
  }
  
  eca <- baseline2eca(projectName)
  
  #max length in cm
  if (is.null(maxlength)){
    maxlength <- max(eca$biotic$length)      
  }
  #consider if it makes sense to extract from data for minage and maxage as well
  
  ecaParameters <- list(resultdir=resultdir, minage=minage, maxage=maxage, delta.age=delta.age, maxlength=maxlength, hatchDaySlashMonth=hatchDaySlashMonth)
  
  #
  # convert data
  #
  
  GlobalParameters <- getGlobalParameters(eca, ecaParameters)
  Landings <- getLandings(eca, ecaParameters)
  AgeLength <- getLengthGivenAge_Biotic(eca, ecaParameters)
  WeightLength <- getWeightGivenLength_Biotic(eca, ecaParameters)
  
  #
  # Run checks
  #
  
  checkAgeLength(AgeLength)
  checkWeightLength(WeightLength)
  checkCovariateConsistency(AgeLength, Landings$AgeLengthCov)
  checkCovariateConsistency(WeightLength, Landings$WeightLengthCov)
  checkLandings(Landings)
  checkGlobalParameters(GlobalParameters)
  
  #
  # store results
  #
  
  setProjectData(projectName=projectName, var=list(GlobalParameters=GlobalParameters, Landings=Landings, WeightLength=WeightLength, AgeLength=AgeLength, StoxExport=eca), name="prepareRECA")
}

#' @title run RECA 
#' @description Loads data produced by \code{\link{prepareRECA}} runs parameterization and makes predictions using RECA. Saves results to project data 'runRECA'
#' @details Most parameters to this function are appended to the argument list produced by prepareRECA and passed as argument GlobalParameters to \code{\link[eca]{eca.estimate}} and \code{\link[eca]{eca.predict}}.
#'     For purposes of testing and running ECA detached from StoX, Data files accepted by code{\link[eca]{eca.estimate}} and \code{\link[eca]{eca.predict}} can be exported using the option export_only. In this case the analysis is not run, but data files and parameter files are stored at the designated location.
#' @param projectName name of stox project
#' @param burnin see specification for GlobalParameters in \code{\link[eca]{eca.estimate}}
#' @param caa.burnin see specification for GlobalParameters in \code{\link[eca]{eca.predict}}
#' @param nSamples see specification for GlobalParameters in \code{\link[eca]{eca.estimate}}
#' @param thin see specification for GlobalParameters in \code{\link[eca]{eca.estimate}}
#' @param fitfile see specification for GlobalParameters in \code{\link[eca]{eca.estimate}}
#' @param predfile see specification for GlobalParameters in \code{\link[eca]{eca.predict}}
#' @param lgamodel see specification for GlobalParameters in \code{\link[eca]{eca.estimate}}
#' @param CC see specification for GlobalParameters in \code{\link[eca]{eca.estimate}}
#' @param CCError see specification for GlobalParameters in \code{\link[eca]{eca.estimate}}
#' @param seed see specification for GlobalParameters in \code{\link[eca]{eca.estimate}}
#' @param age.error see specification for GlobalParameters in \code{\link[eca]{eca.estimate}}
#' @param export_only if not NULL this indicates that eca should not be run, but all parameters should be exported to the file export_only
#' @export
runRECA <- function(projectName, burnin=100, caa.burnin=100, nSamples=1000, thin=1, fitfile="fit", predfile="pred", lgamodel="log-linear", CC=FALSE, CCError=FALSE, seed=NULL, age.error=FALSE, export_only=NULL){
  requireNamespace("eca")
  # Sett kjøreparametere her, sett dataparametere i prep_eca
  prepdata <- loadProjectData(projectName, var="prepareRECA")   
  prepareRECA <- prepdata$prepareRECA
  if (is.null(prepdata)){
    stop("Could not load project data")
  }
  GlobalParameters <- prepareRECA$GlobalParameters
  AgeLength <- prepareRECA$AgeLength
  WeightLength <- prepareRECA$WeightLength
  Landings <- prepareRECA$Landings
  
  GlobalParameters$caa.burnin <- burnin
  GlobalParameters$burnin <- caa.burnin
  GlobalParameters$nSamples <- nSamples
  GlobalParameters$thin <- thin
  GlobalParameters$fitfile <- fitfile
  GlobalParameters$predictfile <- predfile
  GlobalParameters$lgamodel <- lgamodel
  GlobalParameters$CC <- CC
  GlobalParameters$CCerror <- CCError
  GlobalParameters$age.error=age.error
  GlobalParameters$seed <- seed
  
  AgeLength <- AgeLength
  WeightLength <- WeightLength
  Landings <- Landings
  
  if (!is.null(export_only)){
    save(GlobalParameters, AgeLength, WeightLength, Landings, file=export_only)
  }
  else{
    ## Estimate model
    write("#########", stdout())
    write("Running ECA with model configuration:", stdout())
    write("Length given age model:", stdout())
    print(AgeLength$info)
    write(paste("individuals:", nrow(AgeLength$DataMatrix)), stdout())
    write("Weight given length model:", stdout())
    print(WeightLength$info)
    write(paste("individuals:", nrow(WeightLength$DataMatrix)), stdout())
    write("#########", stdout())
    
    fit <- eca::eca.estimate(AgeLength,WeightLength,Landings,GlobalParameters)
    
    ## Predict
    pred <- eca::eca.predict(AgeLength,WeightLength,Landings,GlobalParameters)
    
    setProjectData(projectName=projectName, var=list(fit=fit, pred=pred), name="runRECA")
  }
}

#
# Functions for plotting data and results from RECA
#

#' Generates plots and reports from RECA prediction
#' @param projectName name of stox project
#' @param verbose logical, if TRUE info is written to stderr()
#' @param format function defining filtetype for plots, supports grDevices::pdf, grDevices::png, grDevices::jpeg, grDevices::tiff, grDevices::bmp
#' @param ... parameters passed on plot function and format
#' @return list, with at least one named element 'filename', a vector of file-paths to generated plots.
#' @export
plotRECAresults <- function(projectName, verbose=F, format="png", ...){
  
  out <- list()
  out$filename <- c()
  
  rundata <- loadProjectData(projectName, var="runRECA")
  
  if(length(rundata)==0){
   return(NULL)
  }
  
  prep <- loadProjectData(projectName, var="prepareRECA")
  
  if (format=="png"){
    #dimension in pixels
    width=5000
    height=5000
    res=500
  }
  if (format=="pdf"){
    #dimension in inches
    width=10
    height=10
    res=NULL
  }
  
  fn <- formatPlot(projectName, "RECA_results", function(){plot_RECA_results_panel(rundata$runRECA$pred, prep$prepareRECA$StoxExport$biotic, ...)}, verbose=verbose, format=format, height=height, width=width, res=res, ...)
  out$filename <- c(fn, out$filename)
  warning("Implement save catch matrix")
  #save catch matrix
  return(out)
}

#' Generate plots for diagnosis of RECA model configuration. Compares sampling effort to fisheries along covariates selected in the model , and along some standard covariate choices if available (gear, temporal and spatial)
#' @param projectName name of stox project
#' @param verbose logical, if TRUE info is written to stderr()
#' @param format function defining filtetype for plots, supports grDevices::pdf, grDevices::png, grDevices::jpeg, grDevices::tiff, grDevices::bmp
#' @param ... parameters passed on plot function and format
#' @return list, with at least one named element 'filename', a vector of file-paths to generated plots.
#' @export
diagnosticsRECA <-
  function(projectName,
           verbose = T,
           format = "png",
           ...) {
    
    out <- list()
    out$filename <- c()
    
    prep <- loadProjectData(projectName, var = "prepareRECA")
	
    if(length(prep)==0){
     return(NULL)
    }
  
    stoxexp <- prep$prepareRECA$StoxExpor
    
    #
    # Coverage plot
    #
    if (all(c("gearfactor", "temporal", "spatial") %in% stoxexp$resources$covariateInfo$name)) {
      if (format == "png") {
        #dimension in pixels
        res = 500
        width = 5000
        height = 5000
      }
      if (format == "pdf") {
        #dimension in inches
        width = 10
        height = 10
        res = NULL
      }
      fn <- formatPlot(projectName, "RECA_cell_coverage", function() {
        diagnosticsCoverageRECA(stoxexp, ...)
      }, verbose = verbose, format = format, height = height, width = width, res =
        res, ...)
      out$filename <- c(fn, out$filename)
    }
    else{
      write(
        paste(
          "Need all",
          "gearfactor",
          "temporal",
          "spatial",
          "as covariates to produce RECA_cell_coverage"
        ),
        stderr()
      )
    }
    
    #
    # Samples by cells
    #
    if (all(c("gearfactor", "temporal", "spatial") %in% stoxexp$resources$covariateInfo$name)) {
      rows <-
        nrow(unique(get_gta_landings(stoxexp)[, c("gearfactor", "temporal")]))
      cols <- length(unique(get_gta_landings(stoxexp)$spatial))
      
      if (format == "png") {
        #dimension in pixels
        res = 500
        width = (res / 3) * (cols + 2)
        height = (res / 4) * (rows + 12)
      }
      if (format == "pdf") {
        #dimension in inches
        width = (cols + 2) / 3
        height = (rows + 12) / 4
        res = NULL
      }
      
      fn <- formatPlot(projectName, "RECA_samples_by_cells", function() {
        diagnosticsSamplesRECA(stoxexp, ...)
      }, verbose = verbose, format = format, height = height, width = width, res =
        res, ...)
      out$filename <- c(fn, out$filename)
    }
    else{
      write(
        paste(
          "Need all",
          "gearfactor",
          "temporal",
          "spatial",
          "as covariates to produce RECA_samples_by_cells"
        ),
        stderr()
      )
    }
    
    #
    # Fixed effect plot
    #
    
    #for testing different configs
    #stoxexp$resources$covariateInfo[2,"covType"] <- "Fixed"
    #stoxexp$resources$covariateInfo[3,"covType"] <- "Fixed"
    
    #calculate plot dimensions for table
    rows = nrow(get_fixed_effects_landings(stoxexp))
    cols = ncol(get_fixed_effects_landings(stoxexp)) + 2
    
    if (format == "png") {
      #dimension in pixels
      res = 500
      width = (res / 1.5) * (cols + 2) * 2
      height = (res / 4.9) * (rows + 10)
    }
    if (format == "pdf") {
      #dimension in inches
      width = (cols + 2) * 2 / 1.3
      height = (rows + 7) / 5
      res = NULL
    }
    
    fn <- formatPlot(projectName, "RECA_model_configuration", function() {
      diagnostics_model_configuration(stoxexp, ...)
    }, verbose = verbose, format = format, height = height, width = width, res =
      res, ...)
    out$filename <- c(fn, out$filename)
    
    
    if (format=="png"){
      #dimension in pixels
      width=5000
      height=5000
      res=500
    }
    if (format=="pdf"){
      #dimension in inches
      width=10
      height=10
      res=NULL
    }
    
    fn <- formatPlot(projectName, "RECA_sample_composition", function() {
      plotSampleCompositionRECA(stoxexp$biotic, ...)
    }, verbose = verbose, format = format, height = height, width = width, res =
      res, ...)
    out$filename <- c(fn, out$filename)
    
    return(out)
  }

#' Produces plots for R report for RECA. Fails silently on errors.
#' @param projectName name of stox project
#' @param ... arguments passed to \code{\link{diagnosticsRECA}} and \code{\link{plotRECAresults}}
#' @return list, with at least one named element 'filename', a vector of file-paths to generated plots.
#' @export
plotRECA <- function(projectName, ...){
  
  out <- list()
  out$filename <- c()
  
  tryCatch({
    fn <- diagnosticsRECA(projectName, ...)
    out$filename <- c(fn$filename, out$filename)
            },
           finally={
           }
  )
  
  tryCatch({
    fn <- plotRECAresults(projectName, ...)
    out$filename <- c(fn$filename, out$filename)
    },
    finally={
    }
  )
  return(out)
}
