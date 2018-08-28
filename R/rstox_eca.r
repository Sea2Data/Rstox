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
	covariateProcessesData <- c("temporal", "gearfactor", "spatial") # This is not used anywhere....
	
	# Get the baseline output:
	### baselineOutput <- getBaseline(projectName, input=c("par", "proc"), fun=c(biotic, landing))
	baselineOutput <- getBaseline(projectName, input=c("par", "proc"), proc=c(biotic, landing), ...)
	
	# Run if both biotic and landing data are present:
	if(all(c(biotic[1], landing[1]) %in% names(baselineOutput$out))){
	
		#####################################
		##### (1) Get raw landing data: #####
		#####################################
		
		# (1a) Get the data and convert variable names to lower case:
		landing <- baselineOutput$out[[landing[1]]]
		names(landing) <- tolower(names(landing))
		
		# 2018-08-28: Changed to using 'sistefangstdato' as per comment from Edvin:
		#landing <- addYearday(landing, datecar="formulardato", tz="UTC", format="%d/%m/%Y")
		landing <- addYearday(landing, datecar="sistefangstdato", tz="UTC", format="%d/%m/%Y")
		#####################################
	
		############################################################
		##### (2) Get raw biotic data with some modifications: #####
		############################################################
		# (2a) Get the data and convert variable names to lower case:
		biotic <- baselineOutput$out[[biotic[1]]]
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
		implementedCovariateNames <- c("temporal", "gearfactor", "spatial")
		implementedCovariateDescriptions <- c("The temporal covariate", "The gear covariate given as groups of gear codes", "The spatial covariate giving polygons or locations")
		implementedCovariateProcesses <- c("DefineTemporalLanding", "DefineGearLanding", "DefineSpatialLanding")
		browser()
	
		present <- which(implementedCovariateNames %in% names(biotic))
		covariateNames <- implementedCovariateNames[present]
		covariateDescriptions <- implementedCovariateDescriptions[present]
		covariateProcesses <- implementedCovariateProcesses[present]
		
		# (2c) Add yearday, year and month:
		biotic <- addYearday(biotic, datecar="startdate", tz="UTC", format="%d/%m/%Y")
	 
		# (2d) Hard code the lengthunits (from the sampling handbook). This must be changed in the future, so that lengthunitmeters is present in the biotic file:
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
		Nlevels <- sapply(allLevels, length)
		covariateMatrixBiotic <- sapply(seq_along(covariateNames), function(i) match(biotic[[covariateNames[i]]], allLevels[[i]]))
		covariateMatrixLanding <- sapply(seq_along(covariateNames), function(i) match(landing[[covariateNames[i]]], allLevels[[i]]))
		covariateMatrixBiotic <- as.data.frame(covariateMatrixBiotic, stringsAsFactors=FALSE)
		covariateMatrixLanding <- as.data.frame(covariateMatrixLanding, stringsAsFactors=FALSE)
		colnames(covariateMatrixBiotic) <- covariateNames
		colnames(covariateMatrixLanding) <- covariateNames
		
		
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
		# Add a data frame with meta information about the covariates:
		covType <- unlist(lapply(covariateProcesses, function(xx) baselineOutput$parameters[[xx]]$CovariateType))
		CAR <- rep(NA, length(covType))
		# This process assigns TRUE to CAR only if the parameter 'ConditionalAutoRegression' exists and is equal to the string "true". All other values except empty values (NULL) implies FALSE. If the parameter 'ConditionalAutoRegression' is not present, NA is used:
		temp <- lapply(covariateProcesses, function(xx) baselineOutput$parameters[[xx]]$ConditionalAutoRegression %in% TRUE)
		CAR[unlist(lapply(temp, length))>0] <- unlist(temp)
		 	# Make sure that CAR is a logical when covType is Random:
		CAR[is.na(CAR) & covType=="Random"] <- FALSE
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
			biotic=biotic, 
			landing=landing, 
			landingAggregated=landingAggregated, 
			covariateMatrixBiotic=covariateMatrixBiotic, 
			covariateMatrixLanding=covariateMatrixLanding, 
			ageError=ageErrorMatrix, # Moved from resources on 2018-01-25
			stratumNeighbour=stratumNeighbourList,  # Moved from resources on 2018-01-25
			resources=list(
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
