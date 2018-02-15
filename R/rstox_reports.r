#*********************************************
#*********************************************
#' Impute unknown individual biological parameters from known values
#'
#' This function fills in holes in individual fish samples (also called imputation).
#' In cases where individuals are not aged, missing biological variables (e.g "weight","age","sex", and "specialstage") are sampled from 
#' fish in the same length group at the lowest imputation level possible.
#'	imputeLevel = 0: no imputation, biological information exists
#'	imputeLevel = 1: imputation at station level; biological information is selected at random from fish within station
#'	imputeLevel = 2: imputation at strata level; no information available at station level, random selection within stratum
#'	imputeLevel = 3: imputation at survey level; no information available at lower levels, random selection within suvey
#'	imputeLevel = 99: no imputation, no biological information exists for this length group
#'
#' @param i		The index in the list of bootstrap iterations.
#' @param abnd	Abundance matrix with individual data
#' @param seedV	The seed vector for the random number generator, where element 'i' is picked out (this may seem strange, but is a consequence of the parallelability of the function, where 'i' is the primary parameter).
#'
#' @return Abundance matrix with imputed biological information 
#'
#' @importFrom stats density
#'
#' @export
#' @keywords internal
#' 
#distributeAbundance <- function(i=NULL, abnd, seedV=NULL, dotfile=NULL) {
distributeAbundance <- function(i=NULL, abnd, seedV=NULL) {
	# Function for replacing NAs by data given missing and replacement indices. This function treats individual columns to preserve the data type:
	imputeFromInd <- function(x, indMissing, indReplacement){
		if(length(indMissing)){
			for(i in unique(indMissing[,2])){
				# Get the row indices of missing and replacement values for the current column 'i':
				indMissingInCurrentCol <- indMissing[indMissing[, 2] == i, 1]
				indReplacementInCurrentCol <- indReplacement[indReplacement[, 2] == i, 1]
				x[indMissingInCurrentCol, i] <- x[indReplacementInCurrentCol, i]
			}
		}
		x
	}
	
	if(length(i)==1 && !"Row" %in% names(abnd)){
		abnd = abnd[[i]]
	}
	N <- nrow(abnd)
	
	# 2017-11-03: The sampling procedure throughout Rstox was changed to use the function sampleSorted(). In that function the vector to be sampled can be sorted before sampling. This requires a unique ID to sort:
	# Add a unique super individual ID:
	#superindID <- paste("cruise", b$cruise, "serialno", b$serialno, "aphia", b$aphia, "samplenumber", b$samplenumber, "no", b$no, sep="_")
	# Check the order of the Row information in 'abnd':
	if(!all(abnd$Row == seq_len(nrow(abnd)))){
		warning("The superindividual table is not ordered according to the Row information. Imputing may have different results from when the table i sorted.")
	}
	
	# Get the indices of known (with includeintotal==TRUE) and unknown ages:
	#knownAge <- !is.na(getVar(abnd, "age")) & getVar(abnd, "includeintotal") %in% TRUE
	#unknownAge <- is.na(getVar(abnd, "age"))
	atKnownAge <- which(!is.na(getVar(abnd, "age")) & getVar(abnd, "includeintotal") %in% TRUE)
	atUnknownAge <- which(is.na(getVar(abnd, "age")))
	NatKnownAge <- length(atKnownAge)
	NatUnknownAge <- length(atUnknownAge)
	# Bug discovered on 2017-12-12, where for no unknown ages NULL was returned for 'NumUsed' and onwards. Changed to returning NA:
	#imputeSummary <- vector("list", 7)
	imputeSummary <- as.list(rep(NA, 7))
	names(imputeSummary) <- c("NumNotAged", "NumAged", "NumUsed", "NumNoMatch", "NumImputedAtStation", "NumImputedAtStratum", "NumImputedAtSurvey")
	imputeSummary$NumAged <- NatKnownAge
	imputeSummary$NumNotAged <- NatUnknownAge
	
	# The following conditional expression was found obsolete after issuing the warning in imputeByAge() instead, based on the values of imputeSummary$NatKnownAge (no known ages) and imputeSummary$NumUsed (no unknown ages)
	### # Stop if no known ages and return if no unknown:
	### if(NatKnownAge == 0){
	### 	# Change made on 2017-09-15: Issuing an error here is deprecated. It should be a warning, but the imputing should continue. See note 2017-09-15 below:
	### 	# stop("No known ages")
	### 	# 2017-09-21: Ibrahim experienced a strange error " one node produced an error: replacement has 1 row, data has 0" when running impute on 4 cores over 50 bootstrap replicated. He will test with 5 replicates to see if the error is memory related.
	### 	# 2017-09-22: Having some problems merging develop into alpha... (typing this in to add a small changed in the process of fixing the problem.)
	### 	warning(paste0("No known ages in bootstrap replicate ", i))
	### }
	if(NatUnknownAge == 0){
		#warning(paste0("No unknown ages in bootstrap replicate ", i))
		# Add columns added to the bootstrap runs and return:
		temp <- integer(nrow(abnd))
		abnd$imputeLevel <- temp
		abnd$imputeRow <- temp
		abnd$imputeCount <- temp
		return(list(data=abnd, imputeSummary=imputeSummary, indMissing=NULL, indReplacement=NULL, seedM=NULL))
	}

	# Set the seed matrix:
	# Change introduced on 2017-11-14 by Holmin. To make the code more robust to changes, all generation of seeds has been moved to the functions setSeedSingle(), getSeedV(), getSeedM(), expandSeed():
	#if(isTRUE(seedV[i])){
	#	seedM <- matrix(c(1231234, 1234, 1234), nrow=NatUnknownAge, ncol=3, byrow=TRUE)
	#}
	#else{
	#	set.seed(seedV[i])
	#	# Create a seed matrix with 3 columns representing the replacement by station, stratum and survey:
	#	seedM <- matrix(sample(seq_len(10000000), 3*NatUnknownAge, replace = FALSE), ncol=3)
	#}
	seedM <- getSeedM(i, seedV=seedV, nrow=NatUnknownAge)
	
	########## HERE WE RATHER NEED TO CREATE LISTS OF INDICES FOR THE KNOWN INDIVIDUALS, AND GENERATE THE INIDCES IN THESE LISTS FOR EACH UNIQUE VALUE OF THE KNOWNS ##########
	
	# Run through the unknown rows and get indices for rows at which the missing data should be extracetd:
	#imputeRows <- rep("-", N)
	imputeRows <- rep(NA, N)
	imputeLevels <- integer(N)
	imputeCount <- integer(N)
	
	for(atUnkn in seq_along(atUnknownAge)){
		indUnkn <- atUnknownAge[atUnkn]
		# Get indices for which of the rows with known ages that have the same station, stratum and survey as the current unknown individual:
		matchStratum <- getVar(abnd, "Stratum")[indUnkn] == getVar(abnd, "Stratum")[atKnownAge]
		matchcruise <- getVar(abnd, "cruise")[indUnkn] == getVar(abnd, "cruise")[atKnownAge]
		matchserialno <- getVar(abnd, "serialno")[indUnkn] == getVar(abnd, "serialno")[atKnownAge]
		matchLenGrp <- getVar(abnd, "LenGrp")[indUnkn] == getVar(abnd, "LenGrp")[atKnownAge]
		#id.known.sta <- atKnownAge[ which(matchStratum & matchcruise & matchserialno & matchLenGrp) ]
		#id.known.stratum <- atKnownAge[ which(matchStratum & matchLenGrp) ]
		#id.known.survey <- atKnownAge[ which(matchLenGrp) ]
		# Get the indices of known individuals in the current station:
		id.known.sta <- atKnownAge[matchStratum & matchcruise & matchserialno & matchLenGrp]
		# Get the indices of known individuals in the current stratum:
		id.known.stratum <- atKnownAge[matchStratum & matchLenGrp]
		# Get the indices of known individuals in the wnrite survey:
		id.known.survey <- atKnownAge[matchLenGrp]
		Nid.known.sta <- length(id.known.sta)
		Nid.known.stratum <- length(id.known.stratum)
		Nid.known.survey <- length(id.known.survey)
												
		## Replace by station:
	 	if(any(id.known.sta)){
			#set.seed(seedM[atUnkn,1])
			
			# Change introduced on 2017-11-03, applying the function sampleSorted() for all sampling throughout Rstox in order to avoid dependency on the order of rows in the data:
			#imputeRows[indUnkn] <- id.known.sta[sample.int(Nid.known.sta, size=1)]
			#imputeRows[indUnkn] <- sampleSorted(id.known.sta, size=1, lx=Nid.known.sta, seed=seedM[atUnkn,1], sorted=FALSE)
			imputeCount[indUnkn] <- Nid.known.sta
			imputeRows[indUnkn] <- sampleSorted(id.known.sta, size=1, seed=seedM[atUnkn,1], sorted=FALSE)
			
			#imputeRows[indUnkn] <- id.known.sta[.Internal(sample(Nid.known.sta, 1L, FALSE, NULL))]
			imputeLevels[indUnkn] <- 1L
		}
		## Replace by stratum:
		else if(any(id.known.stratum)){
			#set.seed(seedM[atUnkn,2])
			
			# Change introduced on 2017-11-03, applying the function sampleSorted() for all sampling throughout Rstox in order to avoid dependency on the order of rows in the data:
			#imputeRows[indUnkn] <- id.known.stratum[sample.int(Nid.known.stratum, size=1)]
			#imputeRows[indUnkn] <- sampleSorted(id.known.stratum, size=1, lx=Nid.known.stratum, seed=seedM[atUnkn,2], sorted=FALSE)
			imputeCount[indUnkn] <- Nid.known.stratum
			imputeRows[indUnkn] <- sampleSorted(id.known.stratum, size=1, seed=seedM[atUnkn,2], sorted=FALSE)
			
			#imputeRows[indUnkn] <- id.known.stratum[.Internal(sample(Nid.known.stratum, 1L, FALSE, NULL))]
			imputeLevels[indUnkn] <- 2L
		}
		## Replace by survey:
		else if(any(id.known.survey)) {
			#set.seed(seedM[atUnkn,3])
			
			# Change introduced on 2017-11-03, applying the function sampleSorted() for all sampling throughout Rstox in order to avoid dependency on the order of rows in the data:
			#imputeRows[indUnkn] <- id.known.survey[sample.int(Nid.known.survey, size=1)]
			#imputeRows[indUnkn] <- sampleSorted(id.known.survey, size=1, lx=Nid.known.survey, seed=seedM[atUnkn,3], sorted=FALSE)
			imputeCount[indUnkn] <- Nid.known.survey
			imputeRows[indUnkn] <- sampleSorted(id.known.survey, size=1, seed=seedM[atUnkn,3], sorted=FALSE)
			
			#imputeRows[indUnkn] <- id.known.survey[.Internal(sample(Nid.known.survey, 1L, FALSE, NULL))]
			imputeLevels[indUnkn] <- 3L
		}
		else{
			imputeLevels[indUnkn] <- 99L
		}
	}
	
	abnd$imputeLevel <- imputeLevels
	abnd$imputeRow <- imputeRows
	abnd$imputeCount <- imputeCount
	
	# Create the following two data frames: 1) the rows of abnd which contain missing age and where there is age available in other rows, and 2) the rows with age available for imputing:
	missing <- abnd[atUnknownAge, , drop=FALSE]
	available <- abnd[imputeRows[atUnknownAge], , drop=FALSE]
	# 2017-09-15: If all imputeRows are NA (happend when NatKnownAge==0) a vector of only NAs are passed to [], which causes the full data frame to be returned with all values replaced ny NA. This causes a dimension mismatch between 'missing' and 'available', since 'available' gets the full dimension of 'abnd'. To account for this, we simply crop the 'available' to the dimensions of the 'missing'. This is a dirty fix, but sould work and allow for NatKnownAge=0, which previously resulted in an error: 
	if(nrow(available)>nrow(missing)){
		available <- available[seq_len(nrow(missing)), , drop=FALSE]
	}
	
	# Get the indices of missing data in 'missing' which are present in 'available':
	#ind <- which(missing == "-" & available != "-", arr.ind=TRUE)
	ind <- which(is.na(missing) & !is.na(available), arr.ind=TRUE)
	#indMissing <- cbind(missing$Row[ind[,1]], ind[,2])
	#indReplacement <- cbind(available$Row[ind[,1]], ind[,2])
	indMissing <- data.frame(Row=missing$Row[ind[,1]], Col=ind[,2])
	indReplacement <- data.frame(Row=available$Row[ind[,1]], Col=ind[,2])
	
	# Store process info:
	imputeSummary$NumUsed <- length(unique(indReplacement[,1]))
	#msg[3] <- sum(abnd$imputeLevel[atUnknownAge]==1)
	#msg[4] <- sum(abnd$imputeLevel[atUnknownAge]==2)
	#msg[5] <- sum(abnd$imputeLevel[atUnknownAge]==3)
	#msg[6] <- sum(abnd$imputeLevel[atUnknownAge]==99)
	imputeSummary$NumNoMatch <- sum(abnd$imputeLevel[atUnknownAge]==99)
	imputeSummary$NumImputedAtStation <- sum(abnd$imputeLevel[atUnknownAge]==1)
	imputeSummary$NumImputedAtStratum <- sum(abnd$imputeLevel[atUnknownAge]==2)
	imputeSummary$NumImputedAtSurvey <- sum(abnd$imputeLevel[atUnknownAge]==3)
	imputeSummary <- as.data.frame(imputeSummary)
	
	# Apply the replacement. This may be moved to the funciton imputeByAge() in the future to allow for using previously generated indices:
	abnd <- imputeFromInd(abnd, indMissing, indReplacement)
	#if(length(indMissing)){
	#	abnd[indMissing] <- abnd[indReplacement]
	#}
	
	#if(length(dotfile)){
	#	cat(".", file=dotfile, append=TRUE)
	#}
	#list(data=abnd, msg=msg, indMissing=indMissing, indReplacement=indReplacement, atUnknownAge=atUnknownAge, imputeRows=imputeRows, seedM=seedM)
	list(data=abnd, imputeSummary=imputeSummary, indMissing=indMissing, indReplacement=indReplacement, seedM=seedM)
}


#*********************************************
#*********************************************
#' Impute	missing individual data by age
#' 
#' Impute missing data within the bootstrap data object
#' 
#' @param projectName   The name or full path of the project, a baseline object (as returned from \code{\link{getBaseline}} or \code{\link{runBaseline}}, og a project object (as returned from \code{\link{openProject}}).
#' @param seed			The seed for the random number generator (used for reproducibility)
#' @param cores			An integer giving the number of cores to run the bootstrapping over.
#' @param saveInd		Logical: if TRUE save the imputing indices.
#' 
#' @return Updated list with imputed bootstrap results 
#'
#' @examples
#' projectName <- "Test_Rstox"
#' boot <- runBootstrap(projectName, nboot=10, seed=1, bootstrapMethod="AcousticTrawl")
#' # imputeByAge() fills in empty cells:
#' system.time(bootstrap_Acoustic_imputed <- imputeByAge(projectName))
#'
#' @importFrom data.table rbindlist
#' @importFrom parallel detectCores makeCluster parLapplyLB stopCluster
#' @importFrom utils tail
#'
#' @export
#' 
imputeByAge <- function(projectName, seed=1, cores=1, saveInd=TRUE){
	
	# Write a dot at each iteration to a textfile:
	#dotfile <- file.path(getProjectPaths(projectName)$RReportDir, "imputeProgress.txt")
	#write("", dotfile)
	
	# Read the saved data from the R model. In older versions the user loaded the file "rmodel.RData" separately, but in the current code the environment "RstoxEnv" is declared on load of Rstox, and all relevant outputs are assigned to this environment:
	imputeVariable <- getProjectData(projectName=projectName, var="bootstrap")
	
	nboot <- length(imputeVariable$SuperIndAbundance) ## The length of the data collection corresponds to the number of bootstrap iterations
	imputeSummary.out <- vector("list", nboot)
	indMissing.out <- vector("list", nboot)
	indReplacement.out <- vector("list", nboot)
	seedM.out <- vector("list", nboot)
	
	# Set the seed of the runs, either as a vector of 1234s, to comply with old code, where the seeed was allways 1234 (from before 2016), or as a vector of seeds sampled with the given seed, or as NULL, in which case the seed matrix 'seedM' of distributeAbundance() is set by sampling seq_len(10000000) without seed:
	# Change introduced on 2017-11-14 by Holmin. To make the code more robust to changes, all generation of seeds has been moved to the functions setSeedSingle(), getSeedV(), getSeedM(), expandSeed():
	#if(isTRUE(seed)){
	#	seedV = rep(TRUE, nboot+1) # seed==TRUE giving 1234 for compatibility with older versions
	#}
	#else if(is.numeric(seed)){
	#	set.seed(seed)
	#	seedV = sample(seq_len(10000000), nboot+1, replace=FALSE)
	#}
	#else{
	#	seedV = NULL
	#}
	seedV <- expandSeed(seed, nboot)
	
	
	
	# Store the bootstrap iteration names:
	namesOfIterations <- names(imputeVariable$SuperIndAbundance)
 
	# Impute biological information
	imputeVariable$base.SuperIndAbundance <- distributeAbundance(i=1, abnd=imputeVariable$base.SuperIndAbundance, seedV=tail(seedV,1))$data
	
	# Check available cores:	
	availableCores = detectCores()
	# If memory runs out, a system call to determine number of cores might fail, thus detectCores() could return NA
	# defaulting to single core if this is the case
	if(is.na(availableCores)) availableCores <- 1
	if(cores>availableCores){
		warning(paste0("Only ", availableCores, " cores available (", cores, " requested)"))
	}
	cores = min(cores, nboot, availableCores)
	# Generate the clusters of time steps:
	
	if(cores>1){
		cat(paste0("Imputing missing data (", nboot, " replicates, using ", cores, " cores in parallel):\n"))
		cl<-makeCluster(cores)
		# Bootstrap:
		out <- pblapply(seq_len(nboot), distributeAbundance, abnd=imputeVariable$SuperIndAbundance, seedV=seedV, cl=cl)
		# End the parallel bootstrapping:
		stopCluster(cl)
	}
	else{
		cat(paste0("Imputing missing data (", nboot, " replicates):\n"))
		out <- pblapply(seq_len(nboot), distributeAbundance, abnd=imputeVariable$SuperIndAbundance, seedV=seedV)
	}
		
	imputeVariable$SuperIndAbundance <- lapply(out, "[[", "data")
	# Add names ot the iterations:
	names(imputeVariable$SuperIndAbundance) <- namesOfIterations
	imputeSummary.out <- lapply(out, "[[", "imputeSummary")
	indMissing.out <- lapply(out, "[[", "indMissing")
	indReplacement.out <- lapply(out, "[[", "indReplacement")
	seedM.out <- lapply(out, "[[", "seedM")
	#
	#
	#names(imputeVariable$SuperIndAbundance) <- namesOfIterations
	#msg.out <- lapply(out, "[[", "msg")
	#indMissing.out <- lapply(out, "[[", "indMissing")
	#indReplacement.out <- lapply(out, "[[", "indReplacement")
	#seedM.out <- lapply(out, "[[", "seedM")	
		
	# imputeSummary.out <- t(as.data.frame(imputeSummary.out))
	# Using do.call("rbind", imputeSummary.out) did not result in a data frame on which the $ operator works. Instead we use data.table::rbindlist, but convert back to data frame:
	#imputeSummary.out <- do.call("rbind", imputeSummary.out)
	imputeSummary.out <- as.data.frame(data.table::rbindlist(imputeSummary.out))
	#colnames(msg.out) <- c("Aged", "NotAged", "ImputedAtStation", "ImputedAtStrata", "ImputedAtSurvey", "NotImputed")
	#colnames(imputeSummary.out) <- c("NumAged", "NumNotAged", "NumUsed", "NumNoMatch", "NumImputedAtStation", "NumImputedAtStratum", "NumImputedAtSurvey")
	#rownames(imputeSummary.out) <- paste0("Iter", seq_len(nboot))
	rownames(imputeSummary.out) <- names(imputeVariable$SuperIndAbundance)
	
	# Issue warnings for runs with no unknown, and no known ages:
	NatKnownAge0 <- which(imputeSummary.out$NatKnownAge==0)
	#NumUsedNA <- which(is.na(imputeSummary.out$NumUsed))
	if(length(NatKnownAge0)){
		warning("The following bootstrap runs had no known ages, resulting in no imputing: ", paste(NatKnownAge0, collapse=", "))
	}
	#if(length(NumUsedNA)){
	#	warning("The following bootstrap runs had no unknown ages, resulting in no imputing: ", paste(NumUsedNA, collapse=", "))
	#}
	
	
	# Store the output messages, the missing and replace indices, the seeds and other parameters of the imputing:
	imputeVariable$imputeParameters$imputeSummary <- imputeSummary.out
	imputeVariable$imputeParameters$seed <- seed
	imputeVariable$imputeParameters$seedV <- seedV
	imputeVariable$imputeParameters$seedM <- seedM.out
	imputeVariable$imputeParameters$nboot <- nboot
	imputeVariable$imputeParameters$cores <- cores
	# Add bootstrap methods:
	imputeVariable$imputeParameters$bootstrapMethod <- imputeVariable$bootstrapParameters$bootstrapMethod
	imputeVariable$imputeParameters$acousticMethod  <- imputeVariable$bootstrapParameters$acousticMethod 
	imputeVariable$imputeParameters$bioticMethod    <- imputeVariable$bootstrapParameters$bioticMethod   
	
	if(saveInd){
		imputeVariable$imputeParameters$indMissing <- indMissing.out
		imputeVariable$imputeParameters$indReplacement <- indReplacement.out
	}
	
	# Assign the data to the environment of the project:
	setProjectData(projectName=projectName, var=imputeVariable, name="bootstrapImpute")
	
	return(imputeSummary.out)
}


#*********************************************
#*********************************************
#' Interprets a unit key stings as a scaleing factor
#'
#' Given a key string such as "milliseconds" (possibly abbreviated) or the abbreviation "ms" (identical matching), the unit (here milliseconds) and scaling factor (here 1e-3) is returned.
#'
#' @param unit			A unit key string indicating the unit (see getPlottingUnit()$definitions$unlist.units for available key strings), or alternatively a numeric value giving the scaling factor.
#' @param var			A key string indicating the variable to plot (see getPlottingUnit()$defaults$Rstox_var for available values).
#' @param baseunit		The unit used in the data.
#' @param implemented	An integer vector giving the inplemented variables, which are the first two ("Abundance", "Weight") in the current version of Rstox.
#' @param def.out		Logical: if TRUE return also the defaults and definitions.
#' 
#' @return a list of the following four elements: 1. the scaling factor, 2. the unit string, 3. a matrix of the default values, and 4. a matrix of the defintions.
#'
#' @examples
#' getPlottingUnit(unit="milli", var="abund", baseunit="stox", def.out=FALSE)
#' getPlottingUnit(unit="milli", var="weight", baseunit="stox", def.out=FALSE)
#' getPlottingUnit(unit="hecto", var="weight", baseunit="stox")
#'
#' @export
#' @keywords internal
#' 
getPlottingUnit <- function(unit=NULL, var="Abundance", baseunit=NULL, implemented=c(1,2,3), def.out=TRUE){
	# Function used to get the index of the match of unit against the default units:
	getUnitInd <- function(unit, var, abbrev){
		# Check abbreviations first:
		ind <- if(is.numeric(unit)) which(as.numeric(abbrev[[var]]) == unit) else which(abbrev[[var]] == unit)
		# Then do abbreviated matching:
		if(length(ind)==0){
			ind <- which(abbrMatch(unit, units[[var]], ignore.case=TRUE)$hit)
		}
		# If still no match, use the default:
		if(length(ind)==0){
			unit <- Rstox_unit[[var]]
			warning(paste0("No match for the specified unit. Default used (", unit, ")"))
			ind <- which(abbrMatch(unit, units[[var]], ignore.case=TRUE)$hit)
		}
		ind
	}
	
	# Define variable, unit and base unit default vectors:
	Rstox_var <- c("Abundance", "Count", "Weight", "Length", "Time")[implemented]
	Rstox_unit <- c("millions", "millions", "tonnes", "meters", "seconds")[implemented]
	names(Rstox_unit) <- Rstox_var
	Rstox_baseunit <- c("1", "1", "grams", "centimeters", "seconds")[implemented]
	names(Rstox_baseunit) <- Rstox_var
	defaults <- data.frame(Rstox_var, Rstox_unit, Rstox_baseunit)
	# Define lists of allowed unit definitions, abbreviations and scaling factors to be matched with the inputs:
	units <- list(
		c( "ones", "tens", "hundreds", "thousands", "millions", "billions", "trillions" ),
		c( "ones", "tens", "hundreds", "thousands", "millions", "billions", "trillions" ),
		c( "micrograms", "milligrams", "grams", "hectograms", "kilograms", "tonnes" ),
		c( "micrometers", "millimeters", "centimeters", "decimeters", "meters", "kilometers" ),
		c( "microseconds", "milliseconds", "seconds", "minutes", "hours", "days" ) )[implemented]
	names(units) <- Rstox_var
	abbrev <- list(
		c( "1", "10", "100", "1000", "1e6", "1e9", "1e12" ),
		c( "1", "10", "100", "1000", "1e6", "1e9", "1e12" ),
		c( "mcg", "mg", "g", "hg", "kg", "t" ),
		c( "mcm", "mm", "cm", "dm", "m", "km" ),
		c( "mcs", "ms", "s", "m", "h", "d" ) )[implemented]
	names(abbrev) <- Rstox_var
	scale <- list(
		as.numeric(abbrev$Abundance),
		as.numeric(abbrev$Abundance),
		c( 1e-9, 1e-6, 1e-3, 1e-1, 1, 1e3 ),
		c( 1e-6, 1e-3, 1e-2, 1e-1, 1, 1e3 ),
		c( 1e-6, 1e-3, 1, 60, 60*60, 24*60*60 ) )[implemented]
	names(scale) <- Rstox_var
	definitions <- data.frame(unlist(units), unlist(abbrev), unlist(scale))
	
	# Get the variable by abbreviated matching:
	var <- abbrMatch(var[1], Rstox_var, ignore.case=TRUE)$string
	# Defalut var if missing:
	if(length(var)==0){
		warning(paste0("'var' not matched with any of the available values (", paste0(getPlottingUnit()$defaults$Rstox_var, collapse=", "), "). Default selected (", getPlottingUnit()$defaults$Rstox_var[1],")"))
		var <- getPlottingUnit()$defaults$Rstox_var[1]
	}
	# Defalut unit if missing:
	if(length(unit)==0){
		unit <- Rstox_unit[[var]]
	}
	if(length(baseunit)==0){
		baseunit <- Rstox_baseunit[[var]]
	}
	
	# Get matches:
	ind <- getUnitInd(unit, var, abbrev)
	baseind <- getUnitInd(baseunit, var, abbrev)
	unit.out <- units[[var]][ind]
	# Get the scaling factor between the base unit and requested unit:
	scale.out <- scale[[var]][ind] / scale[[var]][baseind]
	
	out <- list(scale=scale.out, unit=unit.out, baseunit=baseunit, var=var)
	if(def.out){
		out <- c(out, list(defaults=defaults, definitions=definitions))
	}
	return(out)
}


#*********************************************
#*********************************************
#' Plot NASC distribution to file
#'
#' Plots both original and resampled NASC distributions;
#' histogram of NASC transect means together with distribution of resampled NASC values (line).
#' Probability densities, component density, are plotted (so that the histogram has a total area of one).
#' Plot is exported to a tif- or png-file
#'
#' @param projectName   The name or full path of the project, a baseline object (as returned from \code{\link{getBaseline}} or \code{\link{runBaseline}}, og a project object (as returned from \code{\link{openProject}}).
#' @param format		The file format of the saved plot, given as a string naming the function to use for saving the plot (such as bmp, jpeg, png, tiff), with \code{filename} as its first argument. Arguments fo the functions are given as \code{...}. Dimensions are defaulted to width=5000, height=3000, resolution to 500 dpi. If \code{format} has length 0, the plot is shown in the graphics window, and not saved to file.
#' @param filetag			A character string to append to the file name (before file extension).
#' @param ...			Parameters passed on from other functions.
#' 
#' @return Plot saved to file 
#'
#' @examples
#' projectName <- "Test_Rstox"
#' # Run bootstrap before plotting:
#' boot <- runBootstrap(projectName, nboot=10, seed=1, bootstrapMethod="AcousticTrawl")
#' plotNASCDistribution(projectName)
#'
#' @export
#' 
plotNASCDistribution <- function(projectName, format="png", filetag=NULL, ...){
	# Get the parameters to send to the plotting function given by name in 'format':
	lll <- list(...)
	
	# Read the saved data from the R model. In older versions the user loaded the file "rmodel.RData" separately, but in the current code the environment "RstoxEnv" is declared on load of Rstox, and all relevant outputs are assigned to this environment:
	var <- c("psuNASC", "resampledNASC")
	projectEnv <- loadProjectData(projectName=projectName, var=var)
	
	# If any of the psuNASC and resampledNASCDistr are missing in the report environment, issue a warning:
	if(length(projectEnv)==0){
		return(NULL)
	}
	#else if(length(projectEnv) && !all(c("psuNASC", "resampledNASC") %in% ls(projectEnv))){
	if(length(projectEnv$psuNASC)==0 || length(projectEnv$resampledNASC)==0){
		#warning(paste0("At least one of the objects psuNASC and getResampledNASCDistr required to plot by the function plotNASCDistribution() are missing. These are generated by runBootstrap(, bootstrapMethod=\"AcousticTrawl\")"))
		return(NULL)
	}
	# Aggregate the NACS values:
	if(getVar(projectEnv$psuNASC, "LayerType")[1]!="WaterColumn"){
		agg <- aggPSUNASC(projectEnv$psuNASC)
	}
	else{
		agg <- projectEnv$psuNASC
	}
	
	# Define the file name and initiate the plot file:
	#filenamebase <- getProjectPaths(projectName)$RReportDir
	#filename <- file.path(filenamebase, paste("NASC_Distribution", format, sep="."))
	filenamebase <- file.path(getProjectPaths(projectName)$RReportDir, paste0(c("NASC_Distribution", filetag), collapse="_"))
	filename <- paste(filenamebase, format, sep=".")
	
	### if(startsWith(tolower(format), "tif")){
	### 	filename <- file.path(filenamebase, "NASC_Distribution.tif")
	### 	tiff(filename, res=600, compression = "lzw", height=5, width=5, units="in")
	### }
	### else if(startsWith(tolower(format), "png")){
	### 	filename <- file.path(filenamebase, "NASC_Distribution.png")
	### 	png(filename, width=800, height=600)
	### }
	### else{
	### 	filename <- NA
	### 	warning("Invalid format")
	### }
	### moveToTrash(filename)
	# If width and height is not given, default to width=5000, height=3000:
	if(!all(c("width", "height") %in% names(lll))){
		lll$width <- 5000
		lll$height <- 3000
		lll$res <- 500
	}
	
	if(length(format)){
		do.call(format, c(list(filename=filename), applyParlist(lll, format)))
		moveToTrash(filename)
	}
	
	# Run the plot
	out <- list()
	tryCatch(
		{
			# Changed to not show the project name, in order to avoid diffs between identical plots during automatic version testing:
			#out <- hist(getVar(agg, "Value"), breaks=20, freq=FALSE, xlab="NASC transect means", ylab="Relative frequency", main=projectName)
			out <- hist(getVar(agg, "Value"), breaks=20, freq=FALSE, xlab="NASC transect means", ylab="Relative frequency", main="")
			# Change introduced in the output from getResampledNASCDistr(), which form 2017-11-03 returns a list of elements NASC and seed:
			#d <- density(projectEnv$resampledNASC)
			d <- density(if(is.list(projectEnv$resampledNASC)) projectEnv$resampledNASC$NASC else projectEnv$resampledNASC)
			lines(d)
		}, 
		finally = {
			# safe closure of image resource inside finally block
			if(length(format)){
				dev.off()
			}
		}
	)
	
	out$filename <- filename
	out <- c(out, d)
	out
}


#*********************************************
#*********************************************
#' Plot abundance results to the graphics device or to a file
#' 
#' Plots boxplot of bootstrap results together with Coefficient of Variation (CV).
#' 
#' @param projectName   The name or full path of the project, a baseline object (as returned from \code{\link{getBaseline}} or \code{\link{runBaseline}}, og a project object (as returned from \code{\link{openProject}}).
#' @param bootstrapMethod	The bootstrap method used to generate the data.
#' @param var				A key string indicating the variable to plot (see getPlottingUnit()$defaults$Rstox_var for available values). For plotAbundance_SweptAreaTotal() \code{var} is hard coded to "Count"
#' @param unit				A unit key string indicating the unit, or alternatively a numeric value giving the scaling factor (run getPlottingUnit() to see available values).
#' @param baseunit			The unit used in the data.
#' @param grp1				Variable used to group results, e.g. "age", "LenGrp", "sex"
#' @param grp2				An optional second grouping variable
#' @param xlab				The label to user for the x axis, with default depending on data plotted,
#' @param ylab				The label to user for the y axis, with default depending on data plotted.
#' @param main				Main title for plot (text)
#' @param format			The file format of the saved plot, given as a string naming the function to use for saving the plot (such as bmp, jpeg, png, tiff), with \code{filename} as its first argument. Arguments fo the functions are given as \code{...}. Dimensions are defaulted to width=5000, height=3000, , resolution to 500 dpi. If \code{format} has length 0, the plot is shown in the graphics window, and not saved to file.
#' @param log				Character string giving the axes to apply log10() to, or TRUE to indicate log="y".
#' @param filetag			A character string to append to the file name (before file extension).
#' @param maxcv				The maximum cv in the plot. Use Inf to indicate the maximum cv of the data.
#' @param ...				Parameters passed on from other functions. Includes \code{numberscale}, which is kept for compability with older versions. Please use 'unit' instead. (Scale results with e.g. 1000 or 1000000).
#' 
#' @return Plot saved to file and abundance table printed
#'
#' @examples
#' projectName <- "Test_Rstox"
#' plotAbundance(projectName, grp1="age")
#' plotAbundance(projectName, grp1="age", unit=1)
#' 
#' @export
#' @rdname plotAbundance
#' 
plotAbundance <- function(projectName, bootstrapMethod="AcousticTrawl", var="Abundance", unit=NULL, baseunit=NULL, grp1="age", grp2=NULL, xlab=NULL, ylab=NULL, main="", format="png", log=NULL, filetag=NULL, ...){
	fun <- paste0("plotAbundance_", bootstrapMethod)
	do.call(fun, list(projectName=projectName, var=var, unit=unit, baseunit=baseunit, grp1=grp1, grp2=grp2, xlab=xlab, ylab=ylab, main=main, format=format, log=log, filetag=filetag, ...))
}
#'
#' @export
#' @import ggplot2
#' @keywords internal
#' @rdname plotAbundance
#'
plotAbundance_AcousticTrawl <- plotAbundance_SweptAreaLength <- function(projectName, var="Abundance", unit=NULL, baseunit=NULL, grp1="age", grp2=NULL, xlab=NULL, ylab=NULL, main="", format="png", maxcv=1, log=NULL, filetag=NULL, ...){
	
	# Get the parameters to send to the plotting function given by name in 'format':
	lll <- list(...)
	
	# The old parameter 'numberscale' is kept for backwards compatibility:
	if("numberscale" %in% names(lll)){
		warning("The argument numberscale is deprecated. Use the new argument 'unit' instead.")
		unit <- lll$numberscale
		lll$numberscale <- NULL
	}
	plottingUnit <- getPlottingUnit(unit=unit, var=var, baseunit=baseunit, def.out=FALSE)
	
	# Process the boostrap runs:
	temp <- reportAbundance(projectName, grp1=grp1, grp2=grp2, numberscale=plottingUnit$scale, plotOutput=TRUE, msg=FALSE)
	if(length(temp)==0){
		warning("No plots generated. Possibly due to mismatch between the parameter 'bootstrapMethod' in the bootstrapping and in the plotting function.")
	}
	
	outList <- list(filename=NULL, data=NULL)
	
	for(i in seq_along(temp)){
		level <- names(temp)[i]
		out <- temp[[i]]$abnd
		thisgrp1 <- temp[[i]]$grp1
		thisgrp2 <- temp[[i]]$grp2
		#grp1.unknown <- temp[[i]]$grp1.unknown
		abundanceSum <- temp[[i]]$abundanceSum
		
		# Set the missing values to low value (assuming only postive values are used for age and stratum and other variables):
		cat("Abundance by age for ", level, "\n", se0="")
		
		#abundanceSum[[thisgrp1]] <- setValueForMissing(abundanceSum[[thisgrp1]])
		#out[[thisgrp1]] <- setValueForMissing(out[[thisgrp1]])
		xlab <- paste(thisgrp1)
		abundanceSum[[thisgrp1]] <- factorNAfirst(abundanceSum[[thisgrp1]])
		out[[thisgrp1]] <- factorNAfirst(out[[thisgrp1]])
		#if(is.numeric(abundanceSum[[thisgrp1]])){
		#	levels <- seq(min(abundanceSum[[thisgrp1]], na.rm=TRUE), max(abundanceSum[[thisgrp1]], na.rm=TRUE), by=median(diff(sort(unique(abundanceSum[[thisgrp1]]))), na.rm=TRUE))
		#}
		#else{
		#	levels <- unique(abundanceSum[[thisgrp1]])
		#}
		
		
		#if(!is.empty(grp1)){
		#	abundanceSum[[grp1]] <- setValueForMissing(abundanceSum[[grp1]])
		#	out[[grp1]] <- setValueForMissing(out[[grp1]])
		#	xlab <- paste(grp1)
		#	xlim <- range(unique(abundanceSum[[grp1]]))
		#	levels <- seq(min(abundanceSum[[grp1]], na.rm=TRUE), max(abundanceSum[[grp1]], na.rm=TRUE), by=median(diff(sort(unique(abundanceSum[[grp1]]))), na.rm=TRUE))
		#}
		#else{
		#	xlab <- names(out)[1]
		#	xlim <- range(xlab)
		#	levels <- xlab
		#	grp1 <- xlab
		#}
		if(!is.empty(grp2)){
			#abundanceSum[[grp2]] <- setValueForMissing(abundanceSum[[grp2]])
			#out[[grp2]] <- setValueForMissing(out[[grp2]])
			xlab <- paste(thisgrp1, "by", grp2)
			abundanceSum[[thisgrp2]] <- factorNAfirst(abundanceSum[[thisgrp2]])
			out[[thisgrp2]] <- factorNAfirst(out[[thisgrp2]])
		}
	
		# Get ylab and xlab text:
		if(length(ylab)==0){
			ylab <- paste0(plottingUnit$var, " (", plottingUnit$unit, ")")
		}
		#if(is.empty(xlab) & !is.empty(grp2)){
		#	xlab <- paste(grp1,"by", grp2)
		#}
		#if(is.empty(xlab)){
		#	xlab <- paste(grp1)
		#}
	
		# Get file name:
		filenamebase <- file.path(getProjectPaths(projectName)$RReportDir, paste0(c(level, plottingUnit$var, thisgrp1, grp2, filetag), collapse="_"))
		filename <- paste(filenamebase, format, sep=".")
		
		# If width and height is not given, default to width=5000, height=3000:
		if(!all(c("width", "height") %in% names(lll))){
			lll$width <- 5000
			lll$height <- 3000
			lll$res <- 500
		}
		
		if(length(format)){
			do.call(format, c(list(filename=filename), applyParlist(lll, format)))
			moveToTrash(filename)
		}
		
	
		maxcv <- min(maxcv, max(out$Ab.Sum.cv, na.rm=TRUE))
		if(maxcv==0){
			maxcv <- 1
		}
		cvLabels <- pretty(c(0, maxcv))
		
		# Define the ylim, starting from the lower value of abundanceSum$Ab.Sum in the case of logarithmic plotting on the y axis:
		if(isTRUE(log)){
			log <- "y"
		}
		if("y" %in% log){
			ylim <- range(abundanceSum$Ab.Sum, na.rm=TRUE)
		}
		else{
			ylim <- c(0, max(abundanceSum$Ab.Sum, na.rm=TRUE))
		}
		#ylim <- c(0, max(abundanceSum$Ab.Sum, na.rm=TRUE))
		cvScalingFactor <- max(ylim) / maxcv
		# Add a tiny margin to each side of the range to avid values from missing out due to precision errors during scaling with 'cvScalingFactor':
		ylim[2] <- ylim[2] * (1 + 1e-12)
		outtmp <- out
		outtmp$Ab.Sum.cv <- outtmp$Ab.Sum.cv * cvScalingFactor
		
		tryCatch(
			{
				if(is.empty(grp2)){
					pl <- ggplot() + 
						geom_boxplot(data=abundanceSum, aes_string(x=thisgrp1, y="Ab.Sum"), outlier.shape=18) + 
						theme_bw() + 
						scale_x_discrete(drop=FALSE) + 
						 geom_line(aes_string(x=thisgrp1, y="Ab.Sum.cv", group=1), data=outtmp, show.legend=FALSE) + 
						geom_point(aes_string(x=thisgrp1, y="Ab.Sum.cv", group=1), data=outtmp, show.legend=FALSE)
				}	
				else{
					pl <- ggplot() + 
						geom_boxplot(data=abundanceSum, aes_string(x=thisgrp1, y="Ab.Sum", fill=grp2), outlier.shape=18) + 
						theme_bw() + 
						scale_x_discrete(drop=FALSE) + 
						scale_fill_discrete(name=grp2) + 
						 geom_line(aes_string(x=thisgrp1, y="Ab.Sum.cv", group=grp2, colour=out[[grp2]]), data=outtmp, show.legend=FALSE) + 
						geom_point(aes_string(x=thisgrp1, y="Ab.Sum.cv", group=grp2, colour=out[[grp2]]), data=outtmp, show.legend=FALSE)
					}
				pl <- pl + 
					#scale_y_continuous(limits=ylim, trans=if("y" %in% log) "log10" else "identity", sec.axis=sec_axis(~./cvScalingFactor, name="CV")) + 
					scale_y_continuous(trans=if("y" %in% log) "log10" else "identity", sec.axis=sec_axis(~./cvScalingFactor, name="CV")) + 
					coord_cartesian(ylim=ylim) + 
					xlab(xlab) +
					ylab(ylab) + 
					ggtitle(main)
					
			
				# Adjust text size and other theme variables:
				#pl + theme(applyParlist(lll, "theme"))
				pl + theme(axis.text=element_text(size=1.5), axis.title=element_text(size=2,face="bold"), , legend.text=element_text(size=2))
	
				#if("x" %in% log){
				#	pl <- pl + scale_x_log10()
				#}
				#if("y" %in% log){
				#	pl <- pl + scale_y_log10()
				#}
			
				# Activate the plot:
				suppressWarnings(print(pl))
			
				#if(startsWith(tolower(format), "tif")){
				#	dev.copy(tiff, filename=filename, res=600, compression="lzw", height=10, width=15, units="in")
				#}
			}, 
			finally = {
				# safe closure of image resource inside finally block
				if(length(format)){
					dev.off()
				}
			}
		)
		#system(paste0("open '" ,filename, "'"))
		outList$filename[[level]] <- filename
		outList$data[[level]] <- temp[[i]]
	}
}
#'
#' @export
#' @import ggplot2
#' @keywords internal
#' @rdname plotAbundance
#' 
plotAbundance_SweptAreaTotal <- function(projectName, unit=NULL, baseunit=NULL, xlab=NULL, ylab=NULL, main="", format="png", log=NULL, filetag=NULL, ...){
	
	# Get the parameters to send to the plotting function given by name in 'format':
	lll <- list(...)
	
	# The old parameter 'numberscale' is kept for backwards compatibility:
	if("numberscale" %in% names(lll)){
		warning("The argument numberscale is deprecated. Use the new argument 'unit' instead.")
		unit <- lll$numberscale
		lll$numberscale <- NULL
	}
	
	# Process the boostrap runs:
	temp <- reportAbundance_SweptAreaTotal(projectName, unit=unit, baseunit=baseunit)
	outList <- list(filename=NULL, data=NULL)
	
	for(i in seq_along(temp)){
		level <- names(temp)[i]
		abnd <- temp[[i]]$abnd
		# Get the var from the reportAbundance_SweptAreaTotal():
		plottingUnit <- getPlottingUnit(unit=unit, var=temp[[i]]$var, baseunit=baseunit, def.out=FALSE)
	
		# Get ylab and xlab text:
		if(length(ylab)==0){
			ylab <- paste0(plottingUnit$var, ", mean \u00B1 standard deviation (", plottingUnit$unit, ")")
		}
		xlab <- "SpecCat"
		
		# Get file name:
		#filenamebase <- file.path(getProjectPaths(projectName)$RReportDir, paste0(c(level, plottingUnit$var), collapse="_"))
		#filename <- paste(filenamebase, format, sep=".")
		filenamebase <- file.path(getProjectPaths(projectName)$RReportDir, paste0(c(level, plottingUnit$var, xlab, filetag), collapse="_"))
		filename <- paste(filenamebase, format, sep=".")
		
		# If width and height is not given, default to width=5000, height=3000:
		if(!all(c("width", "height") %in% names(lll))){
			lll$width <- 5000
			lll$height <- 3000
			lll$res <- 500
		}
		
		if(length(format)){
			do.call(format, c(list(filename=filename), applyParlist(lll, format)))
			moveToTrash(filename)
		}
		
		tryCatch(
			{
				x <- abnd
				x$Mean_minus_SD <- x$Mean - x$SD
				x$Mean_plus_SD <- x$Mean + x$SD
				pl <- ggplot(x, aes_string(x="SpecCat", y="Mean", color="SpecCat")) + 
					geom_point(data=x, aes_string(x="SpecCat", y="Mean", color="SpecCat"), size=2) + 
					geom_errorbar(aes_string(ymax="Mean_minus_SD", ymin="Mean_plus_SD"), position="dodge")
				
				#x <- cbind(SpecCat=rownames(abnd), abnd)
				#pl <- ggplot(x, aes(x=SpecCat, y=Mean, color=SpecCat)) + 
				#	geom_point(data=x, aes(x=SpecCat, y=Mean, color=SpecCat), size=2) + 
				#	geom_errorbar(aes(ymax=Mean + SD, ymin=Mean - SD), position="dodge")				
		
				pl <- pl + 
					xlab(xlab) +
					ylab(ylab) + 
					ggtitle(main)
				
				if("x" %in% log){
					pl <- pl + scale_x_log10()
				}
				if("y" %in% log){
					pl <- pl + scale_y_log10()
				}
				
				# Adjust text size and other theme variables:
				#pl + theme(applyParlist(lll, "theme"))
				pl + theme(axis.text=element_text(size=1.5), axis.title=element_text(size=2,face="bold"), , legend.text=element_text(size=2))
	
				# Activate the plot:
				suppressWarnings(print(pl))
			}, 
			finally = {
				# safe closure of image resource inside finally block
				if(length(format)){
					dev.off()
				}
			}
		)
		#system(paste0("open '" ,filename, "'"))
		outList$filename[[level]] <- filename
		outList$data[[level]] <- temp[[i]]
	}
}
#'
#' @export
#' @keywords internal
#' @rdname plotAbundance
#' 
factorNAfirst <- function(x){
	### if(is.numeric(x)){
	### 	levels <- seq(min(x, na.rm=TRUE), max(x, na.rm=TRUE), by=median(diff(sort(unique(x))), na.rm=TRUE))
	### }
	### else{
	### 	levels <- unique(x)
	### }
	### if(length(levels)==1 && is.na(levels)){
	### 	levels <- "NA"
	### 	x <- rep("NA", length(x))
	### }
	### else if(any(is.na(x))){
	### 	levels <- c(NA, levels[!is.na(levels)])
	### }
	### factor(x, levels=levels, exclude=FALSE)
	### #is.nax <- is.na(x)
	### #value <- if(is.character(x) || all(is.nax)) "-" else min(x, na.rm=TRUE) - 1
	### #x[is.nax] <- value
	### #x
	if(is.numeric(x)){
		levels <- seq(min(x, na.rm=TRUE), max(x, na.rm=TRUE), by=median(diff(sort(unique(x))), na.rm=TRUE))
		if(any(is.na(x))){
				levels <- c(NA, levels)
			}
	}
	else{
		levels <- unique(x)
	}
	if(length(levels)==1 && is.na(levels)){
		levels <- "NA"
		x <- rep("NA", length(x))
	}
	
	levels <- sort(levels, na.last=FALSE)
	
	factor(x, levels=levels, exclude=NULL)
}




#*********************************************
#*********************************************
#' Calculate a summary of the bootstrap iterations (possibly after imputing unknown ages).
#' 
#' \code{reportAbundance} is a wrapper function for the reportAbundance functions for the bootstrapMehtods "AcousticTrawl", "SweptAreaLength" and "SweptAreaTotal".
#' \code{reportAbundance_AcousticTrawl} reports and writes to file the abundance with uncertanty for the different groups given by \code{grp1} and (possibly) \code{grp2}.
#' \code{reportAbundance_SweptAreaLength} is an alias for \code{reportAbundance_AcousticTrawl}.
#' \code{reportAbundance_SweptAreaTotal} returns summary statistics generated from bootstrap replicates for projects with only total catch.
#' \code{reportAbundanceAtLevel} is used in \code{reportAbundance_AcousticTrawl} and \code{reportAbundance_SweptAreaLength} for generating the report for each level "bootstra" or "bootstrapImpute".
#' 
#' @param projectName   The name or full path of the project, a baseline object (as returned from \code{\link{getBaseline}} or \code{\link{runBaseline}}, og a project object (as returned from \code{\link{openProject}}).
#' @param bootstrapMethod	The method used when bootstrapping (see \code{\link{runBootstrap}}). This option selects different versions of functions such as \code{\link{reportAbundance}}}}. Note: Currently (Rstox_1.7) the report is equal for bootstrapMethod = "AcousticTrawl" and "SweptAreaLength".
#' @param var				A key string indicating the variable to plot (see getPlottingUnit()$defaults$Rstox_var for available values).
#' @param unit				A unit key string indicating the unit, or alternatively a numeric value giving the scaling factor (run getPlottingUnit() to see available values).
#' @param baseunit			The unit used in the data.
#' @param level				A string naming the process level (see getRstoxEnv()$processLevels for available levels).
#' @param grp1				Variable used to group results, e.g. "age", "LenGrp", "sex"
#' @param grp2				An optional second grouping variable
#' @param numberscale		Kept for compability with older versions. Use 'unit' instead. (Scale results with e.g. 1000 or 1000000).
#' @param plotOutput		Logical: if TRUE return a list of the recuired data in the function plotAbundance(). Otherwise, only the abundance data frame is returned.
#' @param write				Logical: if TRUE write the data to a tab-separated file.
#' 
#' @return A data frame of the abundance in sumary per grp2 and grp1 if plotOutput=FALSE, and a list holding this object (keeping "-" for missing values and not ordering) and other objects needed by plotAbundance().
#'
#' @examples
#' projectName <- "Test_Rstox"
#' reportAbundance(projectName, grp1=NULL, grp2=NULL)
#' reportAbundance(projectName, grp1="age", grp2=NULL)
#' reportAbundance(projectName, var="weight", grp1="age", grp2=NULL)
#' reportAbundance(projectName, grp1="age", grp2="Stratum")
#' reportAbundance(projectName, grp1="age", grp2="Stratum")
#'
#' @export
#' @rdname reportAbundance
#'
reportAbundance <- function(projectName, bootstrapMethod="AcousticTrawl", var="Abundance", unit=NULL, baseunit=NULL, grp1="age", grp2=NULL, numberscale=1e6, plotOutput=FALSE, write=FALSE, ...){
	fun <- paste0("reportAbundance_", bootstrapMethod)
	do.call(fun, list(projectName=projectName, var=var, unit=unit, baseunit=baseunit, grp1=grp1, grp2=grp2, numberscale=numberscale, plotOutput=plotOutput, write=write, ...))
}
#'
#' @export
#' @keywords internal
#' @rdname reportAbundance
#'
reportAbundance_SweptAreaLength <- function(projectName, var="Abundance", unit=NULL, baseunit=NULL, grp1="age", grp2=NULL, numberscale=1e6, plotOutput=FALSE, write=FALSE, msg=TRUE, ...){
	out <- list()
	for(level in getRstoxDef("processLevels")){
		out[[level]] <- reportAbundanceAtLevel(projectName, var=var, unit=unit, baseunit=baseunit, level=level, grp1=grp1, grp2=grp2, numberscale=numberscale, plotOutput=plotOutput, write=write)
	}
	out <- out[sapply(out, length)>0]
	if(msg && length(out)==0){
		warning("No reports generated. Possibly due to mismatch between the parameter 'bootstrapMethod' in the bootstrapping and in the report function.")
	}
	out
}
#'
#' @export
#' @keywords internal
#' @rdname reportAbundance
#'
reportAbundance_AcousticTrawl <- reportAbundance_SweptAreaLength
#'
#' @export
#' @keywords internal
#' @rdname reportAbundance
#' 
reportAbundance_SweptAreaTotal <- function(projectName, unit=NULL, baseunit=NULL, write=FALSE, ...){
	level <- "bootstrap"
	outlist <- vector("list", length(level))
	names(outlist) <- level
	projectEnv <- loadProjectData(projectName=projectName, var=level)
	
	plottingUnit <- getPlottingUnit(unit=unit, baseunit=baseunit, def.out=FALSE)
	plottingUnit$nboot <- projectEnv[[level]]$bootstrapParameters$nboot
	plottingUnit$seed <- projectEnv[[level]]$bootstrapParameters$seed
	plottingUnit$bootstrapMethod <- projectEnv[[level]]$bootstrapParameters$bootstrapMethod
	plottingUnit$acousticMethod <- paste(projectEnv[[level]]$bootstrapParameters$acousticMethod, collapse="~")
	plottingUnit$bioticMethod <- paste(projectEnv[[level]]$bootstrapParameters$bioticMethod, collapse="~")
	plottingUnit$var <- projectEnv[[level]]$bootstrapParameters$var
	
	units <- getPlottingUnit(unit=unit, baseunit=baseunit, def.out=FALSE)
	boot <- projectEnv[[level]]$TotalCatch
	if(length(boot)==0){
		warning(paste0("No object named TotalCatch in the project environment of project '", projectName, "'"))
		return(NULL)
	}
	boot <- do.call("rbind", boot) / plottingUnit$scale
	Variance <- apply(boot, 2, var)
	SD <- sqrt(Variance)
	Mean <- apply(boot, 2, mean)
	CV <- SD / Mean
	abnd <- data.frame(SpecCat=names(Variance), Variance, SD, Mean, CV)
	
	# Write the data to a tab-separated file:
	if(write){
		filename <- paste0(file.path(getProjectPaths(projectName)$RReportDir, paste0(c(level, plottingUnit$var), collapse="_")), ".txt")
		moveToTrash(filename)
		writeLines(paste(names(plottingUnit), unlist(plottingUnit), sep=": "), con=filename)
		suppressWarnings(write.table(abnd, file=filename, append=TRUE, sep="\t", dec=".", row.names=FALSE))
	}
	else{
		filename <- NULL
	}
	
	outlist[[level]] <- c(list(abnd=abnd, filename=filename), plottingUnit)
	
	outlist
}
#'
#' @import data.table
#' 
#' @export
#' @keywords internal
#' @rdname reportAbundance
#' 
reportAbundanceAtLevel <- function(projectName, var="Abundance", unit=NULL, baseunit=NULL, level="bootstrapImpute", grp1="age", grp2=NULL, numberscale=1e6, plotOutput=FALSE, write=FALSE){
	# Read the saved data from the R model. In older versions the user loaded the file "rmodel.RData" separately, but in the current code the environment "RstoxEnv" is declared on load of Rstox, and all relevant outputs are assigned to this environment:
	projectEnv <- loadProjectData(projectName=projectName, var=level)
	varInd <- abbrMatch(var[1], c("Abundance", "weight"), ignore.case=TRUE)
	
	# Combine all the bootstrap runs in one data table:
	DT <- rbindlist(projectEnv[[level]]$SuperIndAbundance, idcol=TRUE)
	if(sum(unlist(lapply(DT, length)))==0){
		#warning(paste0("The object ", level, " not present for project ", projectName))
		return(NULL)
	}
	
	# If grp1 is missing, replace it with all zeros:
	if(length(grp1)==0 || length(DT[[grp1]])==0){
		grp1 <- c("TSN", "TSB")[varInd$ind]
		#DT[[grp1]] <- integer(nrow(DT))
		DT[[grp1]] <- grp1
	}

	## Is any data in the grp1 input unkown?
	grp1.unknown <- TRUE
	
	#if(!any((DT[[grp1]]) == "-")){
	if(!any(is.na( DT[[grp1]] ))){
		#DT[[grp1]] <- as.numeric(DT[[grp1]]) 
		grp1.unknown <- FALSE 
	}
	if(!is.empty(grp2)){
		setkeyv(DT, cols=c(".id","Stratum", grp1, grp2))
	}
	else{
		setkeyv(DT, cols=c(".id","Stratum", grp1))
	}
	byGrp <- c(grp1, grp2, ".id")

	# Filter and sum by stratum:
	base <- projectEnv[[level]]$base.SuperIndAbundance
	strata <- unique(base$Stratum[base$includeintotal %in% TRUE])
	# Get the scaling factor for the plotting and the unit requested and the unit used in the data as strings:
	plottingUnit <- getPlottingUnit(unit=unit, var=var, baseunit=baseunit, def.out=FALSE)
	plottingUnit$nboot <- length(projectEnv[[level]]$SuperIndAbundance)
	plottingUnit$seed <- projectEnv[[level]]$bootstrapParameters$seed
	plottingUnit$bootstrapMethod <- projectEnv[[level]]$bootstrapParameters$bootstrapMethod
	plottingUnit$acousticMethod <- paste(projectEnv[[level]]$bootstrapParameters$acousticMethod, collapse="~")
	plottingUnit$bioticMethod <- paste(projectEnv[[level]]$bootstrapParameters$bioticMethod, collapse="~")
	
	# Sum the abundance or the product of abundance and weight (and possibly others in the future):
	# Declare the variables used in the DT[] expression below (this is done to avoid warnings when building the package):
	. <- NULL
	Ab.Sum <- NULL
	Abundance <- NULL
	Stratum <- NULL
	weight <- NULL
	if(varInd$ind==1){
		abundanceSumDT <- DT[Stratum %in% strata, .(Ab.Sum=sum(Abundance, na.rm=TRUE) / plottingUnit$scale), by=byGrp]
	}
	else if(varInd$ind==2){
		abundanceSumDT <- DT[Stratum %in% strata, .(Ab.Sum=sum(Abundance * weight, na.rm=TRUE) / plottingUnit$scale), by=byGrp]
	}
	else{
		warning(paste0("'var' does not match the available values (", getPlottingUnit()$defaults$Rstox_var, ")"))
	}
	
	abundanceSum <- as.data.frame(abundanceSumDT)
	unique_grp1 <- unique(abundanceSum[,grp1])
	unique_grp2 <- unique(abundanceSum[,grp2])

	setkeyv(abundanceSumDT, cols=c(".id", grp1))
	abundanceSumDT <- abundanceSumDT[CJ(unique(abundanceSumDT$.id), unique_grp1), allow.cartesian=TRUE]

	if(!is.empty(grp2)){
		setkeyv(abundanceSumDT, cols=c(".id", grp1, grp2))
		abundanceSumDT <- abundanceSumDT[CJ(unique(abundanceSumDT$.id), unique_grp1, unique_grp2), allow.cartesian=TRUE]
	} 
	
	abundanceSumDT$Ab.Sum[is.na(abundanceSumDT$Ab.Sum)] <- 0
	out <- abundanceSumDT[, .("Ab.Sum.5%" = quantile(Ab.Sum, probs=0.05, na.rm=TRUE),
					"Ab.Sum.50%" = quantile(Ab.Sum, probs=0.50, na.rm=TRUE),
					"Ab.Sum.95%" = quantile(Ab.Sum, probs=0.95, na.rm=TRUE),
					Ab.Sum.mean = mean(Ab.Sum, na.rm=TRUE),
					Ab.Sum.sd = sd(Ab.Sum, na.rm=TRUE),
					Ab.Sum.cv = sd(Ab.Sum, na.rm=TRUE) / mean(Ab.Sum, na.rm=TRUE)) 
					, by = c(grp1, grp2)]
							
	# Before, as.numeric() was added in as.data.frame() for some reason, but as.data.frame() should convert numerics to numeric properly:
	#out <- as.data.frame(lapply(out, as.numeric))
	out <- as.data.frame(out)
	orderFact1 <- if(length(grp1) && length(out[[grp1]])) out[[grp1]] else integer(nrow(out))
	orderFact2 <- if(length(grp2) && length(out[[grp2]])) out[[grp2]] else integer(nrow(out))
	out <- out[order(orderFact2, orderFact1, na.last=FALSE),]
	rownames(out) <- NULL
	
	### if(nrow(out)==1){
	### 	if(length(grp1)){
	### 		out[1] <- "-"
	### 	}
	### 	else{
	### 		out[1] <- c("TSN", "TSB")[varInd$ind]
	### 		rownames(out) <- out[1]
	### 		grp1 <- out[1]
	### 	}
	### 	#out <- out[,-1]
	### 	#out <- out[1] <- 
	### 	#rownames(out) <- c("TSN", "TSB")[varInd$ind]
	### }
	
	# Write the data to a tab-separated file:
	if(write){
		# Set temporary grp1 to NULL:
		#if(grp1=="temp"){
		#	grp1 <- NULL
		#}
		filename <- paste0(file.path(getProjectPaths(projectName)$RReportDir, paste0(c(level, plottingUnit$var, grp1, grp2), collapse="_")), ".txt")
		moveToTrash(filename)
		writeLines(paste(names(plottingUnit), unlist(plottingUnit), sep=": "), con=filename)
		suppressWarnings(write.table(out, file=filename, append=TRUE, sep="\t", dec=".", row.names=FALSE))
	}
	else{
		filename <- NULL
	}
	
	outlist <- c(list(abnd=out, filename=filename), plottingUnit)
	
	if(plotOutput){
		#outlist <- c(outlist, list(grp1.unknown=grp1.unknown, abundanceSum=abundanceSum, unique_grp1=unique_grp1, unique_grp2=unique_grp2, Ab.Sum=abundanceSumDT$Ab.Sum, abundanceSumDT=abundanceSumDT))
		#outlist <- c(outlist, list(grp1.unknown=grp1.unknown, abundanceSum=abundanceSum, unique_grp1=unique_grp1, unique_grp2=unique_grp2, Ab.Sum=abundanceSumDT$Ab.Sum))
		outlist <- c(outlist, list(grp1.unknown=grp1.unknown, abundanceSum=abundanceSum, Ab.Sum=abundanceSumDT$Ab.Sum, grp1=grp1, grp2=grp2))
	}
	outlist
}


#*********************************************
#*********************************************
#' Get various plots and reports in Rstox
#'
#' \code{getPlots} calls all or a subset of the plotting functions in Rstox (name starting with plot). \cr \cr
#' \code{getReports} calls all or a subset of the report functions in Rstox (name starting with report). \cr \cr
#'
#' @param projectName   The name or full path of the project, a baseline object (as returned from \code{\link{getBaseline}} or \code{\link{runBaseline}}, og a project object (as returned from \code{\link{openProject}}).
#' @param out			A string vector giving the plot or report functions to run. See getRstoxEnv()$keywords for available keywords.
#' @param options		A string vector holding the parameters passed on to the plotting functions. These parameters overrides identically named parameters in '...'. The parameters must be formatted as R expressions (as typed into an R console), and separated by semicolons (";"). See examples below:
#' \describe{
#'	\item{"Single string"}{"string = 'a string'"}
#'	\item{"String vector"}{"stringvec = c('red', 'blue', 'yellow2')"}
#'	\item{"Numeric"}{"num = 1.55"}
#'	\item{"Numeric vector"}{"numvec = c(1.4e-6, 16/3, runif(3))"}
#'	\item{"Logical"}{"ok = TRUE"}
#'	\item{"Logical vector"}{"okvec = c( TRUE, FALSE, T, F, {set.seed(0); runif(3)>0.3} )"}
#'	\item{"Array"}{"arr = array(runif(12), dim=3:4)"}
#'	\item{"Function"}{"fun1 = function(x) sin(rev(x))"}
#' }
#' All the examples in one string:
#' options = "string = 'a string'; stringvec = c('red', 'blue', 'yellow2'); num = 1.55; numvec = c(1.4e-6, 16/3, runif(3)); ok = TRUE; okvec = c( TRUE, FALSE, T, F, runif(3)>0.3 ); arr = array(runif(12), dim=3:4); fun1 = function(x) sin(rev(x)); fun3 = runif"
#' @param ...			Parameters passed on to the plotting functions.
#' 
#' @return A vector of file names of the plots or reports.
#'
#' @examples
#' # View all parameters of plotting functions:
#' sapply(getFunsRstox("plot"), function(x) names(formals(x)))
#' projectName <- "Test_Rstox"
#' # Get all plots:
#' getPlots(projectName)
#' # Get all reports:
#' getReports(projectName)
#' # Get a specific plot:
#' getPlots(projectName, out="plotNASCDistribution")
#'
#' @export
#' @rdname getPlots
#' 
getPlots <- function(projectName, out="all", options="", ...){
	runFunsRstox(projectName, string="plot", out=out, options=options, ...)
}
#'
#' @export
#' @rdname getPlots
#' 
getReports <- function(projectName, out="all", options="", ...){
	runFunsRstox(projectName, string="report", out=out, options=options, write=TRUE, all.out=TRUE, ...)
}


#*********************************************
#*********************************************
#' Get various plots and reports in Rstox
#'
#' \code{runFunsRstox} Runs all functions starting with the input parameter \code{string}. \cr \cr
#' \code{getFunsRstox} Gets all functions starting with the input parameter \code{string}. \cr \cr
#'
#' @param projectName   The name or full path of the project, a baseline object (as returned from \code{\link{getBaseline}} or \code{\link{runBaseline}}, og a project object (as returned from \code{\link{openProject}}).
#' @param string		A string giving the first characters of the functions to run, such as "plot" or "report".
#' @param out			A string vector giving the plot or report functions to run. See getRstoxEnv()$keywords for available keywords.
#' @param options		A string vector holding the parameters passed on to the plotting functions. These parameters overrides identically named parameters in '...'. The parameters must be formatted as R expressions (as typed into an R console), and separated by semicolons (";"). See examples below:
#' \describe{
#'	\item{"Single string"}{"string = 'a string'"}
#'	\item{"String vector"}{"stringvec = c('red', 'blue', 'yellow2')"}
#'	\item{"Numeric"}{"num = 1.55"}
#'	\item{"Numeric vector"}{"numvec = c(1.4e-6, 16/3, runif(3))"}
#'	\item{"Logical"}{"ok = TRUE"}
#'	\item{"Logical vector"}{"okvec = c( TRUE, FALSE, T, F, {set.seed(0); runif(3)>0.3} )"}
#'	\item{"Array"}{"arr = array(runif(12), dim=3:4)"}
#'	\item{"Function"}{"fun1 = function(x) sin(rev(x))"}
#' }
#' All the examples in one string:
#' options = "string = 'a string'; stringvec = c('red', 'blue', 'yellow2'); num = 1.55; numvec = c(1.4e-6, 16/3, runif(3)); ok = TRUE; okvec = c( TRUE, FALSE, T, F, runif(3)>0.3 ); arr = array(runif(12), dim=3:4); fun1 = function(x) sin(rev(x)); fun3 = runif"
#' @param all.out		Logical: if TRUE return all data from the functions, and otherwise only return file names.
#' @param drop.out		Should the list of be dropped if only one funciton is run?
#' @param ...			Parameters passed on to the plotting functions.
#' 
#' @return \code{runFunsRstox} returns a the outputs from alle the functions run, whereas \code{getFunsRstox} returns a vector of functions.
#'
#' @export
#' @keywords internal
#' @rdname runFunsRstox
#' 
runFunsRstox <- function(projectName, string, out="all", options="", all.out=FALSE, drop.out=TRUE, ...){
	# Function for extracting the parameters given in the options text string:
	getOptionsText <- function(options){
		# Test first using commas and semi colons:
		options <- unlist(strsplit(options, ";", fixed=TRUE))
		temp <- unlist(strsplit(options, ",", fixed=TRUE))
		commaOK <- all(sapply(gregexpr("=", temp), function(x) sum(x)>0))
		if(commaOK){
			options <- temp
		}
		# Split into single parameter definitions:
		#options <- strsplit(options, ";", fixed=TRUE)[[1]]
		# Get parameter names:
		optionsNames <- gsub("=.*", "", options)
		optionsNames <- gsub("[[:blank:]]", "", optionsNames)
		# Evaluate parameter expressions:
		options <- lapply(options, function(x) eval(parse(text=x)))
		names(options) <- optionsNames
		options
	}
	
	# Get the available functions:
	funs <- getFunsRstox(string=string, out=out)
	
	# Get the parameters
	dotlist <- list(...)
	if(nchar(options)>0){
		# Merge with '...', where 'options' overrides '...':
		dotlist <- c(getOptionsText(options), dotlist)
	}
	
	# Remove duplicates:
	dotlist <- dotlist[!duplicated(names(dotlist))]
	
	# Run functions and return outputs:
	if(all.out){
		#out <- lapply(funs, function(xx) {cat("... running", xx, "...\n"); do.call(xx, c(list(projectName=projectName), dotlist))})
		out <- lapply(funs, function(xx) do.call(xx, c(list(projectName=projectName), dotlist)))
	}
	else{
		#out <- lapply(funs, function(xx) {cat("... running", xx, "...\n"); do.call(xx, c(list(projectName=projectName), dotlist))$filename})
		out <- lapply(funs, function(xx) do.call(xx, c(list(projectName=projectName), dotlist))$filename)
	}
	
	
	out <- out[unlist(lapply(out, length))>0]
	
	
	if(drop.out && length(out)==1){
		return(out[[1]])
	}
	else{
		return(out)
	}
}
#'
#' @export
#' @keywords internal
#' @rdname runFunsRstox
#' 
getFunsRstox <- function(string, out="all"){
	# Function for selecting an element based on the keyword:
	applyKeyword <- function(x, keyword){
		namesx <- names(x)
		hit <- which(startsWith(names(x), keyword))
		if(length(hit)==0){
			warning(paste0("Keyword (", keyword, ") matching none of the valid keywords (", paste(namesx, collapse=", ")), ")")
		}
		else if(length(hit)>1){
			warning(paste0("Keywords (", paste(keyword, ", "), ") matching several of the valid keywords (", paste(namesx, collapse=", ")), "). First selected.")
			hit <- hit[1]
		}
		hit
	}
	
	# Define the valid functions:
	#funs <- list(
	#	plots = list(
	#		all = c("plotAbundance", "plotNASCDistribution")
	#		),
	#	reports = list(
	#		all = c("reportAbundance")
	#		)
	#	)
	funs <- list(
		plots = list(
			all = c("plotAbundance", "plotNASCDistribution")
			),
		reports = list(
			all = c("reportAbundance")
			)
		)
	# Select the specified functions:
	hit <- applyKeyword(funs, string)
	outFuns <- funs[[hit]]
	hit <- applyKeyword(outFuns, out)
	outFuns <- outFuns[[out]]
	return(outFuns)
}


#*********************************************
#*********************************************
#' Generate species matrix with biotic stations as rows and station information and the species present in the \code{ref} file as columns. One matrix generated per variable \code{var}.
#'
#' @param projectName						The name or full path of the project, a baseline object (as returned from \code{\link{getBaseline}} or \code{\link{runBaseline}}, og a project object (as returned from \code{\link{openProject}}).
#' @param ref								The path to a file linking species to species categories, specifically named by \code{specVar} or \code{specVar.ref} and \code{catVar}. The file must be a CSV (comma separated) file with UTF-8 encoding.
#' @param specVar,specVar.bio,specVar.ref	Names of the species columns in the data and in the \code{ref} file. When only specifying \code{specVar}, a common column name is assumed.
#' @param catVar							The name of the species category column in the \code{ref} file.
#' @param bioticProc						The process from which the biotic data should be extracted from the project.
#' @param stationVar						The names of the columns of the biotic data identifying the biotic stations. These columns will be concatenated.
#' @param var								The names of the columns for which the species matrices should be generated.
#' @param na.as								The value to use for missing data (e.g., species categories that are not present in a station).
#' @param drop.out							Logical: If TRUE (default) unlist if only one variable is given in \code{var}.
#' @param close								Logical: If TRUE (default) close the project after reading the data.
#' @param ...								Parameters passed on to getBaseline(), e.g. for changing the baseline parameters (adding filters).

#' 
#' @return A list of matrices (dropped to a matrix if only one variable is specified in \code{var}) with stations as rows and station information and the species present in the \code{ref} file as columns.
#'
#' @export
#' @keywords internal
#' @rdname aggregateBySpeciesCategory
#' 
aggregateBySpeciesCategory <- function(projectName, ref, specVar="noname", specVar.bio=specVar, specVar.ref=specVar, catVar="SpecCat", bioticProc="FilterBiotic", stationVar=c("cruise", "serialno"), var=c("weight", "count"), na.as=0, drop.out=TRUE, close=TRUE, msg=FALSE, ...){
	# Function used for converting a matrix into a data frame and appending the rownames as the first column:
	createTempDataFrame <- function(x){
		out <- data.frame(stationVar=rownames(x), as.data.frame(x))
		names(out) <- c(stationVar, colnames(x))
		out
	}
	# Add concatination of 
	appendStationFirst <- function(x, stationVar=c("cruise", "serialno")){
		Station <- do.call(paste, x[stationVar])
		cbind(Station=Station, x)
	}
	
	# Read the ref file:
	ref <- read.csv2(ref, stringsAsFactors=FALSE)
	ref[[specVar]] <- tolower(ref[[specVar]])
	# Test whether the specVar and catVar are present in the ref file:
	if(!all(c(specVar.ref, catVar) %in% names(ref))){
		stop("All of 'specVar.ref' (possibly specified commonly for the biotic data and the ref file by 'specVar') and 'catVar' must be present in the 'ref' file")
	}
	
	# Get the project output:
	if(msg){
		cat("Generating species matrix for project ", projectName, "\n")
	}
	g <- getBaseline(projectName, input=NULL, proc=bioticProc, ...)
	closeProject(projectName)
	
	# Merge station and catch data from the bioticProc, but keep all stations through all = TRUE:
	out <- g$FilterBiotic_BioticData_FishStation.txt
	out <- appendStationFirst(out, stationVar=stationVar)
	data <- g$FilterBiotic_BioticData_CatchSample.txt
	data <- appendStationFirst(data, stationVar=stationVar)
	availableVars <- names(data)
	
	if(!specVar.bio %in% names(data)){
		stop("'specVar.bio' (possibly specified commonly for the biotic data and the ref file by 'specVar') must be present in the biotic data of the project")
	}

	# Convert to lower case for the species, as per protocol of StoX.
	data[[specVar]] <- tolower(data[[specVar]])
	# Remove any columns named by 'catVar':
	data <- data[, names(data) != catVar]
	# Append the ref file to the data, and keep all species in the reference file:
	data <- merge(data, ref, by=specVar, all.y=TRUE)
	# Get only the the variables specified by 'var':
	if(!all(var %in% names(data))){
		var <- intersect(var, availableVars)
		warning(paste0(if(length(var)==0) "None" else "Not all", " of the variables given by 'var' are present in the data. Available columns are ", paste(availableVars, collapse=", ")))
	}
	data <- data[, c("Station", catVar, var)]
	
	# Group by catVar:
	temp <- lapply(var, function(v) tapply(data[, v], data[, c("Station", catVar)], sum, na.rm=TRUE))
	# Convert into a data frame and merge by 'stationVar':
	#temp <- lapply(temp, createTempDataFrame)
	temp <- lapply(temp, function(x) data.frame(Station=rownames(x), as.data.frame(x)))
	# replace NA:
	temp <- lapply(temp, function(x) {x[is.na(x)] <- na.as; x})
	
	# Merge with the station data:
	temp <- lapply(temp, function(x) merge(out, x, by="Station", all.x=TRUE))
	names(temp) <- var
	if(drop.out && length(temp)==1){
		temp <- temp[[1]]
	}
	
	return(temp)
}
