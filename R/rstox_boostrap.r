#*********************************************
#*********************************************
#' Run one bootstrap iteration of biotic stations and acoustic data 
#'
#' This function is used in bootstrapParallel().
#'
#' @param i				The boostrap iteration number.
#' @param projectName   The name or full path of the project, a baseline object (as returned from getBaseline() or runBaseline()), og a project object (as returned from open).
#' @param assignments	Trawl assignment from baseline.
#' @param strata		The strata of the survey.
#' @param psuNASC		MeanNASC from baseline.
#' @param stratumNASC	Strata NASC estimates from getNASCDistr(baseline).
#' @param resampledNASC	Resampled NASC distribution.
#' @param startProcess	The start process of the bootstrapping, being the first process before which biostations has been assigned and NASC values have been calculated.
#' @param endProcess	The end process of the bootstrapping, being the process returning a matrix containing the following columns: "Stratum", "Abundance", "weight", and grouping variables such as "age", "SpecCat", "sex".
#' @param seedV			A vector of seeds. seedV[i] is used.
#'
#' @return list with (1) the abundance by length in the bootstrap run, (2) the abundance by super individuals in the bootstrap run
#'
#' @export
#' @keywords internal
#'
bootstrapOneIteration <- function(i, projectName, assignments, strata, psuNASC=NULL, stratumNASC=NULL, resampledNASC=NULL, startProcess="TotalLengthDist", endProcess="SuperIndAbundance", seedV=NULL){
	
	# Load Rstox if not already loaded:
	library(Rstox)
	# Get the baseline object (run if not already run), as this is needed to insert biostation weighting and meanNASC values into. The warningLevel = 1 continues with a warning when the baseline encounters warnings:
	baseline <- runBaseline(projectName=projectName, out="baseline", msg=FALSE, warningLevel=1)
	
	# Perform sampling drawing and replacement by stratum
	BootWeights <- data.frame()
	# Not effective if psuNASC has length 0:
	meanNASC <- psuNASC
	# Loop per strata:
	for(j in 1:length(strata)){
		# Get all stations with assignment to one or more PSUs in the current stratum:
		stations <- unique(getVar(assignments, "StID")[getVar(assignments, "Stratum")==strata[j]])
		# Change suggested for a problem with strata with no stations. The change was discarded, since there should be stations in all strata:
		if(length(stations)==0){
			warning(paste("No biotic stations in stratum", j))
			next
		}
		set.seed(seedV[i])
		# Resample BioStation:
		StID <- sample(stations, replace = TRUE)
		# Count weights from resample:
		count <- as.data.frame(table(StID))
		count$Stratum <- strata[j]
		BootWeights <- rbind(BootWeights,count)
		
		# Find NASC scaling factor. This is not directly related to the sampling of biotic stations above. The NASC values have actually been resampled outside of this function, in the resampledNASC <- getResampledNASCDistr() command in runBootstrap():
		if(length(psuNASC)>0){
			sm <- stratumNASC$NASC.by.strata$strata.mean[stratumNASC$NASC.by.strata$Stratum==strata[j]]
			# Scaling factor:
			meanNASC$NASC.scale.f[meanNASC$Stratum==strata[j]] <- ifelse(sm>0,resampledNASC[i,j]/sm,0)
		}
	}
	# Update biostation weighting
	asg2 <- merge(assignments, BootWeights,by=c("Stratum", "StID"), all.x=TRUE)
	asg2$StationWeight <- ifelse(!is.na(asg2$Freq), asg2$StationWeight*asg2$Freq, 0)
	# Update trawl assignment table in Stox Java object:
	setAssignments(projectName, assignments=asg2)

	# Scale and update NASC values
	if(length(psuNASC)>0){
		meanNASC$Value <- meanNASC$Value * meanNASC$NASC.scale.f
		# Update MeanNASC object in Java memory:
		setNASC(projectName, "MeanNASC", meanNASC)
	}
	# Run the sub baseline within Java. The argument reset=TRUE is essensial to obtain the bootstrapping:
	getBaseline(projectName, startProcess=startProcess, endProcess=endProcess, proc=endProcess, input=FALSE, msg=FALSE, save=FALSE, reset=TRUE, drop=FALSE, warningLevel=1)$outputData
}


#*********************************************
#*********************************************
#' Bootstrap biotic stations and acoustic data
#'
#' Resample (bootstrap) trawl stations based on survey (Cruise) number and station numbers (SerialNo) to estimate uncertainty in estimates.
#'
#' @param projectName  	The name or full path of the project, a baseline object (as returned from getBaseline() or runBaseline()), og a project object (as returned from open).
#' @param assignments	Trawl assignment from baseline.
#' @param psuNASC		MeanNASC from baseline.
#' @param stratumNASC	Strata NASC estimates from getNASCDistr(baseline).
#' @param resampledNASC	Resampled NASC distribution.
#' @param nboot			Number of bootstrap replicates.
#' @param startProcess	The start process of the bootstrapping, being the first process before which biostations has been assigned and NASC values have been calculated.
#' @param endProcess	The end process of the bootstrapping, being the process returning a matrix containing the following columns: "Stratum", "Abundance", "weight", and grouping variables such as "age", "SpecCat", "sex".
#' @param seed			The seed for the random number generator (used for reproducibility).
#' @param cores			An integer giving the number of cores to run the bootstrapping over.
#' @param baseline		Optional: a StoX baseline object returned from runBaseline().
#' @param msg			Logical: if TRUE print messages from runBaseline().
#' @param parameters	Parameters set by user in Stox (only kept for compatibility with older versions);.
#' \describe{
#'	\item{parameters$nboot}{Number of bootstrap replicates}
#'	\item{parameters$seed}{The seed for the random number generator (used for reproducibility)}
#' }
#'
#' @return list with (1) the abundance by length in the orginal model, (2) the abundance by length in the bootstrap run, (3) the abundance by super individuals in the orginal model, (4) the abundance by super individuals in the bootstrap run
#'
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom pbapply pblapply
#'
#' @export
#' @keywords internal
#'
bootstrapParallel <- function(projectName, assignments, psuNASC=NULL, stratumNASC=NULL, resampledNASC=NULL, nboot=5, startProcess="TotalLengthDist", endProcess="SuperIndAbundance", seed=1, cores=1, baseline=NULL, msg=TRUE, parameters=list()){
	# Stop the funciton if both projectName and baseline are missing:
	if(length(baseline)==0 && missing(projectName)){
		stop("Either projectName or baseline must be given.")
	}
 
	# Allow for inputs given in 'nboot' and 'seed' to prepare for the higher level functions bootstrapAcoustic() and runBootstrap():
	if(length(parameters$nboot)){
		nboot <- parameters$nboot
	}
	if(length(parameters$seed)){
		seed = parameters$seed
	}

	# Filter assignments against NASC:
	if(length(psuNASC)){
		#assignments <- droplevels(subset(assignments, getVar(assignments, "Stratum") %in% getVar(psuNASC, "Stratum")))
		assignments <- droplevels(assignments[getVar(assignments, "Stratum") %in% getVar(psuNASC, "Stratum"), ])
	}
	# Unique trawl station ID:
	assignments$StID <- getVar(assignments, "Station")
	
	set.seed(if(isTRUE(seed)) 1234 else if(is.numeric(seed)) seed else NULL) # seed==TRUE giving 1234 for compatibility with older versions
	# Makes seed vector for fixed seeds (for reproducibility):
	seedV <- sample(c(1:10000000), nboot, replace = FALSE)
	# Define strata, either by acoustic values (if psuNASC is given) or by the trawl assignments:
	strata <- unique(if(length(psuNASC)>0) getVar(psuNASC, "Stratum") else getVar(assignments, "Stratum"))
	
	# Store the SuperIndAbundance from the original model:
	# base.SuperIndAbundance <- getBaseline(baseline, fun="SuperIndAbundance", input=FALSE, msg=msg, drop=FALSE)$outputData$SuperIndAbundance
	# base.SuperIndAbundance <- getBaseline(baseline, proc="SuperIndAbundance", input=FALSE, msg=msg, drop=FALSE)$outputData$SuperIndAbundance
	base.SuperIndAbundance <- getBaseline(baseline, proc=endProcess, input=FALSE, msg=msg, drop=FALSE)$outputData[[endProcess]]
	
	# Detect the number of cores and use the minimum of this and the number of requested cores and the number of bootstrap replicates:	
	availableCores = detectCores()
	if(cores>availableCores){
		warning(paste0("Only ", availableCores, " cores available (", cores, " requested)"))
	}
	cores = min(cores, nboot, availableCores)
	
	# Generate the clusters of time steps:
	if(cores>1){
		cat(paste0("Running ", nboot, " bootstrap replicates (using ", cores, " cores in parallel):\n"))
		cl<-makeCluster(cores)
		# Bootstrap:
		out <- pblapply(seq_len(nboot), bootstrapOneIteration, projectName=projectName, assignments=assignments, strata=strata, psuNASC=psuNASC, stratumNASC=stratumNASC, resampledNASC=resampledNASC, startProcess=startProcess, endProcess=endProcess, seedV=seedV, cl=cl)
		# End the parallel bootstrapping:
		stopCluster(cl)
	}
	else{
		cat(paste0("Running ", nboot, " bootstrap replicates:\n"))
		out <- pblapply(seq_len(nboot), bootstrapOneIteration, projectName=projectName, assignments=assignments, strata=strata, psuNASC=psuNASC, stratumNASC=stratumNASC, resampledNASC=resampledNASC, startProcess=startProcess, endProcess=endProcess, seedV=seedV)
	}
	
	out <- unlist(out, recursive=FALSE)
	
	# Order the output from the bootstrapping:
	names(out) <- paste0(names(out), "_run", seq_along(out))
	
	bootstrapParameters <- list(
		seed = seed, 
		seedV = seedV, 
		nboot = nboot, 
		cores = cores
		)
	# Return the bootstrap data and parameters:
	list(base.SuperIndAbundance=base.SuperIndAbundance, SuperIndAbundance=out, bootstrapParameters=bootstrapParameters) 
}


#*********************************************
#*********************************************
#' Run a bootstrap in StoX
#'
#' Resample (bootstrap) trawl stations based on swept area data and possibly also acoustic data to estimate uncertainty in estimates.
#'
#' @param projectName   The name or full path of the project, a baseline object (as returned from getBaseline() or runBaseline()), og a project object (as returned from open).
#' @param acousticMethod,bioticMethod   Specification of the method to use for bootstrapping the acoustic and biotic data. Currently only one method is available for acoustic and one for biotic data: acousticMethod = PSU~Stratum, bioticMethod = PSU~Stratum. Other methods are planned in later versions, involving the levels of the data given in the below table.
#' \tabular{rrr}{
#'   Level \tab Acoustic \tab Biotic\cr
#'   Survey \tab Survey \tab Survey\cr
#'   Stratum \tab Stratum \tab Stratum\cr
#'   Assignment \tab Not relevant \tab Assignment of biotic station groups to acoustic PSUs\cr
#'   PSU \tab Acoustic data averaged over e.g. one tansect \tab Biotic station group \cr
#'   EDSU \tab Acoustic data averaged over e.g. one nmi \tab Biotic station\cr
#'   Sample \tab Ping \tab Individal catch sample
#' }
#' @param nboot			Number of bootstrap replicates.
#' @param startProcess	The start process of the bootstrapping, being the first process before which biostations has been assigned and NASC values have been calculated.
#' @param endProcess	The end process of the bootstrapping, being the process returning a matrix containing the following columns: "Stratum", "Abundance", "weight", and grouping variables such as "age", "SpecCat", "sex".
#' @param seed			The seed for the random number generator (used for reproducibility).
#' @param cores			An integer giving the number of cores to run the bootstrapping over.
#' @param msg			Logical: if TRUE print messages from runBaseline().
#' @param ...			Used for backwards compatibility.
#'
#' @return list with (1) the abundance by length in the orginal model, (2) the abundance by length in the bootstrap run, (3) the abundance by super individuals in the orginal model, (4) the abundance by super individuals in the bootstrap run
#'
#' @examples
#' \dontrun{
#' b <- runBootstrap("Test_Rstox", nboot=10, seed=1, cores=1)}
#'
#' @importFrom stats terms as.formula
#'
#' @export
#' @rdname runBootstrap
#'
runBootstrap <- function(projectName, acousticMethod=PSU~Stratum, bioticMethod=PSU~Stratum, nboot=5, startProcess="TotalLengthDist", endProcess="SuperIndAbundance", seed=1, cores=1, msg=TRUE, ...){
	
	# Documentation removed on 2017-08-25, since it has not been implemented:
	# acousticMethod,bioticMethod   Specification of the method to use for bootstrapping the acoustic and biotic data. These can be formulas or characters which can be converted to formulas, given as 'variable to bootstrap ~ level to bootstrap within'. Multiple bootstraps can be specified, such as bioticMethod=c(EDSU~Stratum, Sample~EDSU), instructing to bootstrap the EDSUs (stations) within each stratum, and also bootstrapping the individual catch samples within each EDSU. Only certain strings can be used in the formulas, as shown in the table below. The methods can also be given as functions of at least two arguments, 'projectName' and 'process', which makes modifications to the output from getBaseline(projectName, proc=process, input=NULL) and sends the modified data back to the baseline in Java memory and runs the baseline with the modified data. Using funcitons is not yet implemented. 
	# \tabular{rrr}{
	#   Level \tab Acoustic \tab Biotic\cr
	#   Survey \tab Survey \tab Survey\cr
	#   Stratum \tab Stratum \tab Stratum\cr
	#   Assignment \tab Not relevant \tab Assignment of biotic station groups to acoustic PSUs\cr
	#   PSU \tab Acoustic data averaged over e.g. one tansect \tab Biotic station group \cr
	#   EDSU \tab Acoustic data averaged over e.g. one nmi \tab Biotic station\cr
	#   Sample \tab Ping \tab Individal catch sample
	# }
	
	# Function used for extracting either a matrix of bootstrap variables and domains, or the function specified by the user:
	getBootstrapMethod <- function(x){
		isNULL <- any(length(x)==0, sum(nchar(x))==0, identical(x, FALSE), is.character(x) && identical(tolower(x), "null"))
		if(isNULL){
			return(NULL)
		}
		if(is.function(x)){
			warning("Method as a function not yet implemented")
			return(NULL)
		}
		if(!any(unlist(gregexpr("~", as.character(x), fixed=TRUE))>0)){
			warning("Invalid formula")
			return(NULL)
		}
		# The c(x) is added to assure that sapply works on each formula and not on the parts of one formula, if only one is given:
		if(is.character(x) || all(sapply(c(x), function(xx) class(xx)=="formula"))){
			return(sapply(c(x), function(xx) rownames(attributes(terms(as.formula(xx)))$fact)))
		}
		else{
			warning("Invalid input")
			return(NULL)
		}
	}
	
	lll <- list(...)
	### Backwards compatibility: ###
	# If the old numIterations is given, override the nboot by this:
	if(length(lll$numIterations)){
		nboot <- lll$numIterations
	}
	### End of backwards compatibility: ###
	
	# Run the different bootstrap types:
	matchOldAcoustic <- FALSE
	matchOldSweptArea <- FALSE
	acousticMethod <- getBootstrapMethod(acousticMethod)
	bioticMethod <- getBootstrapMethod(bioticMethod)
	
	if(	length(acousticMethod)==2 
		&& length(bioticMethod)==2 
		&& startsWith(tolower(acousticMethod[1,1]), "psu") 
		&& startsWith(tolower(acousticMethod[2,1]), "stratum") 
		&& startsWith(tolower(bioticMethod[2,1]), "stratum")  
		&& (startsWith(tolower(bioticMethod[1,1]), "edsu") || startsWith(tolower(bioticMethod[1,1]), "psu"))){
			matchOldAcoustic <- TRUE
	}
	else{
		matchOldAcoustic <- FALSE
	}
	
	if(	length(acousticMethod)==0 
		&& length(bioticMethod)==2 
		&& (startsWith(tolower(bioticMethod[1,1]), "edsu") || startsWith(tolower(bioticMethod[1,1]), "psu")) 
		&& startsWith(tolower(bioticMethod[2,1]), "stratum")){
			matchOldSweptArea <- TRUE
	}
	else{
		matchOldSweptArea <- FALSE
	}
	
	# Backwards compatibility for type="Acoustic", hidden in ... (used prior to Rstox 1.5):
	if(length(lll$type)){
		matchOldAcoustic <- matchOldAcoustic || lll$type=="Acoustic"
	}
	# Backwards compatibility for type="SweptArea", hidden in ... (used prior to Rstox 1.5):
	if(length(lll$type)){
		matchOldSweptArea <- matchOldSweptArea || lll$type=="SweptArea"
	}
	
	# Apply the original bootstrap methods in Rstox:
	if(matchOldAcoustic){
		# Baseline and biotic assignments:
		baseline <- runBaseline(projectName, out="baseline", msg=msg, reset=TRUE)
		assignments <- getBioticAssignments(baseline=baseline)
		# Acoustic data:
		# NOTE: The psuNASC is read here once, and used to scale the PSUs in the baseline at each bootstrap replicate. It is important to keept this, since the PSUs are changed in memory in each core, and we wish to scale relative to the original values each time. For the same reason, the PSUs are set back to the original value at the end of bootstrapParallel() when run on 1 core:
		psuNASC <- getPSUNASC(baseline=baseline)
		stratumNASC <- getNASCDistr(baseline=baseline, psuNASC=psuNASC, NASCDistr="observed")
		resampledNASC <- getResampledNASCDistr(baseline=baseline, psuNASC=psuNASC, stratumNASC=stratumNASC, parameters=list(nboot=nboot, seed=seed))
		# Assign varialbes to the project environment:
		setProjectData(projectName=projectName, var=psuNASC)
		setProjectData(projectName=projectName, var=stratumNASC)
		setProjectData(projectName=projectName, var=resampledNASC)
		
		# Run bootstrap:
		bootstrap <- bootstrapParallel(projectName=projectName, assignments=assignments, psuNASC=psuNASC, stratumNASC=stratumNASC, resampledNASC=resampledNASC, nboot=nboot, startProcess=startProcess, endProcess=endProcess, seed=seed, cores=cores, baseline=baseline, msg=msg)
		
		# Add the method specification:
		bootstrap$bootstrapParameters$acousticMethod <- acousticMethod
		bootstrap$bootstrapParameters$bioticMethod <- bioticMethod
		bootstrap$bootstrapParameters$description <- "Original Rstox default 'Acoustic' method up until Rstox 1.5, bootstrapping acousic PSUs within stratum, and scaleing the PSUs to have mean matching that of the bootstrap, and bootstrapping biotic stations within stratum, and assigning station weights equal to the frequency of occurrence of each station"
		bootstrap$bootstrapParameters$alias <- "Acoustic"
		
		# Assign the bootstrap to the project environment:
		setProjectData(projectName=projectName, var=bootstrap)
		# Rerun the baseline to ensure that all processes are run, and return the boostraped data:
		baseline <- runBaseline(projectName, reset=TRUE, msg=FALSE)
		invisible(TRUE)
	}
	else if(matchOldSweptArea){
		# Baseline and biotic assignments:
		baseline <- runBaseline(projectName, out="baseline", msg=msg, reset=TRUE)
		assignments <- getBioticAssignments(baseline=baseline)
		# Run bootstrap:
		bootstrap <- bootstrapParallel(projectName=projectName, assignments=assignments, nboot=nboot, startProcess=startProcess, endProcess=endProcess, seed=seed, cores=cores, baseline=baseline, msg=msg)
		
		# Add the method specification:
		bootstrap$bootstrapParameters$acousticMethod <- acousticMethod
		bootstrap$bootstrapParameters$bioticMethod <- bioticMethod
		description <- "Original Rstox default 'SweptArea' method up until Rstox 1.5, bootstrapping biotic stations within stratum, and assigning station weights equal to the frequency of occurrence of each station"
		alias <- "SweptArea"
		
		# Assign varialbes to the global environment for use in plotting functions. This should be changed to a local Rstox environment in the future:
		setProjectData(projectName=projectName, var=bootstrap)
		# Rerun the baseline to ensure that all processes are run, and return the boostraped data:
		baseline <- runBaseline(projectName, reset=TRUE, msg=FALSE)
		invisible(TRUE)
	}
	else{
		warning("Invalid bootstrap method...")
	}
}


#*********************************************
#*********************************************
#' Parametric variance estimation
#'
#' Calculates mean, variance, coefficient of variation and 90 % confidence bounds based on Jolly, G. M., & Hampton, I. (1990). A stratified random transect design for acoustic surveys of fish stocks. Canadian Journal of Fisheries and Aquatic Sciences, 47(7), 1282-1291.
#'
#' @param projectName   The name or full path of the project, a baseline object (as returned from getBaseline() or runBaseline()), og a project object (as returned from open).
#' @param proc			The process which returns the acoustic density on which the variance should be estimated.
#' @param ignore.case	Logical: If TRUE ignore case in species category SpecCat.
#'
#' @return A list with names "Stratum" and "Total" containing dataframes for each species with the following columns: Stratum, Mean, SD (standard deviation), Variance, CV (coefficient of variation), and lower and upper 90% confidence bounds.
#'
#' @examples
#' \dontrun{
#' b <- varianceEstimation("Test_Rstox")}
#'
#' @importFrom stats terms as.formula
#'
#' @export
#' @rdname varianceEstimation
#'
varianceEstimation <- function(projectName, proc="SweptAreaDensity", ignore.case=TRUE){
	
	# Read the required baseline and process data:
	StratumArea <- getBaseline(projectName, proc="StratumArea", input=NULL)
	DensityMatrix <- getBaseline(projectName, proc=proc, input=NULL)
	psustratum <- getBaseline(projectName, proc=NULL, input="proc")$psustratum
	if(length(DensityMatrix)==0){
		stop(paste0("The process specified by 'proc' (", proc, ") is not present in the model. This must be a process with output containing the columns SpecCat, SampleUnitType, SampleUnit and Density. Also the SampleUnitType must be \"PSU\"."))
	}
	else if(!all(c("SpecCat", "SampleUnitType", "SampleUnit", "Density") %in% names(DensityMatrix)) && head(DensityMatrix$SampleUnitType, 1)!="PSU"){
		stop(paste0("The output from the process specified by 'proc' (", proc, ") does not contain the columns SpecCat, SampleUnitType, SampleUnit and Density, or the SampleUnitType is not \"PSU\"."))
	}
	
	# Function used for inserting an object 'add' into a data frame 'x':
	addVar <- function(x, add, name=NULL){
		# Interpret the name if missing, and add prefix:
		if(length(name)==0){
			name <- deparse(substitute(add))
		}
		
		# If only one value is given, insert this in all rows:
		if(length(add)==1){
			atStratum <- 1
		}
		else{
			atStratum <- match(x$Stratum, names(add))
		}
		# Add the 'add' at the correct rows as specified by Stratum:s
		x[name] <- add[atStratum]
		x
	}
	
	# Function used for adding statistics:
	JollyHampton <- function(x, na.rm=TRUE){
		# Eq (1) in Jolly and Hampton (1990), but replacing the weights w by 1:
		MeanDensityStratum <- tapply(x$Density, x[c("Stratum")], mean, na.rm=na.rm)
		
		# Get also the corresponding stratum areas:
		AreaStratum <- tapply(x$StratumArea, x[c("Stratum")], head, 1)
		# Add the MeanDensityStratum:
		x <- addVar(x, MeanDensityStratum)
		
		# Eq (3) in Jolly and Hampton (1990), but replacing the weights w by 1:
		VarDensityStratumFun <- function(x, varName, meanName, na.rm=TRUE){
			n = nrow(x)
			if(n < 2){
				return(Inf)
			}
			out <- sum( (x[[varName]] - x[[meanName]])^2, na.rm=na.rm ) / (n * (n - 1))
			return(out)
		}
		# Add the VarDensityStratum, SDDensityStratum and CVDensityStratum:
		VarDensityStratum <- c(by(x, x[c("Stratum")], VarDensityStratumFun, varName="Density", meanName="MeanDensityStratum", na.rm=na.rm))
		SDDensityStratum <- sqrt(VarDensityStratum)
		CVDensityStratum <- SDDensityStratum / MeanDensityStratum
		x <- addVar(x, VarDensityStratum)
		x <- addVar(x, SDDensityStratum)
		x <- addVar(x, CVDensityStratum)
		
		# Overall statistics, Eq (2) and (3) in Jolly and Hampton (1990):
		MeanDensity <- sum(MeanDensityStratum * AreaStratum, na.rm=na.rm) / sum(AreaStratum, na.rm=na.rm)
		VarDensity <- sum(VarDensityStratum * AreaStratum^2, na.rm=na.rm) / sum(AreaStratum, na.rm=na.rm)^2
		SDDensity <- sqrt(VarDensity)
		CVDensity <- SDDensity / MeanDensity
		x <- addVar(x, MeanDensity)
		x <- addVar(x, VarDensity)
		x <- addVar(x, SDDensity)
		x <- addVar(x, CVDensity)
		
		return(x)
	}
	
	# Function used for adding zeros at empty data and calculating statistics of Density and Abundance:
	addAbundance <- function(x, psustratum, StratumArea, na.rm=TRUE){
		# Get the strata definitions to add to the data:
		add <- psustratum
		# Add Stratum area:
		add$StratumArea <- StratumArea$Area[match(add$Stratum, StratumArea$PolygonKey)]
		# Check for consistency in SampleUnit:
		if(!x$SampleUnitType[1] %in% names(add)){
			stop("Mismatch between psustratum and SampleUnitType in the data")
		}
		names(add)[names(add)==x$SampleUnitType[1]] <- "SampleUnit"
		# Add the number of sample units to 'add'
		SampleSize <- table(add$Stratum)
		SampleSize <- c(SampleSize[add$Stratum])
		add <- cbind(add, SampleSize)
		# Add also the numer of sample units with positive values:
		# First add Stratum to x:
		x <- cbind(x, Stratum=add$Stratum[match(x$SampleUnit, add$SampleUnit)])
		SampleSizePos <- table(unique(x[, c("SampleUnit", "Stratum")])$Stratum)
		SampleSizePos <- c(SampleSizePos[add$Stratum])
		SampleSizePos[is.na(SampleSizePos)] <- 0
		add <- cbind(add, SampleSizePos)
		
		# Expand the data to having zeros at the empty hauls, since the zeros must be taken into account in the inference:
		# Repeat the first line n times:
		n <- nrow(add)
		out <- x[rep(1, n),]
		# Set all densities to 0:
		out$Density <- 0
		# Insert the positive densities:
		validNames <- !names(out) %in% c("SampleUnit", "Stratum")
		positive <- match(x$SampleUnit, add$SampleUnit)
		out[positive, validNames] <- x[, validNames]
		
		# Combine the strata information and the repeated rows (discarding SampleUnit, which is present in 'add'):
		out <- cbind(add, out[validNames])
		
		# Add statistics to Density:
		out <- JollyHampton(out, na.rm=na.rm)
		
		return(out)
	}
	
	
	# Check for the number of SampleUnits:
	numSampleUnit <- table(psustratum$Stratum)
	if(any(numSampleUnit < 2)){
		warning(paste0("The following strata have less than 2 SampleUnits, which disallows variance estimation: ", names(numSampleUnit)[which(numSampleUnit < 2)]))
	}
	
	# Split into species:
	if(ignore.case){
		byPSU <- split(DensityMatrix, tolower(DensityMatrix$SpecCat))
	}
	else{
		byPSU <- split(DensityMatrix, DensityMatrix$SpecCat)
	}
	# Expand with the zero-observations and add Stratum area and abundance:
	byPSU <- lapply(byPSU, addAbundance, psustratum=psustratum, StratumArea=StratumArea)
	# Clean off non-positive rows:
	byPSUPos <- lapply(byPSU, function(x) x[x$SampleSizePos>0, , drop=FALSE])
	
	# Pick out stratum:
	select <- c("SpecCat", "Stratum", "StratumArea", "SampleSize", "SampleSizePos", "MeanDensityStratum", "VarDensityStratum", "SDDensityStratum", "CVDensityStratum", "MeanDensity", "VarDensity", "SDDensity", "CVDensity") # 
	byStratum <- lapply(byPSU, function(y) unique(y[select]))
	byStratum <- lapply(byStratum, function(y) cbind(y, AbundanceStratum = y$StratumArea * y$MeanDensityStratum))
	
	total <- lapply(byStratum, function(y) cbind(
		SpecCat = y$SpecCat[1], 
		t(colSums(y[c("StratumArea", "SampleSize", "SampleSizePos")])), 
		head(y[c("MeanDensity", "CVDensity")], 1), 
		Abundance=sum(y$AbundanceStratum)
		))
	
		par(mfrow=rep(ceiling(sqrt(length(total))), 2))
		lapply(byStratum, function(y) hist(y$MeanDensityStratum))
	
	return(list(total=total, byStratum=byStratum, byPSU=byPSU))
	#return(list(byStratum=byStratum, total=total))
}
