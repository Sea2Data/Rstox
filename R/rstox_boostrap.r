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
#' @param sorted	Should the data be sorted prior to sampling?
#'
#' @return list with (1) the abundance by length in the bootstrap run, (2) the abundance by super individuals in the bootstrap run
#'
#' @export
#' @keywords internal
#'
bootstrapOneIteration <- function(i, projectName, assignments, strata, psuNASC=NULL, stratumNASC=NULL, resampledNASC=NULL, startProcess="TotalLengthDist", endProcess="SuperIndAbundance", seedV=NULL, sorted=TRUE){
	
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
		#set.seed(seedV[i])
		# Resample BioStation:
		# Change introduced on 2017-11-03, applying the function sampleSorted() for all sampling throughout Rstox in order to avoid dependency on the order of rows in the data:
		#StID <- sample(stations, replace = TRUE)
		print("sorted")
		print(sorted)
		# Sampling with replacement will normally result in fewer rows in the final superIndAbundance tables:
		StID <- sampleSorted(stations, size=length(stations), seed=seedV[i], replace=TRUE, sorted=sorted)
		
		# Count weights from resample:
		count <- as.data.frame(table(StID))
		count$Stratum <- strata[j]
		BootWeights <- rbind(BootWeights,count)
		
		# Find NASC scaling factor. This is not directly related to the sampling of biotic stations above. The NASC values have actually been resampled outside of this function, in the resampledNASC <- getResampledNASCDistr() command in runBootstrap():
		if(length(psuNASC)>0){
			# Pick out the NASC value of the current stratum j of the current bootstrap replicate i:
			sm <- stratumNASC$NASC.by.strata$strata.mean[stratumNASC$NASC.by.strata$Stratum==strata[j]]
			# Change introduced in the output from getResampledNASCDistr(), which form 2017-11-03 returns a list of elements NASC and seed:
			if(is.list(resampledNASC)){
				resampledNASC <- resampledNASC$NASC
			}
			# Scaling factor. This is a factor to multiply all NASC vaules inside the current stratum/bootstrap replicate with:
			meanNASC$NASC.scale.f[meanNASC$Stratum==strata[j]] <- ifelse(sm>0, resampledNASC[i,j]/sm, 0)
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
#' @param sorted	Should the data be sorted prior to sampling?
#'
#' @return list with (1) the abundance by length in the orginal model, (2) the abundance by length in the bootstrap run, (3) the abundance by super individuals in the orginal model, (4) the abundance by super individuals in the bootstrap run
#'
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom pbapply pblapply
#'
#' @export
#' @keywords internal
#'
bootstrapParallel <- function(projectName, assignments, psuNASC=NULL, stratumNASC=NULL, resampledNASC=NULL, nboot=5, startProcess="TotalLengthDist", endProcess="SuperIndAbundance", seed=1, cores=1, baseline=NULL, msg=TRUE, parameters=list(), sorted=TRUE){
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
	# If memory runs out, a system call to determine number of cores might fail, thus detectCores() could return NA
	# defaulting to single core if this is the case
	if(is.na(availableCores)) availableCores <- 1
	if(cores>availableCores){
		warning(paste0("Only ", availableCores, " cores available (", cores, " requested)"))
	}
	cores = min(cores, nboot, availableCores)
	
	# Generate the clusters of time steps:
	if(cores>1){
		cat(paste0("Running ", nboot, " bootstrap replicates (using ", cores, " cores in parallel):\n"))
		cl<-makeCluster(cores)
		# Bootstrap:
		out <- pblapply(seq_len(nboot), bootstrapOneIteration, projectName=projectName, assignments=assignments, strata=strata, psuNASC=psuNASC, stratumNASC=stratumNASC, resampledNASC=resampledNASC, startProcess=startProcess, endProcess=endProcess, seedV=seedV, sorted=sorted, cl=cl)
		# End the parallel bootstrapping:
		stopCluster(cl)
	}
	else{
		cat(paste0("Running ", nboot, " bootstrap replicates:\n"))
		out <- pblapply(seq_len(nboot), bootstrapOneIteration, projectName=projectName, assignments=assignments, strata=strata, psuNASC=psuNASC, stratumNASC=stratumNASC, resampledNASC=resampledNASC, startProcess=startProcess, endProcess=endProcess, seedV=seedV, sorted=sorted)
	}
	
	out <- unlist(out, recursive=FALSE)
	
	# Order the output from the bootstrapping:
	#names(out) <- paste0(names(out), "_run", seq_along(out))
	# If the old version with unsorted sampling and the original names of the bootstrap outputs on each run is needed, this is indicated with the 'sorted' option set to FALSE:
	if(sorted){
		zeropadded <- sprintf(paste0("%0", nchar(length(out)), "d"), seq_along(out))
		names(out) <- paste0(names(out), "_run", zeropadded)
	}
	else{
		names(out) <- paste0(names(out), "_run", seq_along(out))
	}
	
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
#' \code{runBootstrap} is a wrapper function for the bootstrap functions below.
#' \code{runBootstrap_acousticTrawl} Run a simple bootstrap of biotic PSUs within strata.
#' \code{runBootstrap_sweptArea_length} Run a simple bootstrap of biotic PSUs within strata.
#' \code{runBootstrap_sweptArea_total} Run a simple bootstrap of biotic PSUs within strata.
#'
#' Resample (bootstrap) trawl stations based on swept area data and possibly also acoustic data to estimate uncertainty in estimates. By the default method (acousticMethod=PSU~Stratum, bioticMethod=PSU~Stratum), the acoustic transect values (mean NASC along transects) and biotic stations (trawls) are resampled with replacement within each stratum for each bootstrap replicate, and the StoX project rerun and super individual abundance recalculated (or the output from a different process given by \code{endProcess}).
#'
#' @param projectName  					The name or full path of the project, a baseline object (as returned from getBaseline() or runBaseline()), og a project object (as returned from open).
#' @param bootstrapMethod				The method to use for the bootstrap. Currently implemented are given in the following table:
#' \tabular{rrr}{
#'   bootstrapMethod \tab Description
#'   acousticTrawl \tab Bootstrap of acoustic tralw surveys, where both acoustic and biotic data are resampled\cr
#'   sweptArea_length \tab Bootstrap only biotic data with length information\cr
#'   sweptArea_total \tab For surveys with information only about total catch (count or weight), bootstrap biotic stations\cr
#' }
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
#' @param nboot							Number of bootstrap replicates.
#' @param startProcess					The start process of the bootstrapping, being the first process before which biostations has been assigned and NASC values have been calculated.
#' @param endProcess					The end process of the bootstrapping, being the process returning a matrix containing the following columns: "Stratum", "Abundance", "weight", and grouping variables such as "age", "SpecCat", "sex".
#' @param seed							The seed for the random number generator (used for reproducibility).
#' @param cores							An integer giving the number of cores to run the bootstrapping over.
#' @param msg							Logical: if TRUE print messages from runBaseline().
#' @param ignore.case					Logical: If TRUE, ingore case when splitting by species category SpecCat.
#' @param ...							Used for backwards compatibility.
#' @param sorted						Should the data be sorted prior to sampling?
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
runBootstrap <- function(projectName, bootstrapMethod="acousticTrawl", acousticMethod=PSU~Stratum, bioticMethod=PSU~Stratum, nboot=5, startProcess="TotalLengthDist", endProcess="SuperIndAbundance", seed=1, cores=1, msg=TRUE, sorted=TRUE, ...){
	
	# Function used for extracting either a matrix of bootstrap variables and domains, or the function specified by the user:
	getBootstrapLevels <- function(x){
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
	temp <- getBootstrapMethod(bootstrapMethod=bootstrapMethod, acousticMethod=acousticMethod, bioticMethod=bioticMethod, ...)
	bootstrapMethod <- temp$bootstrapMethod
	acousticMethod <- temp$acousticMethod
	bioticMethod <- temp$bioticMethod
	
	# Apply the bootstrap:
	if(!bootstrapMethod %in% getRstoxEnv()$model_types){
		stop("Invalid bootstrap type.")
	}
	bootstrapFun <- paste("runBootstrap", bootstrapMethod, sep="_")
	do.call(bootstrapFun, list(projectName=projectName, acousticMethod=acousticMethod, bioticMethod=bioticMethod, nboot=nboot, startProcess=startProcess, endProcess=endProcess, seed=seed, cores=cores, msg=msg, sorted=sorted, ...))
}
#'
#' @export
#' @rdname runBootstrap
#'
runBootstrap_1.6 <- function(projectName, bootstrapMethod="acousticTrawl", acousticMethod=PSU~Stratum, bioticMethod=PSU~Stratum, nboot=5, startProcess="TotalLengthDist", endProcess="SuperIndAbundance", seed=1, cores=1, msg=TRUE, ...){
	runBootstrap(projectName=projectName, bootstrapMethod=bootstrapMethod, acousticMethod=acousticMethod, bioticMethod=bioticMethod, nboot=nboot, startProcess=startProcess, endProcess=endProcess, seed=seed, cores=cores, msg=msg, sorted=FALSE, ...)
}
#'
#' @export
#' @rdname runBootstrap
#'
runBootstrap_temp <- function(projectName, bootstrapMethod="acousticTrawl", acousticMethod=PSU~Stratum, bioticMethod=PSU~Stratum, nboot=5, startProcess="TotalLengthDist", endProcess="SuperIndAbundance", seed=1, cores=1, msg=TRUE, sorted=TRUE, ...){
	
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
	getBootstrapLevels <- function(x){
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
	acousticMethod <- getBootstrapLevels(acousticMethod)
	bioticMethod <- getBootstrapLevels(bioticMethod)
	
	if(	length(acousticMethod)==2 
		&& length(bioticMethod)==2 
		&& startsWith(tolower(acousticMethod[1,1]), "psu") 
		&& startsWith(tolower(acousticMethod[2,1]), "stratum") 
		&& (startsWith(tolower(bioticMethod[1,1]), "edsu") || startsWith(tolower(bioticMethod[1,1]), "psu"))
		&& startsWith(tolower(bioticMethod[2,1]), "stratum") ){
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
		resampledNASC <- getResampledNASCDistr(baseline=baseline, psuNASC=psuNASC, stratumNASC=stratumNASC, parameters=list(nboot=nboot, seed=seed), sorted=sorted)
		# Assign varialbes to the project environment:
		setProjectData(projectName=projectName, var=psuNASC)
		setProjectData(projectName=projectName, var=stratumNASC)
		setProjectData(projectName=projectName, var=resampledNASC)
		
		# Run bootstrap:
		bootstrap <- bootstrapParallel(projectName=projectName, assignments=assignments, psuNASC=psuNASC, stratumNASC=stratumNASC, resampledNASC=resampledNASC, nboot=nboot, startProcess=startProcess, endProcess=endProcess, seed=seed, cores=cores, baseline=baseline, msg=msg, sorted=sorted)
		
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
		bootstrap <- bootstrapParallel(projectName=projectName, assignments=assignments, nboot=nboot, startProcess=startProcess, endProcess=endProcess, seed=seed, cores=cores, baseline=baseline, msg=msg, sorted=sorted)
		
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
#'
#' @export
#' @rdname runBootstrap
#'
runBootstrap_acousticTrawl <- function(projectName, acousticMethod=PSU~Stratum, bioticMethod=PSU~Stratum, nboot=5, startProcess="TotalLengthDist", endProcess="SuperIndAbundance", seed=1, cores=1, msg=TRUE, sorted=TRUE, ...){
	
	# Baseline and biotic assignments:
	baseline <- runBaseline(projectName, out="baseline", msg=msg, reset=TRUE)
	assignments <- getBioticAssignments(baseline=baseline)
	
	# Acoustic data:
	# NOTE: The psuNASC is read here once, and used to scale the PSUs in the baseline at each bootstrap replicate. It is important to keept this, since the PSUs are changed in memory in each core, and we wish to scale relative to the original values each time. For the same reason, the PSUs are set back to the original value at the end of bootstrapParallel() when run on 1 core:
	psuNASC <- getPSUNASC(baseline=baseline)
	stratumNASC <- getNASCDistr(baseline=baseline, psuNASC=psuNASC, NASCDistr="observed")
	resampledNASC <- getResampledNASCDistr(baseline=baseline, psuNASC=psuNASC, stratumNASC=stratumNASC, parameters=list(nboot=nboot, seed=seed), sorted=sorted)
	# Assign varialbes to the project environment:
	setProjectData(projectName=projectName, var=psuNASC)
	setProjectData(projectName=projectName, var=stratumNASC)
	setProjectData(projectName=projectName, var=resampledNASC)

	# Run bootstrap:
	bootstrap <- bootstrapParallel(projectName=projectName, assignments=assignments, psuNASC=psuNASC, stratumNASC=stratumNASC, resampledNASC=resampledNASC, nboot=nboot, startProcess=startProcess, endProcess=endProcess, seed=seed, cores=cores, baseline=baseline, msg=msg, sorted=sorted)

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
#'
#' @export
#' @rdname runBootstrap
#'
runBootstrap_sweptArea_length <- function(projectName, acousticMethod=PSU~Stratum, bioticMethod=PSU~Stratum, nboot=5, startProcess="TotalLengthDist", endProcess="SuperIndAbundance", seed=1, cores=1, msg=TRUE, sorted=TRUE, ...){
	
	# Baseline and biotic assignments:
	baseline <- runBaseline(projectName, out="baseline", msg=msg, reset=TRUE)
	assignments <- getBioticAssignments(baseline=baseline)
	
	# Run bootstrap:
	bootstrap <- bootstrapParallel(projectName=projectName, assignments=assignments, nboot=nboot, startProcess=startProcess, endProcess=endProcess, seed=seed, cores=cores, baseline=baseline, msg=msg, sorted=sorted)
	
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
#'
#' @export
#' @rdname runBootstrap
#'
runBootstrap_sweptArea_total <- function(projectName, endProcess="SweptAreaDensity", nboot=5, seed=1, cores=1, ignore.case=TRUE, sorted=TRUE, ...){
	boot1 <- function(seed=1, data, list.out=TRUE, sorted=TRUE){
		# Function for calculating the mean density and keep the first row of a matrix:
		MeanDensity1 <- function(y){
			out <- y[1, , drop=FALSE]
			out$Density <- mean(y$Density)
			out
		}
		# Sample the rows of each stratum of each species:
		b <- lapply(data, function(y) by(y, y$Stratum, sampleSorted, seed=seed, by="SampleUnit"))
		# Calculate the mean density of each stratum of each species, and combine to a data.frame:
		m <- lapply(b, function(y) lapply(y, MeanDensity1))
		m <- lapply(m, data.table::rbindlist)
		m <- lapply(m, as.data.frame)
		# Calculate the total abundance:
		tot <- lapply(m, function(y) sum(y$Area * y$Density))
		# Output:
		if(list.out){
			list(tot=tot, mean=m, boot=b)
		}
		else{
			tot
		}
	}
	
	# Define seeds, and save these later:
	if(length(seed)==1){
		set.seed(seed)
		seed <- round(runif(nboot, 1, 1e6))
	}
	else{
		seed <- rep(seed, length.out=nboot)
	}
	
	DensityMatrix <- getBaseline(projectName, proc=endProcess, input=NULL)
	# Add stratum:
	DensityMatrix <- linkPSU2Stratum(DensityMatrix, projectName, ignore.case=ignore.case, list.out=TRUE)
	
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
		bootstrap <- pblapply(seed, boot1, data=DensityMatrix, list.out=TRUE, sorted=sorted, cl=cl)
		# End the parallel bootstrapping:
		stopCluster(cl)
	}
	else{
		cat(paste0("Running ", nboot, " bootstrap replicates:\n"))
		bootstrap <- pblapply(seed, boot1, data=DensityMatrix, list.out=TRUE, sorted=sorted)
	}
	
	setProjectData(projectName=projectName, var=bootstrap)

	boot <- rbindlist(lapply(bootstrap, "[[", "tot"))
	Variance <- apply(boot, 2, var)
	SD <- sqrt(Variance)
	Mean <- apply(boot, 2, mean)
	CV <- SD / Mean

	cbind(Variance, SD, Mean, CV)
}
#'
#' @export
#' @rdname runBootstrap
#'
getBootstrapMethod <- function(bootstrapMethod="acousticTrawl", acousticMethod=PSU~Stratum, bioticMethod=PSU~Stratum, ...){
	# Function for comparing strings:
	strequal <- function(x, y, ignore.case=TRUE){
		if(ignore.case){
			identical(tolower(x[1]), tolower(y[1]))
		}
		else{
			identical(x[1], y[1])
		}
	}
	
	# Function used for extracting either a matrix of bootstrap variables and domains, or the function specified by the user:
	getBootstrapLevels <- function(x){
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

	# get the bootstrap levels:
	acousticMethod <- getBootstrapLevels(acousticMethod)
	bioticMethod <- getBootstrapLevels(bioticMethod)
	
	
	# Special care for when bootstrapMethod=="acousticTrawl" (the default) and acousticMethod = NULL and bioticMethod = PSU~Stratum or EDSU~Stratum:
	if(strequal(bootstrapMethod[1], "acousticTrawl")){
		if(	length(acousticMethod)==0 
			&& length(bioticMethod)==2 
			&& (strequal(bioticMethod[1,1], "edsu") || strequal(bioticMethod[1,1], "psu")) 
			&& strequal(bioticMethod[2,1], "stratum")){
				bootstrapMethod <- "sweptArea_length"
				warning("The value of 'bootstrapMethod' changed from \"acousticTrawl\" to \"sweptArea_length\"")
		}
	}
	
	# Save the dotlist:
	lll <- list(...)
	# Backwards compatibility for type="Acoustic", hidden in ... (used prior to Rstox 1.5):
	if(length(lll$type) && strequal(lll$type, "Acoustic")){
		bootstrapMethod <- "acousticTrawl"
	}
	# Backwards compatibility for type="SweptArea", hidden in ... (used prior to Rstox 1.5):
	if(length(lll$type) && strequal(lll$type, "SweptArea")){
		bootstrapMethod <- "sweptArea_length"
	}
	
	list(bootstrapMethod=bootstrapMethod, acousticMethod=acousticMethod, bioticMethod=bioticMethod)
}
### #'
### #' @importFrom data.table rbindlist
### #'
### #' @export
### #' @keywords internal
### #' @rdname runBootstrap
### #'
### boot1 <- function(seed=1, data, list.out=TRUE, sorted=TRUE){
### 	
### 	# Function for calculating the mean density and keep the first row of a matrix:
### 	MeanDensity1 <- function(y){
### 		out <- y[1, , drop=FALSE]
### 		out$Density <- mean(y$Density)
### 		out
### 	}
### 	## Function for sampling the rows of a matrix:
### 	#sample1 <- function(y){
### 	#	set.seed(seed)
### 	#	ind <- sample(nrow(y), replace=TRUE)
### 	#	y[ind, , drop=FALSE]
### 	#}
### 	
### 	# Sample the rows of each stratum of each species:
### 	#b <- lapply(data$byPSU, function(y) by(y, y$Stratum, sample1))
### 	#b <- lapply(data, function(y) by(y, y$Stratum, sample1))
### 	b <- lapply(data, function(y) by(y, y$Stratum, sampleSorted, by="SampleUnit"))
### 	# Calculate the mean density of each stratum of each species, and combine to a data.frame:
### 	m <- lapply(b, function(y) lapply(y, MeanDensity1))
### 	m <- lapply(m, data.table::rbindlist)
### 	m <- lapply(m, as.data.frame)
### 	# Calculate the total abundance:
### 	tot <- lapply(m, function(y) sum(y$Area * y$Density))
### 	# Output:
### 	if(list.out){
### 		list(tot=tot, mean=m, boot=b)
### 	}
### 	else{
### 		tot
### 	}
### }


#*********************************************
#*********************************************
#' Parametric variance estimation
#'
#' Calculates mean, variance, coefficient of variation and 90 % confidence bounds based on Jolly, G. M., & Hampton, I. (1990). A stratified random transect design for acoustic surveys of fish stocks. Canadian Journal of Fisheries and Aquatic Sciences, 47(7), 1282-1291.
#'
#' @param projectName   The name or full path of the project, a baseline object (as returned from getBaseline() or runBaseline()), og a project object (as returned from open).
#' @param proc			The process which returns the acoustic density on which the variance should be estimated.
#' @param ignore.case	Logical: If TRUE ignore case in species category SpecCat.
#' @param na.rm			Logical: If FALSE return NA for all variance and mean statistics in the presence of NA in at lesat one stratum.
#' @param out			A string specifying the format of the output (first element used). (1) If out = "bySpecCat", return a list with one element per species category SpecCat, with sublists for the total survey and for each stratum. (2) If out = "byLevel", return a list with elements 'Total' and 'Stratum', where all species categories are merged in one data frame. (3) If out = "listByLevel", return a list with elements 'Total' and 'Stratum', with sublists for all species categories.
#'
#' @return A list with names "Stratum" and "Total" containing dataframes for each species with the following columns: Stratum, Mean, SD (standard deviation), Variance, CV (coefficient of variation), and lower and upper 90% confidence bounds.
#'
#' @examples
#' \dontrun{
#' # Download an example swept area project:
#' # pr <- createProject("ftp://ftp.imr.no/StoX/Example%20projects/Example_sweptarea_BarentsSea_cod_1999.zip")
#' # v <- varianceEstimation(pr)
#'
#' @importFrom utils head
#' @importFrom data.table rbindlist
#'
#' @export
#' @rdname varianceEstimation
#'
varianceEstimation <- function(projectName, proc="SweptAreaDensity", ignore.case=TRUE, na.rm=TRUE, out=c("bySpecCat", "byLevel", "listByLevel")){
	
	# Function used for adding statistics:
	JollyHampton <- function(x, na.rm=TRUE){
		# Eq (3) in Jolly and Hampton (1990), but replacing the weights w by 1:
		VarDensityStratumFun <- function(x, varName, meanName, na.rm=TRUE){
			n = nrow(x)
			if(n < 2){
				return(Inf)
			}
			out <- sum( (x[[varName]] - x[[meanName]])^2, na.rm=na.rm ) / (n * (n - 1))
			return(out)
		}
		
		# Get indices at first occurence of the strata, ordered by Stratum ID:
		atStratumFirst <- which(!duplicated(x$Stratum))
		atStratumFirst <- atStratumFirst[order(x$Stratum[atStratumFirst])]
		SampleSizeStratum <- x$SampleSizeStratum[atStratumFirst]
		PosSampleSizeStratum <- x$PosSampleSizeStratum[atStratumFirst]
		
		# Add stratum areas:
		AreaStratum <- tapply(x$Area, x[c("Stratum")], head, 1)
		
		# Add the MeanDensityStratum:
		MeanDensityStratum <- tapply(x$Density, x[c("Stratum")], mean, na.rm=na.rm)
		# Add the VarDensityStratum, SDDensityStratum and CVDensityStratum:
		VarDensityStratum <- c(by(x, x[c("Stratum")], VarDensityStratumFun, varName="Density", meanName="MeanDensityStratum", na.rm=na.rm))
		SDDensityStratum <- sqrt(VarDensityStratum)
		CVDensityStratum <- SDDensityStratum / MeanDensityStratum
		AbundanceStratum <- AreaStratum * MeanDensityStratum
		
		# Overall statistics, Eq (2) and (3) in Jolly and Hampton (1990):
		MeanDensity <- sum(MeanDensityStratum * AreaStratum, na.rm=na.rm) / sum(AreaStratum, na.rm=na.rm)
		VarDensity <- sum(VarDensityStratum * AreaStratum^2, na.rm=na.rm) / sum(AreaStratum, na.rm=na.rm)^2
		SDDensity <- sqrt(VarDensity)
		CVDensity <- SDDensity / MeanDensity
		
		# Create output for stratum:
		xStratum <- data.frame(
			SpecCat = x$SpecCat[1], # 1
			SampleUnitType = "Stratum", # 2
			SampleUnit = x$Stratum[atStratumFirst], # 3
			Area = AreaStratum, # 4
			SampleSize = SampleSizeStratum, # 5
			PosSampleSize = PosSampleSizeStratum, # 6
			MeanDensity = MeanDensityStratum, # 7
			VarDensity = VarDensityStratum, # 8
			SDDensity = SDDensityStratum, # 9
			CVDensity = CVDensityStratum, # 10
			Abundance = AbundanceStratum # 11
		)
		
		# Create output for the whole project:
		xTotal <- data.frame(
			SpecCat = x$SpecCat[1], # 1
			Area = sum(AreaStratum), # 2
			SampleSize = sum(SampleSizeStratum), # 3
			PosSampleSize = sum(PosSampleSizeStratum), # 4
			MeanDensity = MeanDensity, # 5
			VarDensity = VarDensity, # 6
			SDDensity = SDDensity, # 7
			CVDensity = CVDensity, # 8
			Abundance = sum(AbundanceStratum) # 9
		)
		
		return(list(Total=xTotal, Stratum=xStratum))
	}
	
	# Read the required baseline and process data:
	StratumArea <- getBaseline(projectName, proc="StratumArea", input=NULL)
	psustratum <- getBaseline(projectName, proc=NULL, input="psustratum")
	# Check for the number of SampleUnits:
	numSampleUnit <- checkNumPSUsInStratum(psustratum=psustratum)
	# Get the density matrix:
	output <- getBaseline(projectName, proc=proc, input=NULL)
	
	# Test for the required data:
	if(length(output)==0){
		stop(paste0("The process specified by 'proc' (", proc, ") is not present in the model. This must be a process with output containing the columns SpecCat, SampleUnitType, SampleUnit and Density. Also the SampleUnitType must be \"PSU\"."))
	}
	else if(!all(c("SpecCat", "SampleUnitType", "SampleUnit", "Density") %in% names(output)) && head(output$SampleUnitType, 1)!="PSU"){
		stop(paste0("The output from the process specified by 'proc' (", proc, ") does not contain the columns SpecCat, SampleUnitType, SampleUnit and Density, or the SampleUnitType is not \"PSU\"."))
	}
	
	# Add stratum information:
	output <- linkPSU2Stratum(output, projectName=projectName, psustratum=psustratum, StratumArea=StratumArea, ignore.case=ignore.case)
	
	# Apply the variance estimation:
	output <- lapply(output, JollyHampton, na.rm=na.rm)
	
	if(tolower(out[1]) %in% c("bylevel", "listbylevel")){
		output <- list(Total=lapply(output, "[[", "Total"), Stratum=lapply(output, "[[", "Stratum"))
	}
	if(tolower(out[1]) == "bylevel"){
		output <- lapply(output, data.table::rbindlist)
		output <- lapply(output, as.data.frame)
	}
	return(output)
}
#'
#' @importFrom utils head
#'
#' @export
#' @rdname varianceEstimation
#'
varianceEstimation_old <- function(projectName, proc="SweptAreaDensity", ignore.case=TRUE){
	
	# Read the required baseline and process data:
	StratumArea <- getBaseline(projectName, proc="StratumArea", input=NULL)
	psustratum <- getBaseline(projectName, proc=NULL, input="psustratum")
	DensityMatrix <- getBaseline(projectName, proc=proc, input=NULL)
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
		warning(paste0("The following strata have less than 2 SampleUnits, which disallows variance estimation: ", paste(names(numSampleUnit)[which(numSampleUnit < 2)], collapse=", ")))
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
	select <- c("SpecCat", "Stratum", "StratumArea", "SampleSize", "SampleSizePos", "MeanDensityStratum", "VarDensityStratum", "SDDensityStratum", "CVDensityStratum", "MeanDensity", "VarDensity", "SDDensity", "CVDensity")
	byStratum <- lapply(byPSU, function(y) unique(y[select]))
	byStratum <- lapply(byStratum, function(y) cbind(y, AbundanceStratum = y$StratumArea * y$MeanDensityStratum))
	
	total <- lapply(byStratum, function(y) cbind(
		SpecCat = y$SpecCat[1], 
		t(colSums(y[c("StratumArea", "SampleSize", "SampleSizePos")])), 
		#head(y[c("MeanDensity", "CVDensity")], 1), 
		head(y[c("MeanDensity", "VarDensity", "SDDensity", "CVDensity")], 1), 
		Abundance=sum(y$AbundanceStratum)
		))
	
	#par(mfrow=rep(ceiling(sqrt(length(total))), 2))
	#lapply(byStratum, function(y) hist(y$MeanDensityStratum))

	# Remove the total variables from byStratum and byPSU:
	deselectPSU <- c("MeanDensityStratum", "VarDensityStratum", "SDDensityStratum", "CVDensityStratum", "MeanDensity", "VarDensity", "SDDensity", "CVDensity")
	byPSU <- lapply(byPSU, function(x) x[!names(x) %in% deselectPSU])
	deselect_stratum <- c("MeanDensity", "VarDensity", "SDDensity", "CVDensity")
	byStratum <- lapply(byStratum, function(x) x[!names(x) %in% deselect_stratum])
	
	# Order by the first two columns:
	orderFirstTwo <- function(x){
		x[order(x[,1], x[,2]), ]
	}
	byStratum <- lapply(byStratum, orderFirstTwo)
	byPSU <- lapply(byPSU, orderFirstTwo)
	
	return(list(total=total, byStratum=byStratum, byPSU=byPSU))
	#return(list(byStratum=byStratum, total=total))
}




#*********************************************
#*********************************************
#' Bootstrap utils.
#'
#' \code{linkPSU2Stratum} Adds stratum information to a data frame with one row per PSU. Stratum ID, Area, number of PSUs and positive PSUs are added, and rows are generated for strata with non-positive density.
#' \code{checkNumPSUsInStratum} Checks for numner of PSUs >= 2 in each stratum, which is a requirement for variance estimation, and should possibly be a requirement for the bootstrapping when one of the methods is PSU~Stratum.
#'
#' @param x				A data frame with with one row per PSU, to which Stratum information should be added.
#' @param projectName   The name or full path of the project, a baseline object (as returned from getBaseline() or runBaseline()), og a project object (as returned from open).
#' @param psustratum	A data frame linking PSUs to stratum IDs. Should have the columns 'Stratum' and 'PSU' or 'SampleUnit'. Read from the project if missing.
#' @param StratumArea	A data frame providing the area of each stratum. Should have the columns 'Area' and 'PolygonKey' or 'Stratum'. Read from the project if missing.
#' @param list.out		Logical: If TRUE, split the data frame into a list per species category SpecCat.
#' @param ignore.case	Logical: If TRUE, ingore case when splitting by species category SpecCat.
#'
#' @return A list with names "Stratum" and "Total" containing dataframes for each species with the following columns: Stratum, Mean, SD (standard deviation), Variance, CV (coefficient of variation), and lower and upper 90% confidence bounds.
#'
#' @importFrom utils head
#'
#' @export
#' @keywords internal
#' @rdname linkPSU2Stratum
#'
linkPSU2Stratum <- function(x, projectName, psustratum=NULL, StratumArea=NULL, list.out=TRUE, ignore.case=TRUE){
	# Funciton for expanding the matrix by zero density for missing PSUs in the matrix:
	fillZeros <- function(y, psustratum){
		n <- nrow(psustratum)
		# This is crude. We repeat the first line, and don't care that many of the colunms become invalid, as long as, SampleUnit, Stratum, Density, SpecCat and SampleUnitType are correct:
		out <- y[rep(1, n),]
		# Set all densities to 0:
		out$Density <- 0
		# Insert the positive densities:
		validNames <- !names(out) %in% c("SampleUnit", "Stratum")
		positive <- match(y$SampleUnit, psustratum$SampleUnit)
		out[positive, validNames] <- y[, validNames]
		# Add SampleUnit and Stratum:
		out$SampleUnit <- psustratum$SampleUnit
		out$Stratum <- psustratum$Stratum
		# Remove rownames:
		rownames(out) <- NULL
		return(out)
	}
	# Function for adding sample size of the strata:
	addSampleSize <- function(y){
		# Get samole size:
		y$PosSampleSize <- as.numeric(y$Density>0)
		
		# Aggregate over Stratum:
		SampleSize <- by(y$SampleSize, y$Stratum, sum)
		PosSampleSize <- by(y$PosSampleSize, y$Stratum, sum)
		y$SampleSizeStratum <- SampleSize[y$Stratum]
		y$PosSampleSizeStratum <- PosSampleSize[y$Stratum]
		return(y)
	}
	# Function for adding stratum area:
	addstratumArea <- function(x, StratumArea){
		if(!"PolygonKey" %in% names(StratumArea) && "Stratum" %in% names(StratumArea)){
			StratumArea$PolygonKey <- StratumArea$Stratum
		}
		x$Area <- StratumArea$Area[match(x$Stratum, StratumArea$PolygonKey)]
		return(x)
	}
	
	# Get the strata definitions to add to the data:
	if(length(psustratum)==0){
		psustratum <- getBaseline(projectName, proc=NULL, input="psustratum")
	}
	if(length(StratumArea)==0){
		StratumArea <- getBaseline(projectName, proc="StratumArea", input=NULL)
	}
	
	# Check for consistency in SampleUnit:
	if(!x$SampleUnitType[1] %in% names(psustratum)){
		stop("Mismatch between psustratum and SampleUnitType in the data")
	}
	names(psustratum)[names(psustratum)==x$SampleUnitType[1]] <- "SampleUnit"
	
	# First add Stratum to x:
	#x <- cbind(x, Stratum=psustratum$Stratum[match(x$SampleUnit, psustratum$SampleUnit)])
	x$Stratum <- psustratum$Stratum[match(x$SampleUnit, psustratum$SampleUnit)]
	
	# Split into species categories:
	x <- split(x, if(ignore.case) tolower(x$SpecCat) else x$SpecCat)
	x <- lapply(x, fillZeros, psustratum=psustratum)
	x <- lapply(x, addSampleSize)
	x <- lapply(x, addstratumArea, StratumArea=StratumArea)
	
	# Return either a list or combined into a matrix:
	if(!list.out){
		x <- as.data.frame(data.table::rbindlist(x))
	}
	return(x)
}
#'
#' @export
#' @keywords internal
#' @rdname linkPSU2Stratum
#'
checkNumPSUsInStratum <- function(psustratum=NULL, projectName){
	# Get the strata definitions to add to the data:
	if(length(psustratum)==0){
		psustratum <- getBaseline(projectName, proc=NULL, input="psustratum")
	}
	# Check for the number of SampleUnits:
	numSampleUnit <- table(psustratum$Stratum)
	if(any(numSampleUnit < 2)){
		warning(paste0("The following strata have less than 2 SampleUnits, which disallows variance estimation: ", paste(names(numSampleUnit)[which(numSampleUnit < 2)], collapse=", ")))
	}
	numSampleUnit
}
