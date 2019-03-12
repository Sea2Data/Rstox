#*********************************************
#*********************************************
#' Merge two data tables with all=TRUE and the specified columns and keys.
#' 
#' Merges two data tables (see \code{\link[data.table]{data.table}}) with all=TRUE, while keeping only columns of the data tables with names intersecting \code{var}, and using the intersect of \code{keys} and the names of the data tables as the 'by' argument. By data tables 
#' 
#' 
#' @param x,y		Two data tables to be merged.
#' @param var		A character vector of names of the columns to keep while merging.
#' @param keys		A character vector of names of the columns to merge by (see the \code{by} argument in \code{\link[data.table]{merge}}).
#' @param keys.out	Logical: If TRUE return a list with the keys used and the merged data.
#'
#' @return A merged data table.
#'
merge2 <- function(x, y, var=c("distance", "weight", "lengthsampleweight", "length", "lengthunit"), keys=c("cruise", "serialno", "samplenumber", "SpecCat"), keys.out=FALSE){
	# Get the keys common for the two data tables:
	commonVar <- intersect(names(x), names(y))
	thisKeys <- intersect(keys, commonVar)
	# Get the variables requested for x and y:
	xvar <- intersect(names(x), c(var, keys))
	yvar <- intersect(names(y), c(var, keys))
	# Remove variables named identically ('weight' in biotic v1.4):
	yvar <- setdiff(yvar, xvar)
	# Add the keys:
	xvar <- unique(c(thisKeys, xvar))
	yvar <- unique(c(thisKeys, yvar))
	
	# Merge the data.tables:
	out <- merge(x[,xvar, with=FALSE], y[,yvar, with=FALSE], all=TRUE, by=thisKeys)
	
	if(keys.out){
		list(data=out, keys=thisKeys)
	}
	else{
		out
	}
}


#*********************************************
#*********************************************
#' Calculate length distributions of each combination of station and SpecCat
#' 
#' Merges two data tables with all=TRUE, while keeping only columns of the data tables with names intersecting \code{var}, and using the intersect of \code{keys} and the names of the data tables as the 'by' argument.
#' 
#' @param baselineOutput		A list as returned from \code{\link{getBaseline}} containing biotic data with three data tables representing the levels FishStation, CatchSample and Individual.
#' @param BioticData			The function from which the biotic data are retrieved, normally one of "ReadBioticXML" and "FilterBiotic".
#' @param LengthDistType		The type of length distribution to use, one of "LengthDist", "NormLengthDist" and "PercentLengthDist" (see 'Details').
#' @param allowMissingWeight	Logical: If TRUE and \code{LengthDistType} == "PercentLengthDist" accept stations with missing pairs of lengthsampleweight and (total) weight or lengthsamplecatch and (total) catch.
#' @param use.set				For development. Remove this later!!!!!!!!!!!!!!!!!!!!!!!!!!!
#' @param BioticDataList		A list of biotic data representing the levels FishStation, CatchSample and Individual as returned from e.g. getBaseline("Test_Rstox")$outputData$ReadBioticXML.
#'
#' @details The purpose of function StationLengthDist is to produce a length frequency distribution for each biotic station by species. Three different distributions (LengthDistType) can be generated:
#'
#' LengthDist
#' 
#' A calculated length distribution as if every individual in the catch had been length measured.
#'
#' @return A data table of length distributions stacked for each combination of cruise/station and SepcCat.
#'
#' @export
#' @rdname StationLengthDist
#'
StationLengthDist <- function(baselineOutput, BioticData="FilterBiotic", LengthDistType="PercentLengthDist", allowMissingWeight=TRUE, use.set=FALSE){
	# Accept the full output from getBaseline():
	if("outputData" %in% names(baselineOutput)){
		baselineOutput <- baselineOutput$outputData
	}
	
	
	# Define list names to get the data from the specified 'BioticData':
	BioticDataList <- baselineOutput[[BioticData]]
	# Remove the process name in the names of the biotic data tables:
	names(BioticDataList) <- gsub("^.*?_", "", names(BioticDataList))
	BioticDataList <- lapply(BioticDataList, data.table::as.data.table)
	
	getStationLengthDist(BioticDataList, LengthDistType=LengthDistType, allowMissingWeight=allowMissingWeight, use.set=use.set)
}
#'
#' @import data.table
#' @export
#' @rdname StationLengthDist
#'
getStationLengthDist <- function(BioticDataList, LengthDistType="PercentLengthDist", allowMissingWeight=TRUE, use.set=FALSE){
	
	# Fast table function for integer valued data:
	tabulatePlusOne <- function(x, range){
		# Allow for double precision, which may occur when multiplying by a raising factor such as weight/lengthsampleweight:
		out <- as.double(tabulate(x, range[2]))
		out[seq(range[1], range[2])]
	}
	
	# Take the sum of a number of length distributions, and return the input object subset to only the sum. This function is used in \code{getStationLengthDist} when summing over part samples, and is somewhat ad hoc:
	psum <- function(y){
		y$WeightedCount <- rowSums(matrix(y$WeightedCount, nrow=numLengthIntervals), na.rm=TRUE)
		y[seq_len(numLengthIntervals), ]
	}
	
	# Function to get missing data and number of part samples
	checkCatchSample <- function(x){
		has_SpecCat <- !is.na(x$SpecCat)
			has_weight <- !is.na(x$weight)
			has_lengthsampleweight <- !is.na(x$lengthsampleweight)
		has_weight <- has_weight & has_lengthsampleweight
			has_count <- !is.na(x$count)
			has_lengthsamplecount <- !is.na(x$lengthsamplecount)
		has_count <- has_count & has_lengthsamplecount

		has_weightORcount <- has_weight | has_count
		has_NOTweightBUTcount <- !has_weight & has_count

		numPartSamples <- x$samplenumber
		has_onlyOnePartSample <- numPartSamples == 1
	
		out <- list(
			has_SpecCat = has_SpecCat, 
			has_weight = has_weight, 
			has_weightORcount = has_weightORcount, 
			has_NOTweightBUTcount = has_NOTweightBUTcount, 
			has_onlyOnePartSample = has_onlyOnePartSample
		)
	
		# Add the tests to the input data table and return:
		out <- as.data.table(out)
		cbind(x, out)
	}
	
	# Accept the full output from getBaseline():
	if("outputData" %in% names(BioticDataList)){
		BioticDataList <- BioticDataList$outputData
	}
	
	##### Define dataset names: #####
	var <- c("distance", "weight", "lengthsampleweight", "count", "lengthsamplecount", "length", "lengthunit")
	keys <- c("cruise", "serialno", "SpecCat")
	aggregate_keys <- "samplenumber"
	keys <- c(keys, aggregate_keys)
	
	# Define list names to get the data from:
	FishStationName <- "BioticData_FishStation.txt"
	CatchSampleName <- "BioticData_CatchSample.txt"
	IndividualName <- "BioticData_Individual.txt"
	##########
	
	
	##### First merge the fish station and catch sample data tables, to be sure to remove the entire station if none of the catch samples have e.g. weight and lengthsampleweight: #####
	FishStation_CatchSample <- merge2(BioticDataList[[FishStationName]], BioticDataList[[CatchSampleName]], var=var, keys=keys)
	##########
	
	
	##### Subset datasets given 'LengthDistType': #####
	# If LengthDistType="NormLengthDist", accept only stations with 'distance':
	if(LengthDistType == "NormLengthDist"){
		FishStation_CatchSample <- subset(FishStation_CatchSample, !is.na(distance))
	}
	
	# Check validity of the data:
	FishStation_CatchSample <- checkCatchSample(FishStation_CatchSample)
	
	# If LengthDistType="PercentLengthDist" there is a possibility to accept stations with missing weight and count, as long as there is only one sample
	if(LengthDistType == "PercentLengthDist" && allowMissingWeight){
		FishStation_CatchSample <- subset(FishStation_CatchSample, has_SpecCat & (has_weightORcount | has_onlyOnePartSample))
	}
	else{
		FishStation_CatchSample <- subset(FishStation_CatchSample, has_SpecCat & has_weightORcount)
	}
	
	# Merge FishStation with CatchSample, and then the result with Individual:
	thisvar <- c(var, "has_weight", "has_NOTweightBUTcount", "has_onlyOnePartSample")
	temp <- merge2(FishStation_CatchSample, BioticDataList[[IndividualName]], var=thisvar, keys=keys, keys.out=TRUE)
	FishStation_CatchSample_Individual <- temp$data
	keys <- temp$keys
	##########
	
	
	##### Get length intervals: #####
	# The 'lengthunit' from biotic v1.4 is coded as given by getNMDinfo("lengthunit"), which says code = 1:7, resolutionMeters = c(0.001, 0.005, 0.010, 0.030, 0.050, 0.0005, 0.0001):
	resolutionMeters = c(0.001, 0.005, 0.010, 0.030, 0.050, 0.0005, 0.0001)
	lengthRes <- max(resolutionMeters[FishStation_CatchSample_Individual$lengthunit], na.rm=TRUE)
	lengthResCM <- lengthRes * 100
	
	# Get largest length resolution:
	# Round down all length measurements to the nearest length interval (add 1 to make the first interval start at 0, thus rounding down):
	FishStation_CatchSample_Individual$lengthInt <- floor(FishStation_CatchSample_Individual$length / lengthResCM) + 1
	rangeLengthInt <- range(FishStation_CatchSample_Individual$lengthInt, na.rm=TRUE)
	# Get the range of the lengths:
	rangeLengths <- (range(FishStation_CatchSample_Individual$lengthInt, na.rm=TRUE) - 1) * lengthResCM
	# Create a vector of all length intervals:
	lengthIntervals <- seq(rangeLengths[1], rangeLengths[2], by=lengthResCM)
	numLengthIntervals <- length(lengthIntervals)
	##########
	
	
	##### generate length distributions per station and SpecCat: #####
	setkeyv(FishStation_CatchSample_Individual, cols=keys)
	byGrp <- keys
	out <- FishStation_CatchSample_Individual[,  .(
		"WeightedCount" = tabulatePlusOne(lengthInt, rangeLengthInt), 
		"LengthGroup (cm)" = lengthIntervals, 
		"LengthInterval (cm)" = rep(lengthResCM[1], numLengthIntervals), 
		"LengthDistType" = rep(LengthDistType[1], numLengthIntervals),
		"distance" = rep(distance[1], numLengthIntervals),
		"weight" = rep(weight[1], numLengthIntervals),
		"lengthsampleweight" = rep(lengthsampleweight[1], numLengthIntervals), 
		"has_weight" = rep(has_weight[1], numLengthIntervals),
		"has_NOTweightBUTcount" = rep(has_NOTweightBUTcount[1], numLengthIntervals), 
		"has_onlyOnePartSample" = rep(has_onlyOnePartSample[1], numLengthIntervals)
		), by=byGrp]
	##########
	
	
	##### Sum over part samples: #####
	# Apply so called raising factors, which are total weight/count divided by sample weight/count:
	out[has_weight==TRUE, WeightedCount := WeightedCount * weight / lengthsampleweight]
	if(any(out$has_NOTweightBUTcount, na.rm=TRUE)){
		out[has_NOTweightBUTcount==TRUE, WeightedCount := WeightedCount * count / lengthsamplecount]
	}
	
	thiskeys <- setdiff(keys, aggregate_keys)
	setkeyv(out, cols=thiskeys)
	
	# Sum over part samples only if there are stations with more than one part sample:
	if(!all(out$has_onlyOnePartSample, na.rm=TRUE)){
		out <- out[has_onlyOnePartSample == TRUE, psum(.SD), by=thiskeys]
	}
	##########
	
	
	##### Output: #####
	# Define the Station column, which is a concatination of cruise and seialno:
	if(use.set){
		set(out, j="Station", value=paste(out$cruise, out$serialno, sep="/"))
	}
	else{
		out[, Station := paste(cruise, serialno, sep="/")]
	}
	
	# Convert to percent:
	if(LengthDistType == "PercentLengthDist"){
		out[, WeightedCount := WeightedCount/sum(WeightedCount, na.rm=TRUE) * 100, by=thiskeys]
	}
	# Normalize by trawled distance:
	else if(LengthDistType == "NormLengthDist"){
		out[, WeightedCount := WeightedCount/distance, by=thiskeys]
	}
	
	return(out)
	##########
}
