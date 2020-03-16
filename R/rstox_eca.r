#' Compensates bug in stox where the covariate temporal is not set based on prosessdata when useProsessdata is chosen
#' @noRd
temporal_workaround <- function(data, processdata, sourcetype, stations=NULL){
  
  if (!is.null(processdata$temporal)){
    message(paste("Applying workaround to set temporal for ", sourcetype, ". Does not support non-seasonal definitions", sep=""))
    tempdef <- processdata$temporal[processdata$temporal$CovariateSourceType==sourcetype,]
    tempdef$mstart <- substr(tempdef$Value, 4,5)
    tempdef$mend <- substr(tempdef$Value, 10,11)
    tempdef$dstart <- substr(tempdef$Value, 1,2)
    tempdef$dend <- substr(tempdef$Value, 7,8)

    tl <- data
    
    if (sourcetype=="Biotic"){
      
      stationdate <- tl$stationstartdate
      if (any(is.na(stationdate))){
      
        #attempt to use stopdate if startdate is NA  
        #only invoke workaround when needed
        if (!is.null(stations)){
          #need to merge in from original data, as stopdate is not exported by BioticCovData
          npre <- nrow(tl)
          tl <- merge(tl[,c("serialnumber", "cruise")], unique(stations[,c("serialnumber", "stationstartdate", "stationstopdate")]), all.x=T, by.x="serialnumber", by.y=c("serialnumber"))
          if (npre != nrow(tl)){
            stop("Issues with merging in stationdata. Multiple years combined ?")
          }
          
          #reinit as merge might reorder
          stationdate <- tl$stationstartdate
          stationdate[is.na(stationdate)] <- tl$stationstopdate[is.na(stationdate)]    
          
          if (any(is.na(stationdate))){
            stop("NAs in station startdate and stopdate")
          }
          tl$m <- substr(stationdate, 6,7)
          tl$d <- substr(stationdate, 9,10)
        }
        
      }
      else{
        if (any(is.na(stationdate))){
          stop("NAs in station startdate and stopdate")
        }
        tl$m <- substr(stationdate, 4,5)
        tl$d <- substr(stationdate, 1,2)
      }
      tl <- tl[order(tl$serialnumber),]
      data <- data[order(data$serialnumber),]
    }
    else if (sourcetype=="Landing"){
      tl$m <- substr(tl$sistefangstdato, 6,7)
      tl$d <- substr(tl$sistefangstdato, 9,10)
    }
    else{
      stop("Source type not supported for workarund")
    }

    for (i in 1:nrow(tempdef)){
      lte_end <- (tl$m < tempdef[i,"mend"] | (tl$m == tempdef[i,"mend"] & tl$d <= tempdef[i,"dend"]))
      gte_start <- (tl$m > tempdef[i,"mstart"] | (tl$m == tempdef[i,"mstart"] & tl$d >= tempdef[i,"dstart"]))
      selector <- lte_end & gte_start
      tl[selector, "temporal"] <- tempdef[i, "Covariate"]
    }

    data$temporal <- tl$temporal
    return(data)
  }
  else{
    return(data)
  }
}

#' Sets startdate equal to stopdate for biotic, wher the latter and not the former is defined.
#' @noRd
workaraound_set_startdate_from_stopdate <- function(biotic, stations){
  tl <- biotic

  if (any(is.na(tl$stationstartdate))){
    
    #attempt to use stopdate if startdate is NA  
    #only invoke workaround when needed
    if (!is.null(stations)){
      #need to merge in from original data, as stopdate is not exported by BioticCovData
      npre <- nrow(tl)
      tl <- merge(tl[, names(tl)[!(names(tl) %in% c("stationstartdate", "stationstopdate"))]],
                  stations[,c("serialnumber", "stationstartdate", "stationstopdate")], all.x=T, by.x="serialnumber", by.y=c("serialnumber"))
      if (npre != nrow(tl)){
        stop("Issues with merging in stationdata. Multiple years combined ?")
      }
      
      #reinit as merge might reorder
      tl$stationstartdate[is.na(tl$stationstartdate)] <- tl$stationstopdate[is.na(tl$stationstartdate)]    
      
      if (any(is.na(tl$stationstartdate))){
        stop("NAs in station startdate and stopdate")
      }
        
      #reformat startdate
      tl$stationstartdate <- strftime(tl$stationstartdate, format="%d/%m/%Y")
      
    }
  }
  return(tl)
}

#' get random number to pass as seed to RECA
#' @noRd
getseed <- function(){
  return(runif(1, 0,.Machine$integer.max))
}

#' Extract the covaraite definitions
#' Returns NULL if parameter is missing
#' @noRd
getCovparam <- function(projectName, parameter) {
  # Get the project object:
  project <- openProject(projectName, out = "project")
  s <- project$getProcessData()$asTable("covparam")
  if (nchar(s) > 0) {
    out <-
      read.csv(
        textConnection(s),
        sep = '\t',
        row.names = NULL,
        stringsAsFactors = FALSE,
        na.strings = "-",
        encoding = "UTF-8"
      )
    if (!(parameter %in% out$Parameter)) {
      return(NULL)
    }
    out <-
      out[out$Parameter == parameter, c("CovariateTable", "Value")]
    l <- as.list(out$Value)
    names(l) <- out$CovariateTable
    return(l)
  }
  else{
    stop(paste0("Table \"", "covparam", "\" missing in the project.xml file"))
  }
}

#' Function for converting the covariates present in biotic or landing to integer:
#' @noRd
getCovariateMatrix <- function(data, covariateNames, allLevels) {
  # Get the covariate names present in the data:
  covariateNames_present <- intersect(covariateNames, names(data))
  # Convert each covariate to integer (index in allLevels):
  out <-
    sapply(covariateNames_present, function(this)
      match(data[[this]], allLevels[[this]]))
  # Convert to data frame and add colnames:
  out <- as.data.frame(out, stringsAsFactors = FALSE)
  colnames(out) <- covariateNames_present
  out
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
#' @param landingresolution The temporal resolution in days to use for aggregating the landing data.
#' @param ...			Parameters passed to \code{\link{getBaseline}}.
#'
#' @return A reference to the StoX Java baseline object
#'
#' @export
#' @rdname baseline2eca
#'
baseline2eca <-
  function(projectName,
           biotic = "BioticCovData",
           landing = "LandingCovData",
           temporal = NULL,
           gearfactor = NULL,
           spatial = NULL,
           landingresolution = 92,
           ...) {
    # Function that retreives year, month, day, yearday:
    addYearday <-
      function(x,
               datecar = "stationstartdate",
               tz = "UTC",
               format = "%d/%m/%Y") {
        x[[datecar]] <- as.POSIXlt(x[[datecar]], tz = tz, format = format)
        if (length(x$year) == 0) {
          x$year <- x[[datecar]]$year + 1900
        }
        x$month <- x[[datecar]]$mon + 1
        x$monthday <- x[[datecar]]$mday
        x$yearday <- x[[datecar]]$yday + 1
        x
      }
    # Function used for extracting covariate definitions:
    getCovDef <- function(x) {
      # If the covariate is given as a comma separated string, split into a vector:
      if (length(grep(",", x[[3]]))) {
        x[[3]] = strsplit(x[[3]], ",")
        x[[3]] = lapply(x[[3]], as.numeric)
      }
      #biotic <- as.data.frame(sapply(x, "[", x$CovariateSourceType=="Biotic", drop=FALSE))
      biotic <- x[x$CovariateSourceType == "Biotic", , drop = FALSE]
      #landing <- as.data.frame(lapply(x, "[", x$CovariateSourceType=="Landing", drop=FALSE))
      landing <- x[x$CovariateSourceType == "Landing", , drop = FALSE]
      list(biotic = biotic, landing = landing)
    }
    
    # Define covariate processes and returned process data:
    # covariateProcessesData <- c("temporal", "season", "gearfactor", "spatial") # Changed on 2018-08-28 according to Jira STOX-153:
    # covariateProcessesData <- c("temporal", "gearfactor", "spatial") # This is not used anywhere....
    
    # Get the baseline output:
    ### baselineOutput <- getBaseline(projectName, input=c("par", "proc"), fun=c(biotic, landing))
    baselineOutput <-
      getBaseline(
        projectName,
        input = c("par", "proc"),
        proc = c(biotic, landing, "FilterBiotic"),
        ...
      )
    
    # Run if both biotic and landing data are present:
    if (all(c(biotic[1], landing[1]) %in% names(baselineOutput$out))) {
      #####################################
      ##### (1) Get raw landing data: #####
      #####################################
      
      # (1a) Get the data and convert variable names to lower case:
      landing <- baselineOutput$outputData[[landing[1]]]
      if (nrow(landing)==0){
        stop("Landing table is empty.")
      }
      names(landing) <- tolower(names(landing))

      landing <- temporal_workaround(landing, baselineOutput$processData, "Landing")
      # 2018-08-28: Changed to using 'sistefangstdato' as per comment from Edvin:
      landing <-
        addYearday(landing,
                   datecar = "sistefangstdato",
                   tz = "UTC",
                   format = "%Y-%m-%d")
      
      #####################################
      
      ############################################################
      ##### (2) Get raw biotic data with some modifications: #####
      ############################################################
      # (2a) Get the data and convert variable names to lower case:
      biotic <- baselineOutput$outputData[[biotic[1]]]
      
      if (nrow(biotic)==0){
        stop("Biotic table is empty")
      }
      
      names(biotic) <- tolower(names(biotic))
      biotic <- temporal_workaround(biotic, baselineOutput$processData, "Biotic", baselineOutput$outputData$FilterBiotic$FilterBiotic_BioticData_fishstation.txt)
      
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
      configuredCovariates <-
        names(getCovparam(projectName, "CovariateType"))
      present <- which(ECACovariates$Name %in% configuredCovariates)
      covariateNames <- ECACovariates$Name[present]
      covariateDescriptions <- ECACovariates$Description[present]
      covariateProcesses <- ECACovariates$Processe[present]
      
      message("Applying workaround to set startdate for yearday")
      biotic <- workaraound_set_startdate_from_stopdate(biotic, baselineOutput$outputData$FilterBiotic$FilterBiotic_BioticData_fishstation.txt)
      # (2c) Add yearday, year and month:
      biotic <-
        addYearday(biotic,
                   datecar = "stationstartdate",
                   tz = "UTC",
                   format = "%d/%m/%Y")
      
      # (2d) Hard code the lengthunits (from the sampling handbook). This must be changed in the future, so that lengthunitmeters is present in the biotic file:
      # TO BE IMPLEMENTED: Read the lengthresolution from NMD, which here is named by lengthunitmeters and then renamed to lengthresM in getGlobalParameters():
      # TO BE IMPLEMENTED: lengthresolution <- getNMDinfo("lengthresolution")
      
      lengthcode <- 1:7
      # Length unit codes in the reference tables:
      lengthmeters <- c(1, 5, 10, 30, 50, 0.5, 0.1) / 1000
      biotic$lengthunitmeters <-
        lengthmeters[match(biotic$lengthresolution, lengthcode)]
      
      
      # This section was removed with approval from Hanne and David. It was included for historical reasons, and the freqency column was supported in prevoius ECA versions, but we decided that there is no need for compressing the input data in this way anymore, given the assiciated complication of the input data:
      # (2e) Aggregate by lines without weight, but with equal length:
      #duplines = duplicated(biotic[,c("serialnumber", "length", "weight")]) & is.na(biotic$age)
      #if(any(duplines)){
      #	frequency = by(biotic$frequency, paste(biotic$serialnumber, biotic$length), sum)
      #	biotic = biotic[!duplines,]
      #	biotic$frequency = frequency
      #}
      ############################################################
      
      #######################################################
      ### (3) Get covariate definitions and change names: ###
      #######################################################
      covariateDefinition <-
        lapply(baselineOutput$proc[covariateNames], getCovDef)
      # Add year covariate definitions if present:
      if ("year" %in% covariateNames) {
        year <- unique(c(landing$year, biotic$year))
        #yearBiotic = data.frame(CovariateSourceType="Biotic", Covariate=year, Value=year, stringsAsFactors=FALSE)
        #yearLanding = data.frame(CovariateSourceType="Biotic", Covariate=year, Value=year, stringsAsFactors=FALSE)
        yearBiotic = data.frame(
          CovariateSourceType = "Biotic",
          Covariate = year,
          Definition = year,
          stringsAsFactors = FALSE
        )
        yearLanding = data.frame(
          CovariateSourceType = "Biotic",
          Covariate = year,
          Definition = year,
          stringsAsFactors = FALSE
        )
        covariateDefinition$year <-
          list(biotic = yearBiotic, landing = yearLanding)
      }
      
      # Extract the covariates from biotic and landing in two separate matrices, and convert to integers using match():
      allLevels <-
        lapply(covariateNames, function(x)
          sort(unique(c(
            biotic[[x]], landing[[x]]
          ))))
      names(allLevels) <- covariateNames
      Nlevels <- sapply(allLevels, length)
      
      covariateMatrixBiotic <-
        getCovariateMatrix(biotic, covariateNames = covariateNames, allLevels =
                             allLevels)
      covariateMatrixLanding <-
        getCovariateMatrix(landing, covariateNames = covariateNames, allLevels =
                             allLevels)
      
      #covariateMatrixBiotic <- sapply(seq_along(covariateNames), function(i) match(biotic[[covariateNames[i]]], allLevels[[i]]))
      #covariateMatrixLanding <- sapply(seq_along(covariateNames), function(i) match(landing[[covariateNames[i]]], allLevels[[i]]))
      #covariateMatrixBiotic <- as.data.frame(covariateMatrixBiotic, stringsAsFactors=FALSE)
      #covariateMatrixLanding <- as.data.frame(covariateMatrixLanding, stringsAsFactors=FALSE)
      #colnames(covariateMatrixBiotic) <- covariateNames
      #colnames(covariateMatrixLanding) <- covariateNames
      
      
      # Match the levels of each covariate with the unique values of the union of biotic and landing:
      matchToBioticAndLanding <-
        function(i, allLevels, covariateDefinition) {
          allValues <-
            sort(unique(
              union(
                covariateDefinition[[i]]$biotic[, 2],
                covariateDefinition[[i]]$landing[, 2]
              )
            ))
          
          link <- match(allLevels[[i]], allValues)
          data.frame(
            Numeric = seq_along(link),
            Covariate = allValues[link],
            stringsAsFactors = FALSE
          )
        }
      covariateLink <-
        lapply(
          seq_along(allLevels),
          matchToBioticAndLanding,
          allLevels = allLevels,
          covariateDefinition = covariateDefinition
        )
      names(covariateLink) <- names(covariateDefinition)
      
      rows <- sapply(covariateLink, FUN=function(x){nrow(x)})
      if (any(rows==0)){
        stop(paste("Some covariates have zero levels:", paste(names(rows)[rows==0], collapse=",")))
      }

      #covariateLink <- lapply(seq_along(allLevels), function(i) match(allLevels[[i]], covariateDefinition[[i]]$biotic[,2]))
      #covariateLink <- lapply(seq_along(allLevels), function(i) data.frame(Numeric=seq_along(allLevels[[i]]), Covariate=covariateDefinition[[i]]$biotic[covariateLink[[i]], 2], stringsAsFactors=FALSE))
      #names(covariateLink) <- names(covariateDefinition)
      
      ###########################################
      ##### (7) Covariate meta information: #####
      ###########################################
      
      # Changes on 2018-10-03 / 2018-10-11:
      # As of Rstox 1.10 the covariate info (covType and CAR) are stored as a process data output generated by the function bioticCovData (maybe to be changed to landingCovData?), as the data frame 'covparam':
      
      
      # Add a data frame with meta information about the covariates:
      covType <-
        unlist(lapply(covariateNames, function(xx)
          getCovparam(projectName, "CovariateType")[[xx]]))
      CAR <- rep(NA, length(covType))
      # This process assigns TRUE to CAR only if the parameter 'UseStratumNeighbour' exists and is equal to the string "true". All other values except empty values (NULL) implies FALSE. If the parameter 'UseStratumNeighbour' is not present, NA is used:
      temp <-
        lapply(covariateNames, function(xx)
          getCovparam(projectName, "UseStratumNeighbour")[[xx]]=="true")
      CAR[unlist(lapply(temp, length)) > 0] <- unlist(temp)
      # Make sure that CAR is a logical:
      CAR[is.na(CAR)] <- FALSE
      
      # Combine into the covariate info data frame:
      covariateInfo <- data.frame(
        Nlevels = Nlevels,
        covType = covType,
        CAR = CAR,
        name = covariateNames,
        description = covariateDescriptions,
        stringsAsFactors = FALSE
      )
      
      ###########################################
      
      ################################################
      ##### (8) Fish age vs length-error matrix: #####
      ################################################
      ageErrorData <- baselineOutput$proc$ageerror
      
      # Expand the AgeLength data to a sparse matrix:
      if (!is.null(ageErrorData)){
        maxAge <- max(ageErrorData[, 1:2]) + 1
        ageErrorMatrix <- matrix(0, ncol = maxAge, nrow = maxAge)
        ageErrorMatrix[as.matrix(ageErrorData[, 1:2]) + 1] <-
          ageErrorData[, 3]
        rownames(ageErrorMatrix) <- seq_len(maxAge) - 1
        colnames(ageErrorMatrix) <- rownames(ageErrorMatrix)
      }
      else{
        ageErrorMatrix <- NULL
      }
      ################################################
      
      ############################################
      ##### (9) Adjacent strata definitions: #####
      ############################################
      # Get the stratum neighbours and convert to a list named with the strata names, and convert each element into a vector of stratum names:
      stratumNeighbour <- baselineOutput$proc$stratumneighbour
      stratumNeighbourList <- as.list(stratumNeighbour[, 2])
      names(stratumNeighbourList) <- stratumNeighbour[, 1]
      if (is.numeric(stratumNeighbour[, 2])){
        stratumNeighbourList <-
          lapply(stratumNeighbourList, function(xx)
            as.numeric(unlist(strsplit(xx, ","))))
      }
      else{
        stratumNeighbourList <-
          lapply(stratumNeighbourList, function(xx)
            unlist(strsplit(xx, ",")))
      }

      # Extract only the strata present in the data:
      stratumNeighbourList <-
        stratumNeighbourList[names(stratumNeighbourList) %in% covariateLink$spatial[, 2]]
      
      # Remove also the neighbours that are not present:
      stratumNeighbourList <-
        lapply(stratumNeighbourList, function(x)
          x[x %in% names(stratumNeighbourList)])
      numNeighbours <- sapply(stratumNeighbourList, length)
      if (any(numNeighbours == 0)) {
        warning(
          paste0(
            "Some strata have no neighbours present in the data (stratum ",
            paste(names(stratumNeighbourList)[numNeighbours == 0], collapse = ", "),
            ") Please add neighbors to these strata. The present neighbours are:\n",
            paste(covariateLink$spatial[, 2], collapse = ", ")
          )
        )
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
        covariateMatrixBiotic = covariateMatrixBiotic,
        covariateMatrixLanding = covariateMatrixLanding,
        ageError = ageErrorMatrix,
        # Moved from resources on 2018-01-25
        stratumNeighbour = stratumNeighbourList,
        # Moved from resources on 2018-01-25
        resources = list(
          covariateInfo = covariateInfo,
          covariateDefinition = covariateDefinition,
          covariateLink = covariateLink
        )
      )
    }
    else{
      warning(paste0(
        "The processes ",
        paste(c(biotic[1], landing[1]), collapse = " and "),
        "does not exist in the baseline model"
      ))
      invisible(NULL)
    }
  }


#
# Functions for coverting data from stox-structure to ECA structure
#

#' Function used for combining hard coded parameter values and user defeined parameter values
#' @noRd
getHardCoded <- function(info) {
  hardcoded <- as.data.frame(matrix(
    c(#add constant
      "random", "constant", 0,
      # Include slope for the constant:
      "in.slopeModel", "constant", 1),
    byrow = TRUE,
    ncol = 3
  ), stringsAsFactors = FALSE)
  hardcoded[, 3] <- as.numeric(hardcoded[, 3])
  hardcoded <- hardcoded[hardcoded[, 2] %in% rownames(info), ]
  
  for (i in seq_len(nrow(hardcoded))) {
    info[hardcoded[i, 2], hardcoded[i, 1]] <- hardcoded[i, 3]
  }
  
  return(info)
}

#' Function for getting the day of the year that is midway between two dates.
#' @param x string representing a yearless date range as dd/mm-dd/mm or as a single day dd/mm
#' @return the day number in the year for the given day, or the mean day number in the year for the endpoints of the range
#' @noRd
getMidSeason <- function(x, tz = "UTC", format = "%d/%m/%Y") {
  x <- as.Date(strsplit(x, "-")[[1]], "%d/%m")
  x <- as.POSIXlt(x, tz = tz, format = format)
  yearday <- x$yday
  # Trick to get one day for "01/01-02/01"
  yearday[1] <- yearday[1] + 1
  mean(yearday)
}

#' Function used for extracting the correct covariate value from the inidces used in the covariate matrix passed to ECA:
#' @noRd
getCovariateValue <-
  function(index,
           eca,
           cov = "season",
           type = "biotic") {
    # Get first the covariate symbolic value (such as Q3 for "01/07-30/09"):
    posInLinkMatrix <-
      match(index, eca$resources$covariateLink[[cov]]$Numeric)
    symbol <-
      eca$resources$covariateLink[[cov]]$Covariate[posInLinkMatrix]
    # Then link to the value:
    posInDefinitionMatrix <-
      match(symbol, eca$resources$covariateDefinition[[cov]][[type]]$Covariate)
    value <-
      eca$resources$covariateDefinition[[cov]][[type]]$Value[posInDefinitionMatrix]
    value
  }

#' Function for getting the mode of a vector:
#' @noRd
getMode <- function(x) {
  as.numeric(names(table(x))[which.max(table(x))])
}

#' Function for extracting the GlobalParameters object
#' @noRd
getGlobalParameters <- function (biotic, resultdir, maxlength, minage, maxage, delta.age) {
  #serialnumber is there only to enforce return type for getVar
  getnames <- c("lengthunitmeters", "serialnumber")
  usenames <- c("lengthresM", "samplingID")
  DataMatrix <- getVar(biotic, getnames)
  names(DataMatrix) <- usenames
  
  lengthresM <- getMode(DataMatrix$lengthresM)
  lengthresCM <- lengthresM * 100
  if (!all(DataMatrix$lengthresM == head(DataMatrix$lengthresM, 1))) {
    message(
      paste0(
        "Several length resolusions applied in the data (",
        paste(table(DataMatrix$lengthresCM), collapse = ", "),
        "). The mode (",
        lengthresCM,
        ") used in the ECA"
      )
    )
  }
  
  # Convert to centimeters in the lengthunit:
  Gparams <- list(
    lengthresCM = lengthresCM,
    resultdir = resultdir,
    maxlength = maxlength,
    minage = minage,
    maxage = maxage,
    delta.age = delta.age
  )
  
  return(Gparams)
}

#' Function for extracting the Landings object
#' Compiles landings objects aggreageted to the covariates in the models, and to the temporal resolution provided
#' @noRd
getLandings <- function(landing, AgeLength, WeightLength, landingresolution) {
  
  if (any(is.na(landing$rundvekt))) {
    stop("Missing values in landing$rundvekt.")
  }
  
  #
  # test that covariateLink is the same for AgeLength and WeightLength
  #
  if (!all(names(AgeLength$resources$covariateLink) %in% names(WeightLength$resources$covariateLink))){
    stop("Different covariate defintions for Age given length model and Weight given length model is not supported")
  }
  
  #rename covariates
  decomposition <- c()
  for (n in names(AgeLength$resources$covariateLink)){
    if (AgeLength$info[n,"in.landings"]==1){
      #
      # test that covariateLink is the same for AgeLength and WeightLength
      #
      if (!(all(AgeLength$resources$covariateLink[[n]]==WeightLength$resources$covariateLink[[n]]))){
        stop("Different covariate levels for Age given length model and Weight given length model is not supported")
      }
      if (!(n %in% names(landing))){
        stop(paste("Eror in covariate configuration.",n," not found in landings."))
      }
      landing[[n]] <- AgeLength$resources$covariateLink[[n]]$Numeric[match(landing[[n]], AgeLength$resources$covariateLink[[n]]$Covariate)]
      if (any(is.na(landing[n]))){
        stop(paste("Can not aggregate landings for covariates with missing values:", n))
      }
      decomposition <- c(decomposition, n)
    }
  }
  
  # Aggregate the rundvekt by covariates and temporal resolution:
  landing$tempslot <- landing$yearday %/% landingresolution
  landingAggregated <-
    by(
      landing$rundvekt,
      as.data.frame(cbind(landing[,decomposition, drop=F], landing$tempslot), stringsAsFactors = FALSE),
      sum
    )
  # Combine into a data frame with covariates and the rundvekt in the last column:
  # Convert the dimnames to integer
  landingAggregated <-
    cbind(expand.grid(lapply(
      dimnames(landingAggregated), as.integer
    )), rundvekt = c(landingAggregated))
  
  # set the day of the year in the middle of each cell
  numDaysOfYear <- 366 #use max, since this is converted to fractino of the year later, consider looking up in calendar
  landingAggregated$midday <- landingAggregated$`landing$tempslot`*landingresolution + landingresolution/2.0
  landingAggregated$`landing$tempslot`<-NULL
  landingAggregated$midseason <-
    landingAggregated$midday / numDaysOfYear
  landingAggregated$midday <- NULL
  
  # Discard empty covariate combinations:
  landingAggregated <-
    landingAggregated[!is.na(landingAggregated$rundvekt), ]
  # Order by the covariates
  landingAggregated <-
    landingAggregated[do.call("order", landingAggregated[, -ncol(landingAggregated)]), ]
  
  
  ### landingAggregated: ###
  landingAggregated <- 
    cbind(
      constant = 1,
      landingAggregated
    )
  
  weight <- landingAggregated$rundvekt
  landingAggregated$rundvekt <- NULL
  landingAgeLength <- landingAggregated
  landingWeightLength <- landingAggregated
  
  ### Return a list of the data: ###
  landings <-
    list(
      AgeLengthCov = landingAgeLength,
      WeightLengthCov = landingWeightLength,
      LiveWeightKG = weight
    )
  return(landings)
}


#' Function for extracting the DataMatrix for the given variable ("age" or "weight", length is requested in both):
#' @keywords internal
getDataMatrixANDCovariateMatrix <-
  function(eca,
           vars = c("age", "yearday")) {
    #partcount
    
    # Define variables to include in the DataMatrix, where the variable specified in the input 'var' is included:
    getnames <-
      c(
        "lengthcentimeter",
        "serialnumber",
        "catchpartnumber",
        "lengthsamplecount",
        "lengthsampleweight",
        "catchweight",
        "otolithtype"
      )
    usenames <-
      c(
        "lengthCM",
        "samplingID",
        "partnumber",
        "samplecount",
        "sampleweight",
        "catchweight",
        "otolithtype"
      )
    getnames <- c(vars, getnames)
    usenames <- c(vars, usenames)
    
    # Extract the data matrix:
    
    DataMatrix <- getVar(eca$biotic, getnames)
    names(DataMatrix) <- usenames
    
    #Estimate catch sample number: partcount
    DataMatrix$partcount <-
      DataMatrix$catchweight * DataMatrix$samplecount / DataMatrix$sampleweight
    #Drop columns only used for estimating partcount
    DataMatrix <-
      DataMatrix[,!(names(DataMatrix) %in% c("catchweight", "samplecount", "sampleweight"))]
    
    # Add first the samplingID to the object eca$covariateMatrixBiotic:
    CovariateMatrix <- eca$covariateMatrixBiotic
    # Add samplingID, which will be removed at the end:
    CovariateMatrix$samplingID <- DataMatrix$samplingID
    CovariateMatrix <-
      CovariateMatrix[!duplicated(CovariateMatrix$samplingID), ]
    
    # Add the first column, which is only of ones:
    CovariateMatrix <- cbind(constant = 1, CovariateMatrix)
    
    # Convert to 1, 2, 3, and implement an automatic function for this later:
    DataMatrix$samplingID <-
      match(DataMatrix$samplingID, CovariateMatrix$samplingID)
    CovariateMatrix$samplingID <- NULL
    
    
    #random covariates not in landings should have nlev equal to the observed levels
    for (n in names(CovariateMatrix)) {
      if ((n %in% names(eca$covariateMatrixBiotic)) & !(n %in% names(eca$covariateMatrixLanding))) {
        
        ecacodes <- unique(CovariateMatrix[[n]])
        newecacodes <- 1:length(unique(CovariateMatrix[[n]]))
        
        # renumber covariates
        CovariateMatrix[[n]] <- newecacodes[match(CovariateMatrix[[n]], ecacodes)]
        
        # update link to stox names
        eca$resources$covariateLink[[n]] <- eca$resources$covariateLink[[n]][eca$resources$covariateLink[[n]][["Numeric"]] %in% ecacodes,]
        eca$resources$covariateLink[[n]][["Numeric"]] <- newecacodes[match(eca$resources$covariateLink[[n]][["Numeric"]], ecacodes)]
      } 
    }
    
    return(list(DataMatrix = DataMatrix, CovariateMatrix = CovariateMatrix, resources= eca$resources))
  }

#' Function for extracting the CARNeighbours and info:
#' @keywords internal
getInfo <- function(eca, CovariateMatrix, modelSpecification=NULL) {
  
  if (!is.null(modelSpecification)){
    stop("Configuraiton of continous, interactin an in.slopeModel not supported yet.")
  }
  
  ### 3. info: ###
  ncov <- length(names(CovariateMatrix))
  Infonames <-
    c(
      "random",
      "CAR",
      "continuous",
      "in.landings",
      "nlev",
      "interaction",
      "in.slopeModel"
    )
  nInfo <- length(Infonames)
  info <- array(0L, dim = c(ncov, nInfo))
  #info <- info + seq_along(info)
  colnames(info) <- Infonames
  rownames(info) <- names(CovariateMatrix)
  
  # 3.1. random:
  info[eca$resources$covariateInfo$name, "random"] <-
    eca$resources$covariateInfo$covType == "Random"
  
  # 3.2. CAR:
  # Make sure the neighbours are ordered according to the 1:n values in the covariateLink:
  
  if (is.numeric(eca$resources$covariateLink$spatial[, 2])){
    ind <-
      match(as.numeric(names(eca$stratumNeighbour)), eca$resources$covariateLink$spatial[, 2])
  }
  else{
    ind <-
      match(names(eca$stratumNeighbour), eca$resources$covariateLink$spatial[, 2])  
  }
  if (!all(sort(ind) == ind)) {
    eca$stratumNeighbour <- eca$stratumNeighbour[match(eca$resources$covariateLink$spatial[, 2], names(eca$stratumNeighbour))]
    ind <-
      match(names(eca$stratumNeighbour), eca$resources$covariateLink$spatial[, 2])  
  }
  
  stopifnot(all(sort(ind) == ind))
  
  names(eca$stratumNeighbour) <-
    eca$resources$covariateLink$spatial[ind, 1]
  numNeighbours <-
    unlist(lapply(eca$stratumNeighbour, length), use.names = F)
  idNeighbours <- unlist(eca$stratumNeighbour, use.names = F)
  idNeighbours <-
    eca$resources$covariateLink$spatial[match(idNeighbours, eca$resources$covariateLink$spatial[, 2]), 1]
  CARNeighbours <-
    list(numNeighbours = numNeighbours, idNeighbours = idNeighbours)
  
  info[eca$resources$covariateInfo$name, "CAR"] <-
    eca$resources$covariateInfo$CAR
  
  # 3.3. continuous:
  if (length(modelSpecification$continuous)) {
    info[names(modelSpecification$continuous), "continuous"] <-
      unlist(modelSpecification$continuous)
  }
  
  # 3.4. in.landings:
  info[rownames(info), "in.landings"] <-
    as.integer(rownames(info) %in% names(eca$landing))
  info["constant", "in.landings"] <- 1
  
  # 3.5. interaction:
  if (length(modelSpecification$interaction)) {
    info[names(modelSpecification$interaction), "interaction"] <-
      unlist(modelSpecification$interaction)
  }
  else{
    # defaults to no interactions.
    # To make it defaults to interaction term for all covariates in landings
    # info[, "interaction"] <- info[, "in.landings"]
    info["constant", "interaction"] <- 0
  }
  
  # 3.6. include.slope:
  if (length(modelSpecification$in.slopeModel)) {
    info[names(modelSpecification$in.slopeModel), "in.slopeModel"] <-
      unlist(modelSpecification$in.slopeModel)
  }
  
  info <- getHardCoded(info)
  # 3.7. nlev:
  info[rownames(info), "nlev"] <-
    apply(CovariateMatrix, 2, function(x)
      max(x))
  # Continuous covariates should have only one level:
  info[info[, "continuous"] == 1, "nlev"] <- 1
  
  # random covariates in landings should have levels equal to max of landing and samples (if that is different from max of landings, something is wrong, but that must be checked later)
  if (sum(info[, "random"] == 1 & info[, "in.landings"] == 1) > 0) {
    for (n in rownames(info)) {
      if (info[n, "random"] == 1 & info[n, "in.landings"] == 1) {
          info[n, "nlev"] <-
            length(unique((c(eca$landing[[n]], eca$biotic[[n]]))))
      }
    }
  }
  
  return(list(info = info,
              CARNeighbours = CARNeighbours))
}


#' Identifies whichindividuals are from catchsamples with some age readings.
#' @return logical vector identifying which individuals belong to a catchsample where age was sampled
#' @keywords internal
hasAgeInSample <- function(biotic){
  hasage<-biotic[!is.na(biotic$age),c("serialnumber", "catchcategory", "catchpartnumber")]
  return(paste(biotic$serialnumber, biotic$catchcategory, biotic$catchpartnumber, sep="/") %in% paste(hasage$serialnumber, hasage$catchcategory, hasage$catchpartnumber, sep="/"))
}


#' Function for converting to the input format required by ECA (this is the main function):
#' @param onlyagestations If true, only hauls with some aged individiuals are used
#' @keywords internal
getLengthGivenAge_Biotic <- function(eca, hatchDaySlashMonth, minage, maxage, onlyagestations=F) {
  
  if (onlyagestations){
    valid <- hasAgeInSample(eca$biotic)
    eca$biotic <- eca$biotic[valid, , drop = FALSE]
    eca$covariateMatrixBiotic <- eca$covariateMatrixBiotic[valid, , drop = FALSE]
  }
  
  ### 1. DataMatrix: ###
  temp <-
    getDataMatrixANDCovariateMatrix(eca, vars = c("age", "yearday"))
  DataMatrix <- temp$DataMatrix
  CovariateMatrix <- temp$CovariateMatrix
  resources <- temp$resources
  
  
  # Estimate the remainder for real age by use of the hatchDaySlashMonth:
  numDaysOfYear <- 366
  DataMatrix$part.year <- (DataMatrix$yearday - getMidSeason(hatchDaySlashMonth) + 1) / numDaysOfYear
  DataMatrix$yearday <- NULL
  
  ### 3. info: ###
  info <- getInfo(eca, CovariateMatrix)
  
  #reduce ageerror matrix to ages actually used
  ageerrormatrix <- eca$ageError
  ageerrormatrix <- ageerrormatrix[rownames(ageerrormatrix) %in% minage:maxage, colnames(ageerrormatrix) %in% minage:maxage]
  
  ### Return a list of the data: ###
  out <- list(
    DataMatrix = DataMatrix,
    CovariateMatrix = CovariateMatrix,
    CARNeighbours = info$CARNeighbours,
    AgeErrorMatrix = ageerrormatrix,
    info = info$info,
    resources = resources
  )
  
  out$ClassificationErrorVector <- eca$otholiterror
  return(out)
}

#' Function for converting to the input format required by ECA (this is the main function):
#' @keywords internal
getWeightGivenLength_Biotic <- function(eca) {
  # Extract the non-NAs:
  
  var <- "individualweightgram"
  # Remove missing values from the DataMatrix and from the eca$covariateMatrixBiotic:
  valid <- !is.na(eca$biotic[[var]])
  eca$biotic <- eca$biotic[valid, , drop = FALSE]
  eca$covariateMatrixBiotic <-
    eca$covariateMatrixBiotic[valid, , drop = FALSE]
  
  #check that individual weights are of the same product type.
  if (length(unique(eca$biotic$individualproducttype)) != 1){
    producttypes <- unique(eca$biotic$individualproducttype)
    stop(paste("Heterogenous product type composition. Consider using the function 'ConvertLengthAndWeight'. Product types ('individualproducttype') in data:", paste(producttypes, collapse=",")))
  }
  
  ### 1. DataMatrix: ###
  temp <-
    getDataMatrixANDCovariateMatrix(eca, vars = var)
  DataMatrix <- temp$DataMatrix
  CovariateMatrix <- temp$CovariateMatrix
  resources <- temp$resources
  
  # convert weight to KG, since it is in grams in StoX:
  weightunit <- 1e-3
  DataMatrix <-
    cbind(weightKG = eca$biotic$individualweightgram * weightunit, DataMatrix)
  
  ### 3. info: ###
  info <- getInfo(eca, CovariateMatrix)
  
  ### Return a list of the data: ###
  out <- list(
    DataMatrix = DataMatrix,
    CovariateMatrix = CovariateMatrix,
    CARNeighbours = info$CARNeighbours,
    #AgeErrorMatrix = eca$ageError, # This is not needed for WeightGivenLength
    info = info$info,
    resources = resources
  )
  out$ClassificationErrorVector <- eca$otholiterror
  return(out)
}

get_default_result_dir <-
  function(projectName,
           location = getProjectPaths(projectName)$RDataDir) {
    return(file.path(location, "reca"))
  }


#
# Functions for running RECA
#


#' @title Prepare data for RECA
#' @description Convert data to exported from stox to eca format. Save results to project data 'prepareRECA'
#' @details Most parameters to this funciton are set as named members of a list which is passed as argument GlobalParameters to \code{\link[Reca]{eca.estimate}}
#'    The parameters minage and maxage define the range of ages that are considered possible in the model. Because R-ECA integrates weight and length measurements, and allows for modelling errors in age determination, predicted ages might fall outside the age range in samples. minage and maxage should be set with this in mind.
#' @param projectName name of stox project
#' @param minage see specification for GlobalParameters in \code{\link[Reca]{eca.estimate}}.
#' @param maxage see specification for GlobalParameters in \code{\link[Reca]{eca.estimate}}
#' @param delta.age see specification for GlobalParameters in \code{\link[Reca]{eca.estimate}}
#' @param maxlength maximum length of fish in the data set in cm. If null the value will be extracted from the data.
#' @param hatchDaySlashMonth reference day for assumed spawning time of fish, formatted as day / month. Used to estimate fractional age of fish.
#' @param temporalresolution temporal resolution for the aggregated landings in days (used to set midSeason the Landings object for \code{\link[Reca]{eca.predict}})
#' @param resultdir location where R-ECA will store temporal files. Defaults (if null) to a subdirectory of getProjectPaths(projectName)$RDataDir called `reca` whcih will be created if it does not already exist
#' @param overwrite logical, if true, projectData for prepareRECA and runRECA will be nulled before running, and resultdir will be cleaned of any existing output files located in subdirectories cfiles and resfiles.
#' @param agedstationsonly logical, if true, only hauls with some aged individuals will be used for the age model. This does not affect the weight-length model
#' @export
prepareRECA <-
  function(projectName,
           resultdir = NULL,
           minage = 1,
           maxage = 20,
           delta.age = 0.001,
           maxlength = NULL,
           hatchDaySlashMonth = "01/01",
           temporalresolution = 92,
           overwrite=T,
           agedstationsonly=F) {
    if (is.null(resultdir)) {
      resultdir <- get_default_result_dir(projectName)
      if (!(file.exists(resultdir))) {
        dir.create(resultdir, recursive = T)
      }
    }
    if (!(file.exists(resultdir))) {
      stop(paste("Directory", resultdir, "does not exist."))
    }
    
    if (overwrite){
      message("Running prepareECA with overwrite=T")
      setProjectData(
        projectName = projectName,
        var = NULL,
        name = "prepareRECA"
      )
      setProjectData(
        projectName = projectName,
        var = NULL,
        name = "runRECA"
      )
      saveProjectData(projectName)
    }
    
    #clean resultdir if needed
    if (length(list.files(resultdir))>0 & overwrite){
        for (f in list.files(file.path(resultdir, "cfiles"))){
          fn <- file.path(resultdir, "cfiles", f)
          if (file.exists(fn) && !dir.exists(fn)){
            unlink(fn)
          }
        }
        unlink(file.path(resultdir, "cfiles"), recursive=T)
        for (f in list.files(file.path(resultdir, "resfiles"))){
          fn <- file.path(resultdir, "resfiles", f)
          if (file.exists(fn) && !dir.exists(fn)){
            unlink(fn)
          }
        }
        unlink(file.path(resultdir, "resfiles"), recursive = T)
    }
    if (length(list.files(resultdir))>0){
      stop(paste("Directory", resultdir, "contains files."))
    }
    message("checking filepath char comp")
    if (grepl(" ", resultdir)) {
      stop(paste(
        "Please make ecadir",
        "(current:",
        resultdir,
        ") contain no spaces."
      ))
    }
    eca <- baseline2eca(projectName)
    eca$temporalresolution <- temporalresolution

    
    #
    # run data checks here.
    # This is not actually the correct place, as it should ideally be run with reports, but checks need to be run before reca, and I don't want to put it in baseline report as the distinction between baseline report and r report will dissapear in next release.
    # Consider moving this in StoX 3.0
    #
    tryCatch({
      stationissuesfilename <-
        file.path(getProjectPaths(projectName)$RReportDir,
                  "stationissues.txt")
      catchissuesfilename <-
        file.path(getProjectPaths(projectName)$RReportDir,
                  "catchissues.txt")
      imputationissuesfilename  <-
        file.path(getProjectPaths(projectName)$RReportDir,
                  "imputationissues.txt") 
      
      makeDataReportReca(eca$biotic, stationissuesfilename, catchissuesfilename, imputationissuesfilename, T, covariates=names(eca$covariateMatrixBiotic), eca$landing)  
      
      if (file.exists(stationissuesfilename)){
        out$filename <- c(stationissuesfilename, out$filename)        
      }  
      if (file.exists(catchissuesfilename)){
        out$filename <- c(catchissuesfilename, out$filename)        
      }
      if (file.exists(imputationissuesfilename)){
        out$filename <- c(imputationissuesfilename, out$filename)      
      }
      
    },
    error = function(e) {
    },
    finally = {
      
    })  
    
    
    #max length in cm
    if (is.null(maxlength)) {
      maxlength <- max(eca$biotic$lengthcentimeter)
    }
    #consider if it makes sense to extract from data for minage and maxage as well
    
    #
    # convert data
    #
    
    GlobalParameters <- getGlobalParameters(eca$biotic, resultdir, maxlength, minage, maxage, delta.age)
    AgeLength <- getLengthGivenAge_Biotic(eca, hatchDaySlashMonth, minage, maxage, onlyagestations=agedstationsonly)
    WeightLength <- getWeightGivenLength_Biotic(eca)
    Landings <- getLandings(eca$landing, AgeLength, WeightLength, landingresolution = temporalresolution)
    
    #
    # do not run data checks here, as plots need to be made and data will not be written for fails
    #
    
    
    #
    # store results
    #
    
    setProjectData(
      projectName = projectName,
      var = list(
        GlobalParameters = GlobalParameters,
        Landings = Landings,
        WeightLength = WeightLength,
        AgeLength = AgeLength,
        StoxExport = eca
      ),
      name = "prepareRECA"
    )
  }

#' @title run RECA
#' @description Loads data produced by \code{\link{prepareRECA}}, run tests on model configuration, runs parameterization and makes predictions using RECA. Saves results to project data 'runRECA'
#' @details Most parameters to this function are appended to the argument list produced by prepareRECA and passed as argument GlobalParameters to \code{\link[Reca]{eca.estimate}} and \code{\link[Reca]{eca.predict}}.
#'     For purposes of testing and running ECA detached from StoX, Data files accepted by code{\link[Reca]{eca.estimate}} and \code{\link[Reca]{eca.predict}} can be exported using the option export_only. In this case the analysis is not run, but data files and parameter files are stored at the designated location.
#' @param projectName name of stox project
#' @param burnin see specification for GlobalParameters in \code{\link[Reca]{eca.estimate}}
#' @param caa.burnin see specification for GlobalParameters in \code{\link[Reca]{eca.predict}}
#' @param nSamples see specification for GlobalParameters in \code{\link[Reca]{eca.estimate}}
#' @param thin see specification for GlobalParameters in \code{\link[Reca]{eca.estimate}}
#' @param fitfile see specification for GlobalParameters in \code{\link[Reca]{eca.estimate}}
#' @param predfile see specification for GlobalParameters in \code{\link[Reca]{eca.predict}}
#' @param lgamodel see specification for GlobalParameters in \code{\link[Reca]{eca.estimate}}
#' @param CC see specification for GlobalParameters in \code{\link[Reca]{eca.estimate}}
#' @param CCError see specification for GlobalParameters in \code{\link[Reca]{eca.estimate}}
#' @param seed see specification for GlobalParameters in \code{\link[Reca]{eca.estimate}}
#' @param age.error see specification for GlobalParameters in \code{\link[Reca]{eca.estimate}}
#' @param export_only if not NULL this indicates that eca should not be run, but all parameters should be exported to the file export_only
#'
#' @export
runRECA <-
  function(projectName,
           burnin = 100,
           caa.burnin = 100,
           nSamples = 1000,
           thin = 1,
           fitfile = "fit",
           predfile = "pred",
           lgamodel = "log-linear",
           CC = FALSE,
           CCError = FALSE,
           seed = NULL,
           age.error = FALSE,
           export_only = NULL) {
    requireNamespace("Reca")
    
    # Sett run parameters her, sett dataparametere i prep_eca
    prepdata <- loadProjectData(projectName, var = "prepareRECA")
    if (is.null(prepdata)) {
      stop("Could not load project data")
    }

    if (is.null(seed)){
      seed <- getseed()
    }

    prepareRECA <- prepdata$prepareRECA
    GlobalParameters <- prepareRECA$GlobalParameters
    AgeLength <- prepareRECA$AgeLength
    WeightLength <- prepareRECA$WeightLength
    Landings <- prepareRECA$Landings
    
    GlobalParameters$caa.burnin <- caa.burnin
    GlobalParameters$burnin <- burnin
    GlobalParameters$nSamples <- nSamples
    GlobalParameters$thin <- thin
    GlobalParameters$fitfile <- fitfile
    GlobalParameters$predictfile <- predfile
    GlobalParameters$lgamodel <- lgamodel
    GlobalParameters$CC <- CC
    GlobalParameters$CCerror <- CCError
    GlobalParameters$age.error <- age.error
    GlobalParameters$seed <- seed
    
    #
    # Run checks
    #
    
    checkAgeLength(AgeLength, checkAgeErrors=age.error)
    checkWeightLength(WeightLength)
    checkCovariateConsistency(AgeLength, Landings$AgeLengthCov)
    checkCovariateConsistency(WeightLength, Landings$WeightLengthCov)
    checkLandings(Landings)
    checkGlobalParameters(GlobalParameters, AgeLength, WeightLength)
    
    write("######", stdout())
    write("Running ECA with model configuration:", stdout())
    writeRecaConfiguration(GlobalParameters,
                           Landings,
                           WeightLength,
                           AgeLength,
                           fileobj = stdout())
    write("######", stdout())
    
    if (!is.null(export_only)) {
      save(GlobalParameters, AgeLength, WeightLength, Landings, file = export_only)
      return(NULL)
    }
    else{
      ## Estimate model
      fit <-
        Reca::eca.estimate(AgeLength, WeightLength, Landings, GlobalParameters)
      
      ## Predict
      pred <-
        Reca::eca.predict(AgeLength, WeightLength, Landings, GlobalParameters)
      
      return(setProjectData(
        projectName = projectName,
        var = list(fit = fit, pred = pred, GlobalParameters=GlobalParameters),
        name = "runRECA"
      ))
    }
  }

#
# Functions for plotting data and results from RECA
#

#' Splits a prediction object from running coastal cod analysis into two prediction objects, one for each stock
#' @param prediction object for CC analysis
#' @return list with members 'coastal' and 'atlantic' containing prediction objects for coastal and atlantic cod respectively
#' @keywords internal
splitPredCC <- function(pred){
  ret <- list()
  ncat <- length(unique(pred$AgeCategories))
  ret$coastal <- pred
  ret$atlantic <- pred
  
  ret$atlantic$AgeCategories <- ret$atlantic$AgeCategories[1:ncat]
  ret$coastal$AgeCategories <- ret$coastal$AgeCategories[(ncat+1):(2*ncat)]
  
  ret$atlantic$TotalCount <- ret$atlantic$TotalCount[,1:ncat,]
  ret$coastal$TotalCount <- ret$coastal$TotalCount[,(ncat+1):(2*ncat),]
  
  ret$atlantic$MeanWeight <- ret$atlantic$MeanWeight[1:ncat,]
  ret$coastal$MeanWeight <- ret$coastal$MeanWeight[(ncat+1):(2*ncat),]

  ret$atlantic$MeanLength <- ret$atlantic$MeanLength[1:ncat,]
  ret$coastal$MeanLength <- ret$coastal$MeanLength[(ncat+1):(2*ncat),]
  
  return(ret)  
}

#' Splits biotic frame by otolith type
#' @param biotic frame
#' @return list with members 'coastal' and 'atlantic' containing biotic entries for coastal and atlantic cod respectively
#' @keywords internal
splitBioticCC <- function(biotic){
  ret <- list()
  ret$coastal <- biotic
  ret$atlantic <- biotic
  
  ret$coastal <- ret$coastal[!is.na(ret$coastal$otolithtype) & ret$coastal$otolithtype %in% c(1,2),]
  ret$atlantic <- ret$atlantic[!is.na(ret$atlantic$otolithtype) & ret$atlantic$otolithtype %in% c(4,5),]
  
  return(ret)
}

#' Generates plots and reports from RECA prediction
#' @param projectName name of stox project
#' @param verbose logical, if TRUE info is written as messages
#' @param format function defining filtetype for plots, supports grDevices::pdf, grDevices::png, grDevices::jpeg, grDevices::tiff, grDevices::bmp
#' @param ... parameters passed on plot function and format
#' @return list, with at least one named element 'filename', a vector of file-paths to generated plots.
#' @export
plotRECAresults <-
  function(projectName,
           verbose = F,
           format = "png",
           ...) {
    out <- list()
    out$filename <- c()
    
    rundata <- loadProjectData(projectName, var = "runRECA")
    
    if (length(rundata) == 0) {
      return(NULL)
    }
    
    prep <- loadProjectData(projectName, var = "prepareRECA")
    
    if (format == "png") {
      #dimension in pixels
      width = 5000
      height = 5000
      res = 500
    }
    if (format == "pdf") {
      #dimension in inches
      width = 10
      height = 10
      res = NULL
    }
    
    #different plot if stock splitting (CC) is used
    if (is.null(rundata$runRECA$GlobalParameters$CC) || !rundata$runRECA$GlobalParameters$CC){
      fn <-
        formatPlot(projectName, "RECA_traceplot", function() {
          plotMCMCagetraces(rundata$runRECA$pred,
                            ...)
        }, verbose = verbose, format = format, height = height, width = width, res =
          res, ...)
      out$filename <- c(fn, out$filename)
      
      fn <-
        formatPlot(projectName, "RECA_results", function() {
          plot_RECA_results_panel(rundata$runRECA$pred,
                                  prep$prepareRECA$StoxExport$biotic,
                                  ...)
        }, verbose = verbose, format = format, height = height, width = width, res =
          res, ...)
      out$filename <- c(fn, out$filename)
      
    }
    else if(rundata$runRECA$GlobalParameters$CC){
      
      ccpred <- splitPredCC(rundata$runRECA$pred)
      ccbiotic <- splitBioticCC(prep$prepareRECA$StoxExport$biotic)
      
      fn <-
        formatPlot(projectName, "RECA_results_coastal", function() {
          plot_RECA_results_panel(ccpred$coastal,
                                  ccbiotic$coastal,
                                  main="Coastal cod",
                                  ...)
        }, verbose = verbose, format = format, height = height, width = width, res =
          res, ...)
      out$filename <- c(fn, out$filename)
      
      
      fn <-
        formatPlot(projectName, "RECA_traceplot_coastal", function() {
          plotMCMCagetraces(ccpred$coastal,
                            ...)
        }, verbose = verbose, format = format, height = height, width = width, res =
          res, ...)
      out$filename <- c(fn, out$filename)
      
      
      fn <-
        formatPlot(projectName, "RECA_results_atlantic", function() {
          plot_RECA_results_panel(ccpred$atlantic,
                                  ccbiotic$atlantic,
                                  main="Atlantic cod",
                                  ...)
        }, verbose = verbose, format = format, height = height, width = width, res =
          res, ...)
      out$filename <- c(fn, out$filename)
      
      fn <-
        formatPlot(projectName, "RECA_traceplot_atlantic", function() {
          plotMCMCagetraces(ccpred$antlantic,
                            ...)
        }, verbose = verbose, format = format, height = height, width = width, res =
          res, ...)
      out$filename <- c(fn, out$filename)
      
    }
    
    return(out)
  }

#' @title Diagonostics RECA
#' @description Generate plots for diagnosis of RECA model configuration.
#' @details Plots are made conditional on problems. E.g. Fixed effects plot is not made, if all combinations of fixed effects were sampled.
#' @param projectName name of stox project
#' @param verbose logical, if TRUE info is written as messages
#' @param format function defining filtetype for plots, supports grDevices::pdf, grDevices::png, grDevices::jpeg, grDevices::tiff, grDevices::bmp
#' @param ... parameters passed on to plot function and format
#' @return list, with at least one named element 'filename', a vector of file-paths to generated plots.
#' @export
diagnosticsRECA <-
  function(projectName,
           verbose = F,
           format = "png",
           ...) {
    out <- list()
    out$filename <- c()
    
    prep <- loadProjectData(projectName, var = "prepareRECA")
    
    if (length(prep) == 0) {
      return(NULL)
    }
    
    covariateConsistencyIssues <- T
    tryCatch({
      checkCovariateConsistency(prep$prepareRECA$AgeLength,
                                prep$prepareRECA$Landings$AgeLengthCov)
      checkCovariateConsistency(prep$prepareRECA$WeightLength,
                                prep$prepareRECA$Landings$WeightLengthCov)
      covariateConsistencyIssues <- F
    },
    error = function(e) {
    },
    finally = function(e) {
    })
    
    stoxexp <- prep$prepareRECA$StoxExpor
    
    #
    # Plots for dealing with covariate consistency issues
    #
    if (covariateConsistencyIssues) {
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
      
      fn <-
        formatPlot(projectName, "RECA_config_issue_fixed_effects", function() {
          diagnostics_model_configuration(stoxexp, ...)
        }, verbose = verbose, format = format, height = height, width = width, res =
          res, ...)
      out$filename <- c(fn, out$filename)
      
      
      if (format == "png") {
        #dimension in pixels
        width = 5000
        height = 5000
        res = 500
      }
      if (format == "pdf") {
        #dimension in inches
        width = 10
        height = 10
        res = NULL
      }
    }
    
    return(out)
  }

#' @title plotSamplingOverview
#' @description Generate plots to show composition of samples wrp activity in fisheries
#' @details Compares sampling effort to fisheries along covariates selected in the model, and along some standard covariate choices if available (gear, temporal and spatial). Plots compositions of samples with respect to some important variables informative of sampling heterogenety
#' @param projectName name of stox project
#' @param verbose logical, if TRUE info is written as messages
#' @param format function defining filtetype for plots, supports grDevices::pdf, grDevices::png, grDevices::jpeg, grDevices::tiff, grDevices::bmp
#' @param ... parameters passed on to plot function and format
#' @return list, with at least one named element 'filename', a vector of file-paths to generated plots.
#' @export
plotSamplingOverview <-
  function(projectName,
           verbose = F,
           format = "png",
           ...) {
    out <- list()
    out$filename <- c()
    
    prep <- loadProjectData(projectName, var = "prepareRECA")
    
    if (length(prep) == 0) {
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
      fn <-
        formatPlot(projectName, "RECA_cell_coverage", function() {
          diagnosticsCoverageRECA(stoxexp, ...)
        }, verbose = verbose, format = format, height = height, width = width, res =
          res, ...)
      out$filename <- c(fn, out$filename)
    }
    else{
      message(
        paste(
          "Need all",
          "gearfactor",
          "temporal",
          "spatial",
          "as covariates to produce RECA_cell_coverage"
        )
      )
    }
    
    #
    # Samples by cells
    #
    
    if (all(c("gearfactor", "temporal", "spatial") %in% stoxexp$resources$covariateInfo$name)) {
      rows <-
        nrow(unique(get_gta_landings(stoxexp)[, c("gearfactor", "temporal")]))
      colnamefactor <- mean(unlist(lapply(unique(get_gta_landings(stoxexp)$spatial), FUN=function(x){max(5,nchar(x))})))/5
      cols <- length(unique(get_gta_landings(stoxexp)$spatial)) * colnamefactor
      
      cols <- max(13, cols)
      rows <- max(3, rows)
      
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
      
      fn <-
        formatPlot(projectName, "RECA_samples_by_cells", function() {
          diagnosticsSamplesRECA(stoxexp, ...)
        }, verbose = verbose, format = format, height = height, width = width, res =
          res, ...)
      out$filename <- c(fn, out$filename)
    }
    else{
      message(
        paste(
          "Need all",
          "gearfactor",
          "temporal",
          "spatial",
          "as covariates to produce RECA_samples_by_cells"
        )
      )
    }
    
    if (format == "png") {
      #dimension in pixels
      width = 5000
      height = 5000
      res = 500
    }
    if (format == "pdf") {
      #dimension in inches
      width = 10
      height = 10
      res = NULL
    }
    
    fn <-
      formatPlot(projectName, "RECA_sample_composition", function() {
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
plotRECA <- function(projectName, ...) {
  out <- list()
  out$filename <- c()
  
  tryCatch({
    fn <- plotSamplingOverview(projectName, ...)
    out$filename <- c(fn$filename, out$filename)
  },
  finally = {
    
  })
  
  tryCatch({
    fn <- plotRECAresults(projectName, ...)
    out$filename <- c(fn$filename, out$filename)
  },
  finally = {
    
  })
  
  tryCatch({
    fn <- diagnosticsRECA(projectName, ...)
    out$filename <- c(fn$filename, out$filename)
  },
  finally = {
    
  })
  
  return(out)
}

#' @title Writes RECA configuration
#' @description Writes details about the model configuration to a text file or open connection
#' @details Configuration are saved reflecting the parameters as passed to \code{\link[Reca]{eca.estimate}} and \code{\link[Reca]{eca.predict}}, even if it is possible to have for instance the GlobalParameters differ between the two for a valid execution.
#' @param GlobalParameters defined in \code{\link[Reca]{eca.estimate}} and \code{\link[Reca]{eca.predict}}
#' @param Landings defined in \code{\link[Reca]{eca.estimate}} and \code{\link[Reca]{eca.predict}}
#' @param WeightLength defined in \code{\link[Reca]{eca.estimate}} and \code{\link[Reca]{eca.predict}}
#' @param AgeLength defined in \code{\link[Reca]{eca.estimate}} and \code{\link[Reca]{eca.predict}}
#' @param fileobj filename or open connection
#' @param main header to write before configuration
#' @keywords internal
writeRecaConfiguration <-
  function(GlobalParameters,
           Landings,
           WeightLength,
           AgeLength,
           fileobj,
           main = NULL) {
    if (length(class(fileobj)) > 1) {
      if ("connection" %in% class(fileobj)) {
        f <- fileobj
      }
      else{
        stop(paste(
          "Unsupported class for parameter fileobj: ",
          class(fileobj)
        ))
      }
    }
    else if (class(fileobj) == "character") {
      f <- file(fileobj, open = "w")
    }
    else{
      stop(paste("Unsupported class for parameter fileobj: ", class(fileobj)))
    }
    if (!is.null(main)) {
      write(main, f)
    }
    formatinfo <- function(info) {
      info <- as.data.frame(info)
      info$covariate <- row.names(info)
      info <-
        info[, c(length(names(info)), seq(1, length(names(info)) - 1))]
      return(info)
    }
    write(paste("Total landings:", sum(Landings$LiveWeightKG), "kg"), f)
    write("Length given age model:", f)
    write.table(
      formatinfo(AgeLength$info),
      sep = "\t",
      dec = ".",
      row.names = F,
      file = f
    )
    write(paste("stations (hauls):", nrow(AgeLength$CovariateMatrix)), f)
    write(paste("individuals:", nrow(AgeLength$DataMatrix)), f)
    write("Weight given length model:", f)
    write.table(
      formatinfo(WeightLength$info),
      sep = "\t",
      dec = ".",
      row.names = F,
      file = f
    )
    write(paste("stations (hauls):", nrow(WeightLength$CovariateMatrix)), f)
    write(paste("individuals:", nrow(WeightLength$DataMatrix)), f)
    write("Run-parameters:", f)
    write.table(t(as.data.frame(GlobalParameters)), col.names = F, f)
    
    if (length(class(fileobj)) == 1) {
      if (class(fileobj) == "character") {
        close(f)
      }
    }
  }

#' calculates catch matrix for given variable and unit
#' @return list with three members (means, cv and caa_scaled)
#' @keywords internal
getCatchMatrix <- function(pred,
                           var = "Abundance",
                           unit = "ones",
                           plusgr=NULL){
  if (var == "Abundance" | var == "Count") {
    plottingUnit = getPlottingUnit(
      unit = unit,
      var = var,
      baseunit = "ones",
      def.out = F
    )
    caa <- round(apply(pred$TotalCount, c(2, 3), sum))

  }
  else if (var == "Weight") {
    caa <- apply(pred$TotalCount, c(2, 3), sum) * pred$MeanWeight
    plottingUnit = getPlottingUnit(
      unit = unit,
      var = var,
      baseunit = "kilograms",
      def.out = F
    )
  }
  else{
    stop("Not implemented")
  }
  
  ages <- as.character(pred$AgeCategories)
  
  if (!is.null(plusgr)){
    caa[plusgr,] <- colSums(caa[plusgr:nrow(caa),])
    caa <- caa[1:plusgr,]
    ages <- ages[1:plusgr]
    ages[plusgr] <- paste(ages[plusgr], "+", sep="")
  }
  
  caa_scaled <- as.data.frame(caa / plottingUnit$scale)
  means <-
    as.data.frame(list(age = ages, mean = rowMeans(caa_scaled)))
  cv <-
    as.data.frame(list(
      age = ages,
      sd = apply(caa_scaled, FUN = sd, MARGIN = 1)
    ))
  cv$cv <- cv$sd / means$mean
  colnames(caa_scaled) <- paste("Iteration", 1:ncol(caa_scaled))
  caa_scaled$age <- ages
  caa_scaled <-
    caa_scaled[, names(caa_scaled)[order(names(caa_scaled))]]
  
  
  tab <- list()
  tab$means <- means
  tab$cv <- cv
  tab$caa_scaled <- caa_scaled

  return (tab)
  
}


#' calculates catch matrix for given variable and unit
#' @return data.frame with columns: age, meanLength, meanWeight, sd.of.meanLength, sd.of.meanWeight
#' @keywords internal
getAgeGroupParamaters <- function(pred,
                           plusgr=NULL){

  ages <- as.character(pred$AgeCategories)
  abundances <- apply(apply(pred$TotalCount, c(2, 3), sum), 1, mean)
  pred$MeanWeight <- pred$MeanWeight*1000
  weights <- apply(pred$MeanWeight, 1, mean)
  weights.var <- apply(pred$MeanWeight, 1, var)
  lengths <- apply(pred$MeanLength, 1, mean)
  lengths.var <- apply(pred$MeanLength, 1, var)
  
  if (!is.null(plusgr)){
    #mean of age groups in plusgroup, weighted by age group abundance
    weights[plusgr] <- weights[plusgr:length(weights)] %*% (abundances[plusgr:length(abundances)]/sum(abundances[plusgr:length(abundances)]))
    weights.var[plusgr] <- weights.var[plusgr:length(weights.var)] %*% (abundances[plusgr:length(abundances)]/sum(abundances[plusgr:length(abundances)]))**2
    lengths[plusgr] <- lengths[plusgr:length(lengths)] %*% (abundances[plusgr:length(abundances)]/sum(abundances[plusgr:length(abundances)]))
    lengths.var[plusgr] <- lengths.var[plusgr:length(lengths.var)] %*% (abundances[plusgr:length(abundances)]/sum(abundances[plusgr:length(abundances)]))**2
    weights <- weights[1:plusgr]
    weights.var <- weights.var[1:plusgr]
    lengths <- lengths[1:plusgr]
    lengths.var <- lengths.var[1:plusgr]
    ages <- ages[1:plusgr]
    ages[plusgr] <- paste(ages[plusgr], "+", sep="")
  }
  
  lengths.sd <- sqrt(lengths.var)
  weights.sd <- sqrt(weights.var)
  
  tab <- data.frame(age=ages, meanLengthCm=lengths, meanLengthCm.sd=lengths.sd, meanWeightsG=weights, meanWeightsG.sd=weights.sd)
  
  return (tab)
  
}

#' @title Save catch at age matrix
#' @description Write catch at age predicted by \code{\link[Reca]{eca.predict}} as csv file.
#' @details Catch at age matrix is written as comma-separated file with quoted strings as row/column names.
#'    Each row correspond to an age group, and columns to either means or an iteration of the Monte Carlo simulation.
#'    Units are controlled by parameters, and written as metainformation in a preamble identified by the comment charater '#', along with any text provided in other arguments (parameter main).
#' @param pred as returned by \code{\link[Reca]{eca.predict}}
#' @param filename name of file to save to.
#' @param var Variable to extract. Allows for Abundance, Count or Weight
#' @param unit Unit for extracted variable. See \code{\link{getPlottingUnit}}
#' @param main Title for the analysis, to be included as comment in saved file (e.g. species and year)
#' @param savemeans If True, only means and spread statistics for each age group will be saved, otherwise extracted variable is saved for each iteration of the Monte Carlo simulation.
#' @param plusgr Lower age in plusgr for tabulation. If NULL plusgr is not used.
#' @keywords internal
saveCatchMatrix <-
  function(pred,
           filename,
           var = "Abundance",
           unit = "ones",
           main = "",
           savemeans = F,
           plusgr=NULL) {
    comments <- c()
    if (savemeans) {
      title <- "Mean catch at age estimates"
    }
    else{
      title <- "Catch at age estimates"
    }
    
    if (var == "Abundance" | var == "Count") {
      
      if (unit == "ones") {
        comments <- c(paste(title, "as", var))
      }
      if (unit != "ones") {
        comments <- c(paste(title, "as", var, "in", unit))
      }
      
    }
    else if (var == "Weight") {
      comments <- c(paste(title, "as", var, "in", unit))
    }
    else{
      stop("Not implemented")
    }
    
    comments <- c(main, comments)
    
    tab <- getCatchMatrix(pred, var, unit, plusgr = plusgr)
    
    f <- file(filename, open = "w")
    write(paste("#", comments), f)
    if (savemeans) {
      write.table(
        merge(tab$means, tab$cv),
        file = f,
        sep = "\t",
        dec = ".",
        row.names = F
      )
      close(f)
    }
    else{
      write.table(
        tab$caa_scaled,
        file = f,
        sep = "\t",
        dec = ".",
        row.names = F
      )
      close(f)
    }
  }

#' @title Save catch at age variance-covariance matrix for age groups
#' @description Write catch at age covariance predicted by \code{\link[Reca]{eca.predict}} as csv file.
#' @details Covariance matrix is written as comma-separated file with quoted strings as row/column names.
#'    Each row and column correspond to an age group.
#'    Units for catch at age are controlled by parameters, and written as metainformation in a preamble identified by the comment charater '#', along with any text provided in other arguments (parameter main).
#' @param pred as returned by \code{\link[Reca]{eca.predict}}
#' @param filename name of file to save to.
#' @param var Variable to extract for covariance calculation. Allows for Abundance, Count or Weight
#' @param unit Unit for extracted variable. See \code{\link{getPlottingUnit}}
#' @param main Title for the analysis, to be included as comment in saved file (e.g. species and year)
#' @param standardize If True, pearson correlations are calculated, rather than covariances.
#' @param plusgr Lower age in plusgr for tabulation. If NULL plusgr is not used.
#' @keywords internal
saveCatchCovarianceMatrix <- function(pred,
                                      filename,
                                      var = "Abundance",
                                      unit = "ones",
                                      main = "",
                                      standardize=F,
                                      plusgr=NULL) {
  
  
  comments <- c()
  
  if (standardize){
    title <- "correlation matrix for age groups based on catch at age as"    
  }
  else{
    title <- "variance-Covariance matrix for age groups based on catch at age as"    
  }

  
  if (var == "Abundance" | var == "Count") {
    
    if (unit == "ones") {
      comments <- c(paste(title, "as", var))
    }
    if (unit != "ones") {
      comments <- c(paste(title, "as", var, "in", unit))
    }
    
  }
  else if (var == "Weight") {
    comments <- c(paste(title, "as", var, "in", unit))
  }
  else{
    stop("Not implemented")
  }
  
  comments <- c(main, comments)
  
  tab <- getCatchMatrix(pred, var, unit, plusgr=plusgr)

  caa_scaled <- tab$caa_scaled
  if (!standardize){
    covmat <- cov(t(caa_scaled))   
  }
  else{
    covmat <- cor(t(caa_scaled))   
  }
  
  rownames(covmat) <- tab$caa_scaled$age
  colnames(covmat) <- tab$caa_scaled$age
  
  f <- file(filename, open = "w")
  write(paste("#", comments), f)
    write.table(
      covmat,
      file = f,
      sep = "\t",
      dec = ".",
      row.names = T
    )
    close(f)
}

#' Make decomposed catch matrix
#' @description Compiles catch matrix decomposed on given variables
#' @details 
#'    Contain options for workaround variables, until reporting setup can be configured in stox.
#'    
#'    decomposition variables need not be the same as model covariates, but model covariates will be used for estimation.
#'    if the model covariates represent a finer decomposition of the sampling frame than the decomposition variables, interpretation is straightforward.
#'    if the model covariates represent a coarser decomposition of the sampling frame than the decomposition variables this implies an assumption of validity of parameteres outside the covariate combinations they are obtained for.
#'    
#'    Specifying either customMainAreaGrouping or customLocationGrouping, adds a column "spatialGroup" to the decomposition
#'    The spatial group is specified by the arguments 'customMainAreaGrouping' or 'customLocationGrouping' as some merge of either main-areas
#'    or combination of main-area location (Directorate of fisheries).
#'    
#' @param projectName Name identifying StoX project
#' @param filename filename to write decomposed catch matrix to. If NULL a filename in the projects R-report directory will be generated.
#' @param decomposition variables to use for decomposition, must be available for all rows in landings
#' @param addQuarterToDecomp workaround variable for adding quarter to decomp
#' @param customMainAreaGrouping optional, list mapping custom spatial groups to vectors of main area strings (2 character string <mainarea>, e.g. "12" or "09")
#' @param customLocationGrouping optional, list mapping custom spatial groups to vectors of location strings (5 character string <mainarea>-<location>, e.g.: "12-01" or "09-01")
#' @param var Variable to extract for calculation. Allows for Abundance, Count or Weight
#' @param unit Unit for extracted variable. See \code{\link{getPlottingUnit}}
#' @param plusgr Lower age in plusgr for tabulation. If NULL plusgr is not used.
#' @param main Title for the analysis, to be included as comment in saved file (e.g. species and year)
#' @return data frame with rows for each combination of decomposition variables and age groups, and columns with values or levels for age groups and decomposition variables, in addition to columns for the catch at age and standard deviation
#' @export
saveDecomposedCatchMatrix <- function(projectName, 
                                      filename=NULL, 
                                      decomposition=c("omr\u00e5degrupperingbokm\u00e5l"), 
                                      addQuarterToDecomp=T, 
                                      customMainAreaGrouping=NULL,
                                      customLocationGrouping=NULL,
                                      var = "Abundance",
                                      unit = "millions",
                                      plusgr=NULL,
                                      main = ""){
  
  
  if (length(customMainAreaGrouping) > 0 & length(customLocationGrouping) > 0){
    stop("You may specify only one of 'customLocationGrouping' and 'customLocationGrouping'")
  }
  
  customSpatialName <- "spatialGroup"
  if (length(customMainAreaGrouping) > 0){
    if (any(nchar(unlist(customMainAreaGrouping))!=2)){
      stop("Provide customMainAreaGrouping as 2-character strings. E.g. \"01\"")
    }
    decomposition <- c(decomposition, customSpatialName)
  }
  
  if (length(customLocationGrouping) > 0){
    if (any(nchar(unlist(customLocationGrouping))!=5)){
      stop("Provide customLocationGrouping as 5-character strings. E.g. \"01-01\"")
    }
    decomposition <- c(decomposition, customSpatialName)
  }
  
  if (is.null(filename)){
    resultdir <- getProjectPaths(projectName)$RReportDir
    filename <- file.path(resultdir, "decomposedcatch.csv")
  }
  
  quartcolumnname <- "Quarter"
  if (addQuarterToDecomp){
    decomposition <- c(decomposition, quartcolumnname)
  }
  getQuarter <- function(date){
    month <- substr(date, 6,7)
    month[month=="01" | month=="02" | month=="03"] <- "Q1"
    month[month=="04" | month=="05" | month=="06"] <- "Q2"
    month[month=="07" | month=="08" | month=="09"] <- "Q3"
    month[month=="10" | month=="11" | month=="12"] <- "Q4"
    return(month)
  }
  
  # load eca configuration and parameterization
  prepdata <- loadProjectData(projectName, var = "prepareRECA")
  rundata <- loadProjectData(projectName, var = "runRECA")
  if (is.null(prepdata) | is.null(rundata)) {
    stop("Could not load project data")
  }
  
  prepareRECA <- prepdata$prepareRECA
  
  projectlandings <- prepareRECA$StoxExport$landing
  if (addQuarterToDecomp){
    projectlandings[,quartcolumnname] <- getQuarter(projectlandings$sistefangstdato)
  }
  if (length(customMainAreaGrouping) > 0){
    areacodes <- sprintf("%02d", projectlandings[,"hovedomr\u00e5dekode"])
    
    if (!all(areacodes %in% unlist(customMainAreaGrouping))){
      missing <- unique(areacodes[!(areacodes %in% unlist(customMainAreaGrouping))])
      stop(paste("Custom group is not provided for all main areas. Missing: ", paste(missing, collapse=", ")))
    }
    
    groupedcodes <- rep(names(customMainAreaGrouping), unlist(lapply(customMainAreaGrouping, length)))
    projectlandings[,customSpatialName] <- groupedcodes[match(areacodes, unlist(customMainAreaGrouping))]
  }
  
  if (length(customLocationGrouping) > 0){
    locationcodes <- sprintf("%02d-%02d", projectlandings[,"hovedomr\u00e5dekode"], projectlandings[,"lokasjonkode"])
    
    if (!all(locationcodes %in% unlist(customLocationGrouping))){
      missing <- unique(areacodes[!(areacodes %in% unlist(customLocationGrouping))])
      stop(paste("Custom group is not provided for all main areas and locations. Missing: ", paste(missing, collapse=", ")))
    }
    
    groupedcodes <- rep(names(customLocationGrouping), unlist(lapply(customLocationGrouping, length)))
    projectlandings[,customSpatialName] <- groupedcodes[match(locationcodes, unlist(customLocationGrouping))]
  }
  
  projecttempres <- prepareRECA$StoxExport$temporalresolution
  
  
  AgeLength <- prepareRECA$AgeLength
  WeightLength <- prepareRECA$WeightLength
  runRECA <- rundata$runRECA
  GlobalParameters <- runRECA$GlobalParameters
  
  agglistproject <- list()
  for (n in decomposition){
    agglistproject[[n]]<-projectlandings[[n]]
  }
  
  decomps <- split.data.frame(projectlandings, f=agglistproject, drop=T)
  
  output <- NULL
  for (d in decomps){
    
    ## extract catchmatrix for decomposition
    decompLandings <- getLandings(d, AgeLength, WeightLength, projecttempres)
    pred <- Reca::eca.predict(AgeLength, WeightLength, decompLandings, GlobalParameters)
    catchmatrix <- getCatchMatrix(pred, var = var, unit = unit, plusgr=plusgr)

    decompmatrix <- merge(catchmatrix$means, catchmatrix$cv)
    decompmatrix[,decomposition]<-d[1,decomposition]
    
    if (is.null(output)){
      output <- decompmatrix
    }
    else{
      output <- rbind(output, decompmatrix)
    }
  }

  # add comments
  comments <- c()
  title <- "Mean catch at age estimates"
  if (var == "Abundance" | var == "Count") {
    
    if (unit == "ones") {
      comments <- c(paste(title, "as", var))
    }
    if (unit != "ones") {
      comments <- c(paste(title, "as", var, "in", unit))
    }
    
  }
  else if (var == "Weight") {
    comments <- c(paste(title, "as", var, "in", unit))
  }
  else{
    stop("Not implemented")
  }
  
  comments <- c(main, comments, "")
  
  f <- file(filename, open = "w")
  write(paste("#", comments), f)
  write.table(
    output,
    file = f,
    sep = "\t",
    dec = ".",
    row.names = F
  )
  close(f)
}

#' @title Report RECA.
#' @description Produces reports for for RECA. Fails silently on errors.
#' @details Exports a tab separated file with means of catch at age (produced by \code{\link{saveCatchMatrix}}), one for the posterior distribution of catch at age (produced by \code{\link{saveCatchMatrix}}), and a file summarizing the model configuration (produced by \code{\link{writeRecaConfiguration}})
#'    This function is intended for use with \code{\link{getReports}}, and contain parameters like write in order to adhere to that contract.
#' @param projectName name of stox project
#' @param var Variable to extract. Allows for Abundance, Count or Weight
#' @param unit Unit for extracted variable. See \code{\link{getPlottingUnit}}
#' @param write logical determining if report is written to files. If false the function return immidiatly with NULL.
#' @param ... arguments passed to \code{\link{saveCatchMatrix}},\code{\link{saveCatchCovarianceMatrix}} and \code{\link{writeRecaConfiguration}}
#' @return list, with at least one named element 'filename', a vector of file-paths to generated plots.
#' @export
reportRECA <-
  function(projectName,
           var = "Abundance",
           unit = "millions",
           write = T,
           ...) {
    if (!write) {
      return(NULL)
    }
    
    out <- list()
    out$filename <- c()
    get_filename <- function(stat) {
      if (unit == "ones") {
        return(paste0(
          file.path(
            getProjectPaths(projectName)$RReportDir,
            paste0(c(stat, var), collapse = "_")
          ),
          ".txt"
        ))
      }
      else{
        return(paste0(
          file.path(
            getProjectPaths(projectName)$RReportDir,
            paste0(c(stat, var, unit), collapse = "_")
          ),
          ".txt"
        ))
      }
    }
    tryCatch({
      pd <- loadProjectData(projectName, var = "runRECA")
      
    },
    error = function(e) {
    },
    finally = {
      
    })

    tryCatch({
      saveCatchMatrix(
        pd$runRECA$pred,
        get_filename("means"),
        main = projectName,
        savemeans = T,
        var = var,
        unit = unit
      )
      out$filename <- c(get_filename("means"), out$filename)
    },
    error = function(e) {
    },
    finally = {
      
    })
    
    tryCatch({
      saveCatchMatrix(
        pd$runRECA$pred,
        get_filename("distribution"),
        main = projectName,
        savemeans = F,
        var = var,
        unit = unit
      )
      out$filename <- c(get_filename("distribution"), out$filename)
    },
    error = function(e) {
    },
    finally = {
      
    })
    
    tryCatch({
      saveCatchCovarianceMatrix(
        pd$runRECA$pred,
        get_filename("covariance"),
        main = projectName,
        standardize = F,
        var = var,
        unit = unit
      )
      out$filename <- c(get_filename("covariance"), out$filename)
    },
    error = function(e) {
    },
    finally = {
      
    })

    tryCatch({
      pd <- loadProjectData(projectName, var = "prepareRECA")
      filename <-
        file.path(getProjectPaths(projectName)$RReportDir,
                  "eca_configuration.txt")
      writeRecaConfiguration(
        pd$prepareRECA$GlobalParameters,
        pd$prepareRECA$Landings,
        pd$prepareRECA$WeightLength,
        pd$prepareRECA$AgeLength,
        fileobj = filename,
        main = projectName
      )
      out$filename <- c(filename, out$filename)
    },
    error = function(e) {
    },
    finally = {
      
    })
    return(out)
  }
