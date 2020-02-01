#' Checks for data issues wrp to running RECA
#' 

#' @noRd
getCatchIssues <- function(catches, biotic, common_columns_catch){
  
  #get catches with several fractions sampled (delprove)
  delp <- aggregate(list(partnCount=catches$catchpartnumber), by=list(taxa=catches$catchcategory, serialnumber=catches$serialnumber, year=catches$year, mission=catches$cruise), FUN=function(x){length(unique(x))})
  if (nrow(delp)>0){
    delp <- delp[delp$partnCount>1,]  
  }
  
  severalDelp <- rep(F, nrow(catches))
  if (nrow(delp)>0){
    severalDelp <- catches$cruise %in% delp$mission & catches$year %in% delp$year & catches$catchcategory %in% delp$taxa & catches$serialnumber %in% delp$serialnumber
  }
  
  catchissues <- biotic[0,common_columns_catch]
  missingcatchweight <- catches[severalDelp & (is.na(catches$catchweight) | catches$catchweight==0),common_columns_catch]
  if (nrow(missingcatchweight)){
    missingcatchweight$issue <- "missing or 0 catch weight, with several catch fractions (delpr\u00F8ve)"
    catchissues <- rbind(catchissues, missingcatchweight)
  }
  missingsampleweight <- catches[severalDelp & (is.na(catches$lengthsampleweight) | catches$lengthsampleweight==0),common_columns_catch]
  if (nrow(missingsampleweight)){
    missingsampleweight$issue <- "missing or 0 sample weight, with several catch fractions (delpr\u00F8ve)"  
    catchissues <- rbind(catchissues, missingsampleweight)
  }
  missingsamplecount <- catches[severalDelp & (is.na(catches$lengthsamplecount) | catches$lengthsamplecount==0),common_columns_catch]
  if (nrow(missingsamplecount)){
    missingsamplecount$issue <- "missing or 0 sample count, with several catch fractions (delpr\u00F8ve)"  
    catchsissues <- rbind(catchsissues, missingsamplecount)
  }
  
  return(catchissues)
}

#' Data report for RECA
#' @description Generates reports on data issues that might need to be addressed before running RECA
#' @details Checks data exported from stox for missing mandatory information, and issues that will invalidate common covariate-configurations
#'    issues with stations are written as a tab delimited file to stationissuesfile
#'    issues with catch samples are written as a tab delimited fil to catchissuefile
#'    issues that can be solved with imputation or parameter estimation tools provided by stox are written to imputationfile
#' @param biotic sample data exported from stox in \code{\link[Rstox]{prepareRECA}}
#' @param stationussuesfile file for writing station issues
#' @param catchissuefile file for writing catch sample issues
#' @param imputationfile file for writing report on issues that may need data massaging (imputation or estimation)
#' @param verbose If true, summary and file locations will be printed to stdout
#' @param covariates vector with names of covariates (identifies columns)
#' @param landing landing data
#' @keywords internal
makeDataReportReca <- function(biotic, stationissuesfile, catchissuefile, imputationfile, verbose=T, covariates=NULL, landing=NULL){
  

  #
  # station issues
  #
  
  stations <- biotic[!duplicated(biotic[c("cruise", "serialnumber")]),]
  common_columns_station <- c("cruise", "serialnumber", "stationstartdate", "latitudestart", "longitudestart", "area", "location", "gear")
  
  stationissues <- biotic[0,common_columns_station]
  
  if (is.null(covariates) || "temporal" %in% covariates){
    # missing time
    missingstartdate <- stations[is.na(stations$stationstartdate),common_columns_station]
    if (nrow(missingstartdate)>0){
      missingstartdate$issue <- "missing startdate (may be OK if stopdate is set.)"
      stationissues <- rbind(stationissues, missingstartdate)
    }
    
    #missing temporal covariate not yet accounted for
    if (!is.null(stations$temporal)){
      missingtemporal <- stations[is.na(stations$temporal) & !(stations$serialnumber %in% missingstartdate$serialnumber),common_columns_station]
      if (nrow(missingtemporal)>0){
        missingtemporal$issue <- "missing temporal covariate (has station startdate or stopdate)"
        stationissues <- rbind(stationissues, missingtemporal)
      } 
    }
  }
  
  if (is.null(covariates) || "spatial" %in% covariates){
    #missing position or area code
    missingposition <- stations[(is.na(stations$latitudestart) | is.na(stations$latitudeend)) & (is.na(stations$location) | is.na(stations$area)),common_columns_station]
    if (nrow(missingposition)>0){
      missingposition$issue <- "missing start positions or area codes"
      stationissues <- rbind(stationissues, missingposition)
    }
    
    #missing spatial covariate not yet accounted for
    if (!is.null(stations$spatial)){
      missingposition <- stations[is.na(stations$spatial) & !(stations$serialnumber %in% missingposition$serialnumber),common_columns_station]
      if (nrow(missingposition)>0){
        missingposition$issue <- "missing spatial covariate (has position or area code on station)"
        stationissues <- rbind(stationissues, missingposition)
      }
    }
  }
  
  if (is.null(covariates) || "gearfactor" %in% covariates){
    #missing gear
    missinggear <- stations[is.na(stations$gear),common_columns_station]
    if (nrow(missinggear)>0){
      missinggear$issue <- "missing gear code"
      stationissues <- rbind(stationissues, missinggear)
    }
    
    #missing gear covariate not yet accounted for
    if (!is.null(stations$gearfactor)){
      missinggear <- stations[is.na(stations$gearfactor) & !(stations$serialnumber %in% missinggear$serialnumber),common_columns_station]
      if (nrow(missinggear)>0){
        missinggear$issue <- "missing gear covariate (has gear code on station)"
        stationissues <- rbind(stationissues, missinggear)
      }
    }
  }
  
  # sampling frame issues
  if (length(covariates) > 1 & !is.null(landing)){
    inlandings <- covariates[covariates %in% names(landing)]
    if (length(inlandings)>1){
      cellsSamples <- apply( stations[,inlandings] , 1 , paste , collapse = "/" )
      cellsLandings <- unique(apply( landing[,inlandings] , 1 , paste , collapse = "/" ))
      missing <- !(cellsSamples %in% cellsLandings)
    }
    else{
      cellsSamples <- stations[[inlandings]]
      cellsLandings <- unique(landing[[inlandings]])
      missing <- !(cellsSamples %in% cellsLandings)
    }
    missingLandings <- stations[missing,common_columns_station]
    if (nrow(missingLandings)>0){
      missingLandings$issue <- "cell is missing from landings."  
      stationissues <- rbind(stationissues, missingLandings)
    }
    
  }
  
  
  if (nrow(stationissues)>0){
    f <- file(stationissuesfile, open = "w")
    write.table(
      stationissues,
      file = f,
      sep = "\t",
      dec = ".",
      row.names = F
    )
    close(f)
    
    if (verbose){
      write(paste("Some stations have issues that might prevent common covariateconfigurations. See report in:", stationissuesfile), file = "")
    }
  }
  
  
  #
  # catch sample issues
  #
  
  catches <- biotic[!duplicated(biotic[c("cruise", "serialnumber", "catchcategory", "catchpartnumber")]),]
  common_columns_catch <- c( "cruise", "serialnumber", "catchcategory", "catchpartnumber", "catchweight", "lengthsampleweight", "lengthsamplecount")
  
  catchissues <- getCatchIssues(catches, biotic, common_columns_catch)
  
  if (nrow(catchissues)>0){
    f <- file(catchissuefile, open = "w")
    write.table(
      catchissues,
      file = f,
      sep = "\t",
      dec = ".",
      row.names = F
    )
    close(f)
    
    if (verbose){
      write(paste("Some catch samples have issues that might prevent estimation. See report in:", catchissuefile), file = "")
    }
  }
  
  
  #
  # Imputation or estimation issues
  #
  
  soft_problems <- c()
  if (any(is.na(stations$platform))){
    soft_problems <- c(soft_problems, "Some stations are missing platforms. Consider imputing from mission level.")
  }
  if (!is.null(stations$spatial)){
    if (any(is.na(stations$spatial) & ((is.na(stations$latitudestart) | is.na(stations$latitudeend)) & !is.na(stations$location) & !is.na(stations$area)))){
      soft_problems <- c(soft_problems, "Some stations are missing positions. Consider imputing from area codes.")
    }
  }
  # add check on product type to signal use of length weight conversion
  
  if (length(soft_problems)>0){
    f <- file(imputationfile, open = "w")
    for (i in soft_problems){
      write(i, f)
    }
    close(f)
    
    if (verbose){
      write(paste("The data has some issues that might require imputation or parameter estimation. See report in:", imputationfile), file = "")
    }
    
  }
  
}


#' Sample Homogeneity
#' @description 
#'  Checks selected parameters for homogeneity assumed by Reca.
#'  This is implemented to compensate variables that are not available for checking in the data exported by BioticCovData and LandingCovData.
#' @details 
#'  runs a StoX-project up to the process specified by 'processName', which is assumed to putput biotic data.
#'  Writes an issue list if:
#'  \itemize{
#'   \item{There are more than one species used for the variable 'catchcategory'}
#'   \item{There are more than one value (including NA) used for the variable 'lengthmeasurement'}
#'   \item{Catch is fractioned (several samples for species at station) and product types are missing or differ between catch and sample weight ('catchproducttype' and 'sampleproducttype').}
#'  }
#' @param projectName name of stox project
#' @param processName process whose output is to be checked
#' @param issuefile filename for writing issues, if NULL, write to stderr()
#' @export
makeSampleHomogeneityReportRECA <- function(projectName, processName="FilterBiotic", issuefile=NULL){
  bl <- Rstox::getBaseline(projectName, endProcess = processName)
  
  if (is.null(bl$outputData[[processName]])){
    stop(paste("No output found for process:", processName))
  }
  if (is.null( bl$outputData[[processName]][[paste(processName,"_BioticData_catchsample.txt", sep="")]])){
    stop(paste("No catchsample output found for process:", processName))
  }
  
  #
  # Find issues
  #
  issues <- c()
  
  catches <- bl$outputData[[processName]][[paste(processName,"_BioticData_catchsample.txt", sep="")]]
  
  #species
  species <- unique(catches$catchcategory)
  if (length(species)!=1){
    issues <- c(issues, c(paste("Several codes for species ('catchcategory') found in data:", paste(species, collapse = ", "))))
  }
  
  #lengthmeasurment
  lengthmeasurements <- unique(catches$lengthmeasurement)
  if (length(lengthmeasurements)!=1){
    issues <- c(issues, c(paste("Several codes for length measurements ('lengthmeasurement') found in data:", paste(lengthmeasurements, collapse = ", "))))
  }

  #sampleproducttype
  sampleproducttypes <- unique(catches$sampleproducttype)
  if (length(sampleproducttypes)!=1){
    issues <- c(issues, c(paste("Several codes for sample product types ('sampleproducttypes') found in data:", paste(sampleproducttypes, collapse = ", "), "consider correcting weights with ConvertLengthAndWeight.")))
  }
  
    
  #producttypes when delprove taken
  stationids <- do.call(paste, catches[,c("platform", "startyear", "missiontype", "missionnumber", "serialnumber")])
  duplicates <- duplicated(stationids)
  if (any(duplicates)){
    dupcatches <- catches[stationids %in% stationids[duplicates],]
    producttypedifference <- dupcatches[!is.na(dupcatches$catchproducttype) & !is.na(dupcatches$sampleproducttype) & dupcatches$catchproducttype != dupcatches$sampleproducttype,]
    if (nrow(producttypedifference) != 0){
      for (i in 1:nrow(producttypedifference)){
        issues <- c(issues, paste("Product types differ for fractioned catch. Catch weight (catchproducttype):", producttypedifference[i, "catchproducttype"],
                                  "sample weight (sampleproducttype):", producttypedifference[i, "sampleproducttype"],
                                  "platform:", producttypedifference[i, "platform"], 
                                  "startyear:", producttypedifference[i, "startyear"],
                                  "missiontype", producttypedifference[i, "missiontype"],
                                  "missionnumber", producttypedifference[i, "missionnumber"],
                                  "serialnumber", producttypedifference[i, "serialnumber"]))
      }
    }
    producttypemissing <- dupcatches[is.na(dupcatches$catchproducttype) | is.na(dupcatches$sampleproducttype),]
    if (nrow(producttypemissing) != 0){
      for (i in 1:nrow(producttypemissing)){
        issues <- c(issues, paste("Product types missing for fractioned catch. Catch weight (catchproducttype):", producttypemissing[i, "catchproducttype"],
                                  "sample weight (sampleproducttype):", producttypemissing[i, "sampleproducttype"],
                                  "platform:", producttypemissing[i, "platform"], 
                                  "startyear:", producttypemissing[i, "startyear"],
                                  "missiontype", producttypemissing[i, "missiontype"],
                                  "missionnumber", producttypemissing[i, "missionnumber"],
                                  "serialnumber", producttypemissing[i, "serialnumber"]))
      }
    }
  }
  
  
  #
  # write issues
  #
  
  if (is.null(issuefile)){
    stream <- stderr()
  }
  else{
    stream <- file(issuefile, open="w")
  }
  
  if (length(issues > 0)){
    write(paste("Potential issues with samples in project", projectName, ":", sep=""), stream)
    for (i in issues){
      write(i, stream)  
    }
  }
  else{
    write("No issues found", stdout())
  }
  
  if (!is.null(issuefile)){
    close(stream)
  }

}