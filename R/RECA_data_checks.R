#' Checks for data issues wrp to running RECA
#' 

#' Checks for NAs in columns commonly used for setting covariates
#' @keywords internal
check_common_covariates_for_NAs <- function(stoxexport){
  # startdate
  # position or area code
  # gear
  
}

#' Checks for NAs in catch parameters needed
#' @keywords internal
check_catch_variables_for_NAs <- function(stoxexport){
  # catchweight
  # sample weight
  # sample number
}

#' @keywords internal
#' Checks for indicators that stox-provided imputation tools or parameter estimation tools are needed
check_if_data_massaging_needed <- function(stoxexport){
  # area but not position
  # mixed product types
  # mixed length measurements
}

#' Data report for RECA
#' @description Generates reports on data issues that might need to be addressed before running RECA
#' @details Checks data exported from stox for missing mandatory information, and issues that will invalidate common covariate-configurations
#'    issues with stations are written as a tab delimited fil to stationissuesfile
#'    issues with catch samples are written as a tab delimited fil to catchissuefile
#'    issues that can be solved with imputation or parameter estimation tools provided by stox are written to imputationfile
#' @param stoxexport sample data exported from stox in \code{\link[Rstox]{prepareRECA}}
#' @param stationussuesfile file for writing station issues
#' @param catchissuefile file for writing catch sample issues
#' @param imputationfile file for writing report on issues that may need data massaging (imputation or estimation)
#' @param verbose If true, summary and file locations will be printed to stdout
makeDataReportReca <- function(stoxexport, stationissuesfile, catchissuefile, imputationfile, verbose=T){
  
  #
  # station issues
  #
  
  common_columns_station <- c("cruise", "serialnumber", "startdate", "latitudestart", "longitudestart", "area", "location", "gear")
  
  stationissues <- stoxexport$biotic[0,common_columns_station]
  
  # missing time
  missingstartdate <- stoxexport$biotic[is.na(stoxexport$biotic$startdate),common_columns_station]
  if (nrow(missingstartdate)>0){
    missingstartdate$issue <- "missing startdate"
    stationissues <- rbind(stationissues, missingstartdate)
  }
  
  #missing position or area code
  missingposition <- stoxexport$biotic[(is.na(stoxexport$biotic$latitudestart) | is.na(stoxexport$biotic$latitudeend)) & (is.na(stoxexport$biotic$location) | is.na(stoxexport$biotic$area)),common_columns_station]
  if (nrow(missingposition)>0){
    missingposition$issue <- "missing start positions or area codes"
    stationissues <- rbind(stationissues, missingposition)
  }
  
  #missing gear
  missinggear <- stoxexport$biotic[is.na(stoxexport$biotic$gear),common_columns_station]
  if (nrow(missinggear)>0){
    missinggear$issue <- "missing gear code"
    stationissues <- rbind(stationissues, missinggear)
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
  
  common_columns_catch <- c( "cruise", "serialnumber", "species", "samplenumber", "catchweight", "lengthsampleweight", "lengthsamplecount")
  
  catchissues <- stoxexport$biotic[0,common_columns_catch]
  missingcatchweight <- stoxexport$biotic[is.na(stoxexport$biotic$catchweight) | stoxexport$biotic$catchweight==0,common_columns_catch]
  if (nrow(missingcatchweight)){
    missingcatchweight$issue <- "missing or 0 catch hweight"
    catchissues <- rbind(catchissues, missingcatchweight)
  }
  missingsampleweight <- stoxexport$biotic[is.na(stoxexport$biotic$lengthsampleweight) | stoxexport$biotic$lengthsampleweight==0,common_columns_catch]
  if (nrow(missingsampleweight)){
    missingsampleweight$issue <- "missing or 0 sample weight"  
    catchissues <- rbind(catchissues, missingsampleweight)
  }
  missingsamplecount <- stoxexport$biotic[is.na(stoxexport$biotic$lengthsamplecount) | stoxexport$biotic$lengthsamplecount==0,common_columns_catch]
  if (nrow(missingsamplecount)){
    missingsamplecount$issue <- "missing or 0 sample count"  
    catchsissues <- rbind(catchsissues, missingsamplecount)
  }
   
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
  if (any(is.na(stoxexport$biotic$platform))){
    soft_problems <- c(soft_problems, "Some stations are missing platforms. Consider imputing from mission level.")
  }
  if (any((is.na(stoxexport$biotic$latitudestart) | is.na(stoxexport$biotic$latitudeend)) & !is.na(stoxexport$biotic$location) & !is.na(stoxexport$biotic$area))){
    soft_problems <- c(soft_problems, "Some stations are missing positions. Consider imputing from area codes.")
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