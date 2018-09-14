#
# Tests for data format to be exported to ECA
#

#' checks that column names are present on datamatrix
#' @keywords internal
check_columns_present <- function(datamatrix, columns){
  errors <- ""
  for (col in columns){
    if (!(col %in% attributes(datamatrix)$names)){
      errors <- paste(errors, "column", col, "missing.\n")
    }
  }
  if (errors != ""){
    stop(errors)
  }
}
#' columns does not have missing values
#' @keywords internal
check_none_missing <- function(datamatrix, columns){
  errors <- ""
  for (col in columns){
    if (any(is.na(datamatrix[,col]))){
      errors <- paste(errors, "column", col, "has missing value.\n")
    }
  }
  if (errors != ""){
    stop(errors)
  }
}
#'
#' @keywords internal
check_cov_vs_info <- function(modelobj){
  if (!("constant" %in% names(modelobj$CovariateMatrix)) | !("constant" %in% rownames(modelobj$info))){
    stop("No constant column provided in covariate matrix or info matrix")
  }
  if (modelobj$info["constant","in.landings"]!=1 | modelobj$info["constant","in.slopeModel"]!=1 | modelobj$info["constant","random"]!=0 | modelobj$info["constant","CAR"]!=0 | modelobj$info["constant","continuous"]!=0){
    stop("Constant covariate is not configured correctly")
  }
  for (co in names(modelobj$CovariateMatrix)){
    if (!co %in% rownames(modelobj$info)){
      stop(paste("Covariate", co, "not in info matrix"))
    }
    if (any(is.na(modelobj$CovariateMatrix[,co]))){
      stop(paste("NAs for covariate", co))
    }
    ma <- max(modelobj$CovariateMatrix[,co])
    mi <- min(modelobj$CovariateMatrix[,co])
    num_unique <- length(unique(modelobj$CovariateMatrix[,co]))
    
    if (modelobj$info[co,"continuous"]==0 & ma > modelobj$info[co,"nlev"]){
      stop(paste("Max value higher than nlev for covariate", co))
    }
    if (modelobj$info[co,"continuous"]==0 & mi < 1){
      stop(paste("Min value lower than 1 for covariate", co))
    }
    if (modelobj$info[co,"CAR"]==1 & modelobj$info[co,"random"]!=1){
      stop("CAR variable not designated as random effect.")
    }
    if (modelobj$info[co,"CAR"]==1 & modelobj$info[co,"random"]!=1){
      stop("CAR variable not designated as random effect.")
    }
    if (modelobj$info[co,"continuous"]==1 & modelobj$info[co,"nlev"]!=1){
      stop(paste("nlev wrongly configured for continuous variable", co))
    }
    if (modelobj$info[co,"interaction"]==1 & modelobj$info[co,"in.landings"]!=1){
      stop(paste("Interaction specified for covariate that are not in landings", co))
    }
    if (modelobj$info[co,"random"]==0 & modelobj$info[co,"continuous"]==0 & num_unique!=modelobj$info[co,"nlev"]){
      stop(paste("Not all values present for fixed covariate", co))
    }
    if (modelobj$info[co,"CAR"]==1 & is.null(modelobj$CARNeighbours)){
      stop(paste("CAR variable specified as", co, "but CARneighbours not specified"))
    }
    if (modelobj$info[co,"CAR"]==1 & (max(modelobj$CARNeighbours$idNeighbours)>modelobj$info[co,"nlev"] | max(modelobj$CARNeighbours$idNeighbours)<1)){
      stop(paste("Neigbour matrix not consistent with nlev for CAR vairable", co))
    }
    if (modelobj$info[co,"CAR"]==1 & (any(modelobj$CARNeighbours$numNeighbours<1) | length(modelobj$CARNeighbours$numNeighbours) < modelobj$info[co,"nlev"])){
      stop(paste("CAR variable specified as", co, "but some areas are missing neighbours in the data."))
    }
    if (modelobj$info[co,"CAR"]==1 & sum(modelobj$CARNeighbours$numNeighbours) != length(modelobj$CARNeighbours$idNeighbours)){
      stop(paste("CAR variable specified as", co, "numNeigbours is not consistent with idNeigbours"))
    }
  }
}
#'
#' @keywords internal
check_data_matrix <- function(modelobj){
  #if ("otolithtype" %in% names(modelobj$DataMatrix)){
  #  check_none_missing(modelobj$DataMatrix, c("otolithtype"))
  #}
  
  warning("Check otolith requirements after R-ECA is revised")
  lastsample <- max(modelobj$DataMatrix$samplingID)
  if (!lastsample==nrow(modelobj$CovariateMatrix)){
    stop("sampling ids does not equal the number of rows in covariate matrix")
  }
  
}
#' checks that specification of covariates are OK
#' @keywords internal
check_covariates <- function(modelobject){
  check_cov_vs_info(modelobject)
}

#' checks that agelenght is configured correctly
#' @keywords internal
checkAgeLength<-function(agelength, num_tolerance = 1e-10){
  check_columns_present(agelength$DataMatrix, c("age", "realage", "part.year", "lengthCM", "samplingID", "partnumber", "partcount"))
  check_none_missing(agelength$DataMatrix, c("lengthCM", "samplingID", "partnumber"))
  if (any(is.na(agelength$DataMatrix$partcount))){
    stop("Missing values for partcount (derived from lengthsamplecount, lengthsampleweight and cathcweight)")
  }
  check_data_matrix(agelength)
  check_covariates(agelength)
  if (any(is.na(agelength$AgeErrorMatrix)) || any(agelength$AgeErrorMatrix>1) || any(agelength$AgeErrorMatrix<0)){
    stop("Invalid values in age error matrix")
  }
  if (any(abs(colSums(agelength$AgeErrorMatrix)-1)>num_tolerance)){
    stop("Columns of age error matrix does not sum to 1")
  }
}
#' checks that weightlenght is configured correctly
#' @keywords internal
checkWeightLength<-function(weightlength, landings){
  check_columns_present(weightlength$DataMatrix, c("weightKG", "lengthCM", "samplingID", "partnumber", "partcount"))
  check_none_missing(weightlength$DataMatrix, c("lengthCM", "samplingID", "partnumber", "weightKG"))
  if (any(is.na(weightlength$DataMatrix$partcount))){
    stop("Missing values for partcount (derived from lengthsamplecount, lengthsampleweight and catchweight)")
  }
  check_data_matrix(weightlength)
  check_covariates(weightlength)
}

#' checks that covariates are compatible between model and landings
#' @keywords internal
checkCovariateConsistency <- function(modelobj, landingscov){
  
  inlandings <- rownames(modelobj$info[modelobj$info[,"in.landings"]==1,])
  if (any(!(inlandings %in% names(landingscov)))){
    stop("some covariates labeled as in.landings are not found in corresponding covariate matrix in landings")
  }
  
  landingscoovariates <- names(landingscov)[names(landingscov) %in% inlandings]
  if (!all(inlandings==landingscoovariates)){
    stop("Covariates are not ordered consistently in model and landings")
  }
  
  #check that all level are present for all fixed effects
  nonconfixedeffects <- rownames(modelobj$info[modelobj$info[,"random"]==0 & modelobj$info[,"continuous"]==0,])
  for (co in nonconfixedeffects){
      num_unique <- length(unique(landingscov[,co]))
      if (num_unique!=modelobj$info[co,"nlev"]){
        stop(paste("Fixed effect", co, "does not have values for all corresponding landings"))
      }
  }
  
  #check that the number of combinations of fixed effects in samples equal those in landing
  samp <- unique(modelobj$CovariateMatrix[,nonconfixedeffects])
  land <- unique(landingscov[,nonconfixedeffects])
  
  sample_combos <- nrow(samp)
  land_combos <- nrow(land)
  if (is.null(sample_combos) & is.null(land_combos) & length(sample_combos)!=length(land_combos)){
    stop("Not all combinations of fixed effects are sampled")
  }
  else if (sample_combos != land_combos){
    stop("Not all combinations of fixed effects are sampled")
  }
}

#' checks formatting on landing cov-matrices
#' @keywords internal
check_landings_cov <- function(cov){
  if (!all(cov$midseason>0 & cov$midseason<=1)){
    stop("midseason must be in <0,1]")
  }
  if (any(is.na(cov))){
    stop("NAs in landings")
  }
}

#' checks that landings are specified correctly
#' @keywords internal
checkLandings <- function(landings){
  if (nrow(landings$AgeLengthCov) != nrow(landings$WeightLengthCov)){
    stop("number of rows landings covariate matrices does not match")
  }
  if (nrow(landings$AgeLengthCov) != length(landings$LiveWeightKG)){
    stop("length of weight vector does not match number of rows in covariate matrices in landings.")
  }
  check_landings_cov(landings$AgeLengthCov)
  check_landings_cov(landings$WeightLengthCov)
}

#' 
#' @keywords internal
checkGlobalParameters <- function(globalparameters){
  if (is.na(globalparameters$lengthresCM)){
    stop("Length resolution not set (lengthresCM)")
  }
}