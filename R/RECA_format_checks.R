#
# Tests for data format to be exported to ECA. Organise all test on objects that is accepted by eca here. Tests on stox objects go elsewhere.
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
      stop(paste("Not all values present for fixed covariate", co, "(samples)"))
    }
    if (modelobj$info[co,"random"]==1 & modelobj$info[co,"in.landings"]==0 & modelobj$info[co,"continuous"]==0 & num_unique!=modelobj$info[co,"nlev"]){
      stop(paste("Nlev is incorrectly set for random effect not in landings: ", co, "(samples)"))
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
    if (modelobj$info[co,"CAR"]==1){
      asymmetric_pairs <- ""
      for (i in 1:modelobj$info[co,"nlev"]){
        last_i <- sum(modelobj$CARNeighbours$numNeighbours[1:i])
        num_i <- modelobj$CARNeighbours$numNeighbours[i]
        neighbours_i <- modelobj$CARNeighbours$idNeighbours[(last_i-num_i+1):last_i]
        for (j in 1:modelobj$info[co,"nlev"]){
          last_j <- sum(modelobj$CARNeighbours$numNeighbours[1:j])
          num_j <- modelobj$CARNeighbours$numNeighbours[j]
          neighbours_j <- modelobj$CARNeighbours$idNeighbours[(last_j-num_j+1):last_j]

          ineighbourofj <- i %in% neighbours_j
          jneighbourofi <- j %in% neighbours_i
            
          if (ineighbourofj!=jneighbourofi){
            asymmetric_pairs <- paste(asymmetric_pairs, " (",i,",", j, ") ", sep="")
          }
        }  
      }
      if (nchar(asymmetric_pairs)>0){
        stop(paste("CAR variable specified as", co, "but neighbour matrix is not symmetric (i is neighbour of j, but not j of i, or vice versa). Asymmetric pairs: ", asymmetric_pairs))
      }
    }
  }
}
#'
#' @keywords internal
check_data_matrix <- function(modelobj){
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
  check_columns_present(agelength$DataMatrix, c("age", "part.year", "lengthCM", "samplingID", "partnumber", "partcount"))
  check_none_missing(agelength$DataMatrix, c("lengthCM", "samplingID", "partnumber"))
  if (any(is.na(agelength$DataMatrix$partcount))){
    stop("Missing values for partcount (derived from lengthsamplecount, lengthsampleweight and cathcweight)")
  }
  check_data_matrix(agelength)
  check_covariates(agelength)
  if (!is.null(agelength$AgeErrorMatrix) & (any(is.na(agelength$AgeErrorMatrix)) || any(agelength$AgeErrorMatrix>1) || any(agelength$AgeErrorMatrix<0))){
    stop("Invalid values in age error matrix")
  }
  if (!is.null(agelength$AgeErrorMatrix) & any(abs(rowSums(agelength$AgeErrorMatrix)-1)>num_tolerance)){
    stop("Rows of age error matrix does not sum to 1")
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
  naerrors <- c()
  for (i in 1:ncol(cov)){
    if (any(is.na(cov[,1]))){
      naerrors <- c(naerrors, names(cov)[i])
    }
    if (length(naerrors)>0){
      stop(paste("NAs in landings: ", paste(naerrors, collapse=",")))
    }
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
checkGlobalParameters <- function(globalparameters, agelength, weightlength){
  if (is.na(globalparameters$lengthresCM)){
    stop("Length resolution not set (lengthresCM)")
  }
  if (max(agelength$DataMatrix$age, na.rm=T)>globalparameters$maxage){ #ages is checked for nas elsewere
    stop(paste("Parameter maxage", globalparameters$maxage, "is smaller than maximal age in samples (", max(agelength$DataMatrix$age, na.rm=T), ")"))
  }
  if (min(agelength$DataMatrix$age, na.rm=T)<globalparameters$minage){ #ages is checked for nas elsewere
    stop(paste("Parameter minage", globalparameters$minage, " is larger than minimal age in samples (", min(agelength$DataMatrix$age, na.rm=T), ")"))
  }
  if (max(weightlength$DataMatrix$lengthCM, na.rm=T)>globalparameters$maxlength){ #lengths are checked for nas elsewere
    stop(paste("Parameter maxlength (", globalparameters$maxlength, ") is smaller than maximal length in samples (", max(weightlength$DataMatrix$lengthCM, na.rm=T), ")"))
  }
  if (!globalparameters$age.error & !is.null(agelength$AgeErrorMatrix)){
    warning("Age error matrix set, but age.error parameter set to FALSE.")
  }
  if (globalparameters$age.error & is.null(agelength$AgeErrorMatrix)){
    stop("Age error matrix not set, but age.error parameter set to TRUE.")
  }
  if (globalparameters$age.error & nrow(agelength$AgeErrorMatrix) != (globalparameters$maxage-globalparameters$minage+1)){
    stop(paste0("Rows of age matrix does not match minage maxage parameters (", nrow(agelength$AgeErrorMatrix), " vs ", globalparameters$maxage, ":", globalparameters$minage, ")"))
  }
  if (globalparameters$age.error & as.numeric(row.names(agelength$AgeErrorMatrix))[1] != globalparameters$minage){
    stop("First age of age error matrix does not correspond to minage")
  }
  if (globalparameters$age.error & as.numeric(row.names(agelength$AgeErrorMatrix))[length(row.names(agelength$AgeErrorMatrix))] != globalparameters$maxage){
    stop("Last age of age error matrix does not correspond to maxage")
  }
  if (globalparameters$age.error & ncol(agelength$AgeErrorMatrix) != (globalparameters$maxage-globalparameters$minage+1)){
    stop(paste0("Columns of age matrix does not match minage maxage parameters (", ncol(agelength$AgeErrorMatrix), " vs ", globalparameters$maxage, ":", globalparameters$minage, ")"))
  }
  
}