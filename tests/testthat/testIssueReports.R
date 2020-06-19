landings <- readRDS(system.file("extdata", "testresources","HAD_landings_2018_sampled10K.rds", package="Rstox"))

loadReport <- function(filename){
  f <- file(filename, open="r")
  lines <- readLines(f)
  close(f)
  return(lines)
}


context("test landings issue report")
tempf <- tempfile()
writeLandingsIssues(tempf, landings, c("temporal", "gearfactor", "spatial"), F)
expect_false(file.exists(tempf))
if (file.exists(tempf)){
  file.remove(tempf)  
}

tempf <- tempfile()
land <- landings
land$sistefangstdato[1] <- NA
expect_message(writeLandingsIssues(tempf, land, c("temporal", "gearfactor", "spatial"), T), "")
expect_true(file.exists(tempf))
expect_true("Some landings are missing date of catch ('sistefangstdato')" %in% loadReport(tempf))
if (file.exists(tempf)){
  file.remove(tempf)  
}


context("test landings issue report missing gear")
tempf <- tempfile()
land <- landings
land$gearfactor[1] <- NA
land$gearfactor[100] <- NA
expect_silent(writeLandingsIssues(tempf, land, c("temporal", "gearfactor", "spatial"), F))
expect_true(file.exists(tempf))
expect_true("Some landings are not assigned covariate 'gearfactor' (redskapskode:  22,32 )" %in% loadReport(tempf))
if (file.exists(tempf)){
  file.remove(tempf)  
}

tempf <- tempfile()
land <- landings
land$spatial[1] <- NA
expect_message(writeLandingsIssues(tempf, land, c("temporal", "gearfactor", "spatial"), T), "")
expect_true(file.exists(tempf))
expect_true("Some landings are not assigned covariate \'spatial\'" %in% loadReport(tempf))
if (file.exists(tempf)){
  file.remove(tempf)  
}