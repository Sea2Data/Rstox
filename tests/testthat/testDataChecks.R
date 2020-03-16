prep <- readRDS(system.file("extdata", "testresources", "prepRecaWOstoxExport.rds", package = "Rstox"))

#GlobalParameters$caa.burnin <- caa.burnin
#GlobalParameters$burnin <- burnin
#GlobalParameters$nSamples <- nSamples
#GlobalParameters$thin <- thin
#GlobalParameters$fitfile <- fitfile
#GlobalParameters$predictfile <- predfile
#GlobalParameters$lgamodel <- lgamodel
#prep$GlobalParameters$CC <- F
#GlobalParameters$CCerror <- CCError
#prep$GlobalParameters$age.error <- T
#GlobalParameters$seed <- seed

context("data checks otolithtype")
prep$GlobalParameters$CC <- F
prep$GlobalParameters$age.error <- T
checkGlobalParameters(prep$GlobalParameters, prep$AgeLength, prep$WeightLength)

prep$GlobalParameters$CC <- T
expect_error(checkGlobalParameters(prep$GlobalParameters, prep$AgeLength, prep$WeightLength), "CC is set, but otolithtypes are not provided")
prep$AgeLength$DataMatrix[1:5, "otolithtype"] <- 2
expect_error(checkGlobalParameters(prep$GlobalParameters, prep$AgeLength, prep$WeightLength), "CC is set, but all records have the same otolithtype")
prep$AgeLength$DataMatrix[6:10, "otolithtype"] <- 1
checkGlobalParameters(prep$GlobalParameters, prep$AgeLength, prep$WeightLength)