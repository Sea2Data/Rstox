stations <- readRDS(system.file("extdata", "testresources", "filterBioticHadTest.rds", package = "Rstox"))
processdata <- readRDS(system.file("extdata", "testresources", "processDataHadTest.rds", package = "Rstox"))
biotic <- readRDS(system.file("extdata", "testresources", "BioticCovDataHadExample.rds", package = "Rstox"))

nonNAs <- biotic[!is.na(biotic$Temporal),]
NAs <- biotic[is.na(biotic$Temporal),]
expect_false(any(nonNAs$serialnumber %in% NAs$serialnumber))
snrcountByQuarter <- aggregate(list(serialnumber=nonNAs$serialnumber), by=list(temporal=nonNAs$Temporal), FUN=function(x){length(unique(x))})
expect_equal(sum(snrcountByQuarter$serialnumber), length(unique(nonNAs$serialnumber)))

context("Test temporal workaround")
names(biotic) <- tolower(names(biotic))
suppressMessages(biotic <- temporal_workaround(biotic, processdata, "Biotic", stations))
snrcountByQuarter <- aggregate(list(serialnumber=biotic$serialnumber), by=list(temporal=biotic$temporal), FUN=function(x){length(unique(x))})
expect_equal(sum(snrcountByQuarter$serialnumber), length(unique(biotic$serialnumber)))