context("readErsFile: normal run")
data <- readErsFile(system.file("extdata", "testresources","logbooks_trimmed_2015.psv", package="Rstox"))
expect_true(data.table::is.data.table(data))
expect_true("RC" %in% names(data))
expect_true(is.numeric(data$RUNDVEKT))
expect_true(is.numeric(data[["ST\u00D8RSTE_LENGDE"]]))
expect_equal(nrow(data),9)

context("calculateProportions")
logbooks <- readErsFile(system.file("extdata", "testresources","logbooks_trimmed_2015.psv", package="Rstox"))
expect_error(calculateCatchProportions(logbooks, totalcell=c("dummy")), "Not all columns defining totalcell is in logbooks")
ll <- calculateCatchProportions(logbooks)
expect_false(all(ll$fraction == 1))
agg <- aggregate(list(total=ll$fraction), by=list(REGM=ll$REGM, FANFSTART=ll$FANGSTART), FUN=sum)
expect_true(all(ll$total == 1))
logbooks$HOMR <- substr(logbooks$LOKASJON_START,1,2)
logbooks$Q <- quarters(logbooks$STARTTIDSPUNKT)
ll <- calculateCatchProportions(logbooks, subcell = c("HOMR", "Q"))
expect_true(all(c("HOMR", "Q") %in% names(ll)))
agg <- aggregate(list(total=ll$fraction), by=list(REGM=ll$REGM, FANFSTART=ll$FANGSTART), FUN=sum)
expect_true(all(ll$total == 1))

context("calculateProportions, check cells")
logbook <- readRDS(system.file("extdata", "testresources","HAD_logbook_2018.rds", package="Rstox"))
logbook <- logbook[logbook$AKTIVITET=="I fiske",]
logbook <- logbook[!is.na(logbook$REDSKAP_NS),]
logbook$MONTH <- substr(logbook$STARTTIDSPUNKT,6,7)
proportions <- calculateCatchProportions(logbook, totalcell= c("FANGSTART_FAO"), subcell = c("REDSKAP_NS", "MONTH"), weight = "RUNDVEKT")
proportions[1,"fraction"]
expect_equal(proportions[1,"fraction"], sum(logbook[logbook$REDSKAP_NS==proportions$REDSKAP_NS[1] & logbook$MONTH == proportions$MONTH[1], "RUNDVEKT"]) / sum(logbook$RUNDVEKT))
expect_equal(proportions[10,"fraction"], sum(logbook[logbook$REDSKAP_NS==proportions$REDSKAP_NS[10] & logbook$MONTH == proportions$MONTH[10], "RUNDVEKT"]) / sum(logbook$RUNDVEKT))

context("test lst parser")
logbooks <- readLstFile(system.file("extdata", "testresources","dagbok_2019_trunc.lst", package="Rstox"))
expect_true(all(c("FISK", "FAAR") %in% names(logbooks)))
expect_true(all(!is.na(logbooks$VEKT)))
expect_true(is.numeric(logbooks$VEKT))

expect_error(calculateCatchProportions(logbooks, totalcell = c("FISK", "REGM"), subcell = c("HO", "FM")), "Column RUNDVEKT not in logbooks")

logbooks <- logbooks[!is.na(logbooks$FISK)]
ll <- calculateCatchProportions(logbooks, totalcell = c("FISK", "REGM"), subcell = c("HO", "FM"), weight = "VEKT")
expect_true(all(c("HO", "FM") %in% names(ll)))
agg <- aggregate(list(total=ll$fraction), by=list(REGM=ll$REGM, FANFSTART=ll$FISK), FUN=sum)
expect_true(all(ll$total == 1))


context("test NAs")
logbooks <- readErsFile(system.file("extdata", "testresources","logbooks_trimmed_2015.psv", package="Rstox"))

#NA not covering complete cell
logbooks$RUNDVEKT[2] <- NA
expect_error(calculateCatchProportions(logbooks), "got NA fractions for some partitions.")
calculateCatchProportions(logbooks, na.rm = T)


#
#
# Test logbook cleaning
#
#



#' For testing adjustLandings based on lst files
testAdjustLandingsWithLst <- function(landingsStox, logbooksLst, gearTable, gearCode, minVesselSize=15){
  gearvector <- unlist(strsplit(gearTable$Value[gearTable$CovariateSourceType=="Landing" & gearTable$Covariate==gearCode], ","))
  originalColumns <- names(landingsStox)
  
  #
  # annotate with compareable codes, gear
  # and reduce logbooks to desired gear
  #
  landingsStox$gearCategory <- NA
  landingsStox$gearCategory[as.character(landingsStox$redskapkode) %in% gearvector] <- gearCode
  logbooksLst$gearCategory <- NA
  logbooksLst$gearCategory[as.character(logbooksLst$RE) %in% gearvector] <- gearCode
  logbooksLst <- logbooksLst[!is.na(logbooksLst$gearCategory)]
  
  #
  # annotate with compareable codes, species
  # and reduce lobooks to species that are in landings
  #
  landingsStox$speciesCategory <- substring(as.character(landingsStox$artkode), 1,4)
  logbooksLst$speciesCategory <- substring(as.character(logbooksLst$FISK), 1,4)
  logbooksLst <- logbooksLst[logbooksLst$speciesCategory %in% landingsStox$speciesCategory,]
  
  #
  # annotate with compareable codes, vessel size
  # and reduce logbooks to desired vessel size
  #
  landingsStox$vesselSizeCategory <- NA
  landingsStox$vesselSizeCategory[!is.na(landingsStox[["st\u00F8rstelengde"]]) & landingsStox[["st\u00F8rstelengde"]]>=minVesselSize] <- "o15"
  logbooksLst$vesselSizeCategory <- NA
  logbooksLst$vesselSizeCategory[logbooksLst$LENG>=minVesselSize] <- "o15"
  logbooksLst <- logbooksLst[!is.na(logbooksLst$vesselSizeCategory),]
  
  
  #
  # annotate with compareable codes, quarter
  #
  landingsStox$month <- substr(landingsStox$sistefangstdato, 6,7)
  landingsStox$quarterCategory <- NA
  landingsStox$quarterCategory[landingsStox$month %in% c("01","02","03")] <- "Q1"
  landingsStox$quarterCategory[landingsStox$month %in% c("04","05","06")] <- "Q2"
  landingsStox$quarterCategory[landingsStox$month %in% c("07","08","09")] <- "Q3"
  landingsStox$quarterCategory[landingsStox$month %in% c("10","11","12")] <- "Q4"
  
  logbooksLst$quarterCategory <- NA
  logbooksLst$quarterCategory[logbooksLst$FM %in% c(1,2,3)] <- "Q1"
  logbooksLst$quarterCategory[logbooksLst$FM %in% c(4,5,6)] <- "Q2"
  logbooksLst$quarterCategory[logbooksLst$FM %in% c(7,8,9)] <- "Q3"
  logbooksLst$quarterCategory[logbooksLst$FM %in% c(10,11,12)] <- "Q4"
  
  #
  # annotate with compareable codes, area
  # and reduce logbooks to areas in landings
  #
  landingsStox$areaCategory <- as.integer(landingsStox[["hovedomr\u00E5dekode"]])
  logbooksLst$areaCategory <- as.integer(logbooksLst$HO)
  logbooksLst <- logbooksLst[logbooksLst$areaCategory %in% landingsStox$areaCategory,]
  
  
  #
  # Define cells and adjust landings
  #
  totalcell <- c("vesselSizeCategory", "speciesCategory", "gearCategory")
  subcell <- c("quarterCategory", "areaCategory")
  
  proportions <- calculateCatchProportions(logbooksLst, totalcell, subcell, weight="VEKT")
  adjustedLandings <- adjustLandings(landingsStox, proportions, totalcell, subcell, "rundvekt")
  
  return(adjustedLandings)
}

context("landingscorrections no difference")
prepExample <- readRDS(system.file("extdata", "testresources","prepEcaWHB.rds", package="Rstox"))
gearTable <- readRDS(system.file("extdata", "testresources","gearTable.rds", package="Rstox"))
landings <- prepExample$StoxExport$landing

mockLog <- data.table::data.table(FAAR=as.integer(landings[["fangst\u00E5r"]]), REGM=as.character(landings$registreringsmerkeseddel), RE=as.character(landings$redskapkode), FM=as.integer(substr(landings$sistefangstdato,6,7)), HO=as.character(landings[["hovedomr\u00E5dekode"]]), LENG=as.double(landings[["st\u00F8rstelengde"]]), FISK=as.character(substr(landings$artkode,1,4)), VEKT=as.double(landings$rundvekt))

adjustedLandings <- testAdjustLandingsWithLst(landings, mockLog, gearTable, "Trawl")
expect_equal(nrow(adjustedLandings), nrow(landings))
expect_equal(sum(adjustedLandings$rundvekt), sum(landings$rundvekt))
expect_equal(sum(adjustedLandings$rundvekt[adjustedLandings[["hovedomr\u00E5dekode"]]==43]), sum(landings$rundvekt[landings[["hovedomr\u00E5dekode"]]==43]))

context("landingscorrections extra areas in logbooks")
prepExample <- readRDS(system.file("extdata", "testresources","prepEcaWHB.rds", package="Rstox"))
gearTable <- readRDS(system.file("extdata", "testresources","gearTable.rds", package="Rstox"))
landings <- prepExample$StoxExport$landing

mockLog <- data.table::data.table(FAAR=as.integer(landings[["fangst\u00E5r"]]), REGM=as.character(landings$registreringsmerkeseddel), RE=as.character(landings$redskapkode), FM=as.integer(substr(landings$sistefangstdato,6,7)), HO=as.character(landings[["hovedomr\u00E5dekode"]]), LENG=as.double(landings[["st\u00F8rstelengde"]]), FISK=as.character(substr(landings$artkode,1,4)), VEKT=as.double(landings$rundvekt))
extraA <- mockLog[1:10,]
extraA$HO<-99
mockLog <- rbind(mockLog, extraA)

adjustedLandings <- testAdjustLandingsWithLst(landings, mockLog, gearTable, "Trawl")
expect_equal(nrow(adjustedLandings), nrow(landings))
expect_equal(sum(adjustedLandings$rundvekt), sum(landings$rundvekt))
expect_equal(sum(adjustedLandings$rundvekt[adjustedLandings[["hovedomr\u00E5dekode"]]==43]), sum(landings$rundvekt[landings[["hovedomr\u00E5dekode"]]==43]))


context("landingscorrections area adjusted")
prepExample <- readRDS(system.file("extdata", "testresources","prepEcaWHB.rds", package="Rstox"))
gearTable <- readRDS(system.file("extdata", "testresources","gearTable.rds", package="Rstox"))
landings <- prepExample$StoxExport$landing
mockLog <- data.table::data.table(FAAR=as.integer(landings[["fangst\u00E5r"]]), REGM=as.character(landings$registreringsmerkeseddel), RE=as.character(landings$redskapkode), FM=as.integer(substr(landings$sistefangstdato,6,7)), HO=as.character(landings[["hovedomr\u00E5dekode"]]), LENG=as.double(landings[["st\u00F8rstelengde"]]), FISK=as.character(substr(landings$artkode,1,4)), VEKT=as.double(landings$rundvekt))
mockLog[mockLog$HO=="43", "VEKT"] <- mockLog[mockLog$HO=="43", "VEKT"]*1.2

frac43Log <- sum(mockLog[mockLog$HO=="43", "VEKT"]) / sum(mockLog$VEKT)
frac43Land <- sum(landings[landings[["hovedomr\u00E5dekode"]]=="43", "rundvekt"]) / sum(landings$rundvekt)

adjustedLandings <- testAdjustLandingsWithLst(landings, mockLog, gearTable, "Trawl")
expect_equal(nrow(adjustedLandings), nrow(landings))
expect_equal(sum(adjustedLandings$rundvekt), sum(landings$rundvekt))
expect_lt(abs(sum(adjustedLandings$rundvekt[adjustedLandings[["hovedomr\u00E5dekode"]]==43])/sum(adjustedLandings$rundvekt) - frac43Log)/frac43Log, 1e-4)


context("landingscorrections area missing landings")
prepExample <- readRDS(system.file("extdata", "testresources","prepEcaWHB.rds", package="Rstox"))
gearTable <- readRDS(system.file("extdata", "testresources","gearTable.rds", package="Rstox"))
landings <- prepExample$StoxExport$landing
mockLog <- data.table::data.table(FAAR=as.integer(landings[["fangst\u00E5r"]]), REGM=as.character(landings$registreringsmerkeseddel), RE=as.character(landings$redskapkode), FM=as.integer(substr(landings$sistefangstdato,6,7)), HO=as.character(landings[["hovedomr\u00E5dekode"]]), LENG=as.double(landings[["st\u00F8rstelengde"]]), FISK=as.character(substr(landings$artkode,1,4)), VEKT=as.double(landings$rundvekt))
referenceFraction <- sum(landings$rundvekt[landings[["hovedomr\u00E5dekode"]] == 8 & landings$temporal=="Q3"]) / sum(landings$rundvekt)
landings <- landings[!(landings[["hovedomr\u00E5dekode"]] == 8 & landings$temporal=="Q3"),]
expect_true(sum(mockLog$HO==8 & (mockLog$FM %in% c(7,8,9))) > 0)
expect_equal(sum(landings[["hovedomr\u00E5dekode"]]==8 & (substr(landings$sistefangstdato,6,7) %in% c("07","08","09"))), 0)

adjustedLandings <- testAdjustLandingsWithLst(landings, mockLog, gearTable, "Trawl")
expect_equal(sum(adjustedLandings$rundvekt), sum(landings$rundvekt))
expect_gt(nrow(adjustedLandings), nrow(landings))
expect_lt(abs((sum(adjustedLandings$rundvekt[adjustedLandings$areaCategory == 8 & adjustedLandings$quarterCategory == "Q3"]) / sum(adjustedLandings$rundvekt)) - referenceFraction)/referenceFraction, 1e-3)

context("landingscorrections area missing logbooks")
prepExample <- readRDS(system.file("extdata", "testresources","prepEcaWHB.rds", package="Rstox"))
gearTable <- readRDS(system.file("extdata", "testresources","gearTable.rds", package="Rstox"))
landings <- prepExample$StoxExport$landing
mockLog <- data.table::data.table(FAAR=as.integer(landings[["fangst\u00E5r"]]), REGM=as.character(landings$registreringsmerkeseddel), RE=as.character(landings$redskapkode), FM=as.integer(substr(landings$sistefangstdato,6,7)), HO=as.character(landings[["hovedomr\u00E5dekode"]]), LENG=as.double(landings[["st\u00F8rstelengde"]]), FISK=as.character(substr(landings$artkode,1,4)), VEKT=as.double(landings$rundvekt))
mockLog <- mockLog[mockLog$HO!=43,]

adjustedLandings <- testAdjustLandingsWithLst(landings, mockLog, gearTable, "Trawl")
expect_equal(sum(adjustedLandings$rundvekt), sum(landings$rundvekt))
expect_equal(sum(adjustedLandings$rundvekt[adjustedLandings$landings[["hovedomr\u00E5dekode"]]==43]), 0)




#
# landing annotation with psv files
#


#
# test annotate gear
#
context("annotate gear psv logbooks")
logbook <- readErsFile(system.file("extdata", "testresources","logbooks_trimmed_2015.psv", package="Rstox"))
gearTable <- readRDS(system.file("extdata", "testresources","gearTable.rds", package="Rstox"))
nrowPre <- nrow(logbook)
logbook <- annotateLogbooksGear(logbook, gearTable, "gearfactor")
expect_true(all(logbook$gearfactor == "Seine+pots"))
expect_equal(nrow(logbook), nrowPre)

logbook <- readRDS(system.file("extdata", "testresources","HAD_logbook_2018.rds", package="Rstox"))
logbook <- annotateLogbooksGear(logbook, gearTable, "gearfactor")
gearECA <- c("51","52","54","50","53")
expect_equal(sum(!is.na(logbook$gearfactor) & logbook$gearfactor=="Trawl"), sum(!is.na(logbook$REDSKAP_NS) & logbook$REDSKAP_NS %in% gearECA))


context("annotate temporal psv logbooks")
logbook <- readErsFile(system.file("extdata", "testresources","logbooks_trimmed_2015.psv", package="Rstox"))
nrowPre <- nrow(logbook)
temporalTable <- readRDS(system.file("extdata", "testresources","temporalTable.rds", package="Rstox"))
logbook <- annotateLogbooksTemporal(logbook, temporalTable, "temporal")
expect_true(all(logbook$temporal == "Q1"))
expect_equal(nrow(logbook), nrowPre)

context("annotate spatial psv logbooks")
logbook <- readErsFile(system.file("extdata", "testresources","logbooks_trimmed_2015.psv", package="Rstox"))
nrowPre <- nrow(logbook)
spatialTable <- readRDS(system.file("extdata", "testresources","stratumpolygon.rds", package="Rstox"))
logbook <- annotateLogbooksSpatial(logbook, spatialTable, "spatial")
area4 <- logbook[logbook$spatial=="4",]
expect_true(all(substr(area4$LOKASJON_START,1,2) == "04"))


context("adjust laninggs Reca psv logbooks")
logbook <- readRDS(system.file("extdata", "testresources","HAD_logbook_2018.rds", package="Rstox"))
landings <- readRDS(system.file("extdata", "testresources","HAD_landings_2018_sampled10K.rds", package="Rstox"))
spatialTable <- readRDS(system.file("extdata", "testresources","stratumpolygon.rds", package="Rstox"))
temporalTable <- readRDS(system.file("extdata", "testresources","temporalTable.rds", package="Rstox"))
gearTable <- readRDS(system.file("extdata", "testresources","gearTable.rds", package="Rstox"))

adjustedLandings <- adjustRecaSpatialTemporal(landings, logbook, gearTable, temporalTable, spatialTable, "Trawl")
expect_equal(sum(adjustedLandings$rundvekt), sum(landings$rundvekt))
expect_equal(sum(landings$gearfactor!="Trawl"), sum(adjustedLandings$gearfactor!="Trawl"))
expect_equal(sum(landings$rundvekt[landings$gearfactor!="Trawl"]), sum(adjustedLandings$rundvekt[adjustedLandings$gearfactor!="Trawl"]))
expect_true(all(!is.na(adjustedLandings$sistefangstdato)))
expect_true(all(!is.na(adjustedLandings$artkode)))
expect_true(all(!is.na(adjustedLandings$redskapkode)))

context("adjust lanings Reca psv logbooks check fraction o 15")
logbook <- readRDS(system.file("extdata", "testresources","HAD_logbook_2018.rds", package="Rstox"))
landings <- readRDS(system.file("extdata", "testresources","HAD_landings_2018_sampled10K.rds", package="Rstox"))
spatialTable <- readRDS(system.file("extdata", "testresources","stratumpolygon.rds", package="Rstox"))
temporalTable <- readRDS(system.file("extdata", "testresources","temporalTable.rds", package="Rstox"))
gearTable <- readRDS(system.file("extdata", "testresources","gearTable.rds", package="Rstox"))

adjustedLandings <- adjustRecaSpatialTemporal(landings, logbook, gearTable, temporalTable, spatialTable, "Trawl")

omr1 <- c("20","21","22","23","25","26","27")
omr2 <- c("3", "2", "10", "11", "13", "14", "15", "16", "17", "18", "24", "1")
months <- c(1,2,3)
gearECA <- c("51","52","54","50","53")


logbook <- annotateLogbooksSpatial(logbook, spatialTable, "spatial")
logbook <- annotateLogbooksGear(logbook, gearTable, "gearfactor")
logbook <- annotateLogbooksTemporal(logbook, temporalTable, "temporal")
cell1log <- logbook[!is.na(logbook$spatial) & logbook$spatial=="20-21-22-23-25-26-27" & !is.na(logbook$gearfactor) & logbook$gearfactor == "Trawl" & !is.na(logbook$temporal) & logbook$temporal=="Q1",]
cell2log <- logbook[!is.na(logbook$spatial) & logbook$spatial=="3-2-10-11-13-14-15-16-17-18-24-1" & !is.na(logbook$gearfactor) & logbook$gearfactor == "Trawl" & !is.na(logbook$temporal) & logbook$temporal=="Q1",]
ratioLog <- sum(cell1log$RUNDVEKT) / sum(cell2log$RUNDVEKT)

trawlo15 <- adjustedLandings[adjustedLandings$gearfactor == "Trawl",]
cell1adj <- trawlo15[trawlo15$spatial=="20-21-22-23-25-26-27" & trawlo15$gearfactor == "Trawl" & trawlo15$temporal=="Q1",]
cell2adj <- trawlo15[trawlo15$spatial=="3-2-10-11-13-14-15-16-17-18-24-1" & trawlo15$gearfactor == "Trawl" & trawlo15$temporal=="Q1",]
ratioAdj <- sum(cell1adj$rundvekt) / sum(cell2adj$rundvekt)

expect_lt(abs(ratioLog-ratioAdj)/ratioLog, 1e6)

context("adjust lanings Reca error messages")
logbook <- readRDS(system.file("extdata", "testresources","HAD_logbook_2018.rds", package="Rstox"))
landings <- readRDS(system.file("extdata", "testresources","HAD_landings_2018_sampled10K.rds", package="Rstox"))
spatialTable <- readRDS(system.file("extdata", "testresources","stratumpolygon.rds", package="Rstox"))
temporalTable <- readRDS(system.file("extdata", "testresources","temporalTable.rds", package="Rstox"))
gearTable <- readRDS(system.file("extdata", "testresources","gearTable.rds", package="Rstox"))
landings$spatial[1] <- "Not a spatial"
expect_error(adjustRecaSpatialTemporal(landings, logbook, gearTable, temporalTable, spatialTable, "Trawl"), "Landings contain illegal values for covariate: spatial")

logbook <- readRDS(system.file("extdata", "testresources","HAD_logbook_2018.rds", package="Rstox"))
landings <- readRDS(system.file("extdata", "testresources","HAD_landings_2018_sampled10K.rds", package="Rstox"))
spatialTable <- readRDS(system.file("extdata", "testresources","stratumpolygon.rds", package="Rstox"))
temporalTable <- readRDS(system.file("extdata", "testresources","temporalTable.rds", package="Rstox"))
gearTable <- readRDS(system.file("extdata", "testresources","gearTable.rds", package="Rstox"))
landings$gearfactor[1] <- "Not a gear"
expect_error(adjustRecaSpatialTemporal(landings, logbook, gearTable, temporalTable, spatialTable, "Trawl"), "Landings contain illegal values for covariate: gearfactor")

logbook <- readRDS(system.file("extdata", "testresources","HAD_logbook_2018.rds", package="Rstox"))
landings <- readRDS(system.file("extdata", "testresources","HAD_landings_2018_sampled10K.rds", package="Rstox"))
spatialTable <- readRDS(system.file("extdata", "testresources","stratumpolygon.rds", package="Rstox"))
temporalTable <- readRDS(system.file("extdata", "testresources","temporalTable.rds", package="Rstox"))
gearTable <- readRDS(system.file("extdata", "testresources","gearTable.rds", package="Rstox"))
landings$temporal[1] <- "Not a temporal"
expect_error(adjustRecaSpatialTemporal(landings, logbook, gearTable, temporalTable, spatialTable, "Trawl"), "Landings contain illegal values for covariate: temporal")


