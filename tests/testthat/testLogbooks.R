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


context("landingscorrections")
prepExample <- readRDS(system.file("extdata", "testresources","prepEcaWHB.rds", package="Rstox"))
gearTable <- readRDS(system.file("extdata", "testresources","gearTable.rds", package="Rstox"))
landings <- prepExample$StoxExport$landing

mockLog <- data.table::data.table(FAAR=as.integer(landings$fangstår), REGM=as.character(landings$registreringsmerkeseddel), RE=as.character(landings$redskapkode), FM=substr(as.character(landings$sistefangstdato),6,7), HO=as.character(landings$hovedområdekode), LENG=as.double(landings$størstelengde), FISK=as.character(substr(landings$artkode,1,4)), VEKT=as.double(landings$rundvekt))
#mockLog[mockLog$HO=="43", "VEKT"] <- mockLog[mockLog$HO=="43", "VEKT"]*1.2

adjustedLandings <- adjustGearLandingsWithLogbooksReca(landings, mockLog, gearTable, "Trawl")
expect_equal(nrow(adjustedLandings), nrow(landings))
expect_equal(sum(adjustedLandings$rundvekt), sum(landings$rundvekt))
expect_equal(sum(adjustedLandings$rundvekt[adjustedLandings$hovedområdekode==43]), sum(landings$rundvekt[landings$hovedområdekode==43]))


context("landingscorrections")

context("landingscorrections incomplete landings cells")

context("landingscorrections incomplete logbook cells")

#
# filtrer logbøker på aktivitet ?
# annoter område, kvartal, og o15m (+ kyst/hav for kysttorsk? basert på posisjon eller lokasjon eller use.key i Sondre sitt script ?)
# hent ut trål vha processdata
# fordel trål på område og kvartal
# juster sluttsedler
# 
#


