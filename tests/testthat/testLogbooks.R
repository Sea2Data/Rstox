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


#context("landingscorrections")
#logbooks <- readErsFile(system.file("extdata", "testresources","logbooks_trimmed_2015.psv", package="Rstox"))
#prepExample <- readRDS(system.file("extdata", "testresources","prepEcaWHB.rds", package="Rstox"))
#landings <- prepExample$StoxExport$landing

