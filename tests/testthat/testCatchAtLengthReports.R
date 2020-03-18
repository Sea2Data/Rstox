pred <- readRDS(system.file("extdata", "testresources", "ecaPredExample.rds", package = "Rstox"))

calDefault <- getCatchAtLength(pred)
expect_equal(nrow(calDefault), 140)
expect_equal(ncol(calDefault), 3)

calMillions <- getCatchAtLength(pred, unit="million")
expect_equal(calDefault$mean / 1e6, calMillions$mean)

calWeightKg <- getCatchAtLength(pred, var="Weight", unit="kilograms")
calWeightT <- getCatchAtLength(pred, var="Weight", unit="tonnes")
expect_equal(calWeightKg$mean / 1e3, calWeightT$mean)

context("save Catch at length")
tempf <- tempfile()
saveCatchAtLength(pred, tempf)
tt <- read.csv(tempf, sep="\t", comment.char = "#")
expect_equal(tt$mean, calMillions$mean)
expect_equal(tt$sd, calMillions$sd)
expect_equal(nrow(tt),140)
expect_equal(ncol(tt),3)
file.remove(tempf)
