pred <- readRDS(system.file("extdata", "testresources", "ecaPredExample.rds", package = "Rstox"))

context("Age-group parameters no plusgr")

tab<-getAgeGroupParamaters(pred)
expect_equal(nrow(tab), 20)
expect_equal(ncol(tab), 5)

context("Age-group parameters plusgr")

tabp19<-getAgeGroupParamaters(pred, plusgr=19)
expect_equal(nrow(tabp19), 19)
weightsd19 <- tab$meanWeightsG.sd[19]
plsugrSd <- sqrt((tab$meanWeightsG.sd[19]**2)*(0.01896988) + (tab$meanWeightsG.sd[20]**2)*(0.74350748))
expect_equal(tabp19$meanWeightsG.sd[19], plsugrSd)
lengthsd19 <- tab$meanLengthCm.sd[19]
plsugrSd <- sqrt((tab$meanLengthCm.sd[19]**2)*(0.01896988) + (tab$meanLengthCm.sd[20]**2)*(0.74350748))
expect_equal(tabp19$meanLengthCm.sd[19], plsugrSd)

