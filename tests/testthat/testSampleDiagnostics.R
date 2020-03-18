prep <- readRDS(system.file("extdata", "testresources", "prepEcaWHB.rds", package = "Rstox"))

context("get table for sample cellplot")

mm <- get_g_s_a_frame(prep$StoxExport)
expect_equal(sum(!is.na(prep$StoxExport$biotic$age)), sum(mm$aged))

m20 <- get_g_s_a_frame(prep$StoxExport, agesampletypes=c(20))
expect_equal(sum(!is.na(prep$StoxExport$biotic$sampletype) & prep$StoxExport$biotic$sampletype==20), sum(m20$aged))

m2023 <- get_g_s_a_frame(prep$StoxExport, agesampletypes=c(20,23))
expect_equal(sum(!is.na(prep$StoxExport$biotic$sampletype) & prep$StoxExport$biotic$sampletype %in% c(20,23)), sum(m2023$aged))