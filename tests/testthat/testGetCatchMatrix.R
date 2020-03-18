pred <- readRDS(system.file("extdata", "testresources", "ecaPredExample.rds", package = "Rstox"))

context("get catch at age no plusgr")

tab<-getCatchMatrix(pred)
expect_equal(rank(as.integer(as.character(tab$means$age))), rank(order(as.integer(as.character(tab$means$age)))))
expect_equal(class(tab$means$age), class(tab$caa_scaled$age))
expect_equal(class(tab$means$age), class(tab$cv$age))

context("get catch at age plusgr")
tabPg<-getCatchMatrix(pred, plusgr = 7)
expect_equal(rank(as.integer(as.character(tab$means$age[1:length(tab$means$age)]))), rank(order(as.integer(as.character(tab$means$age[1:length(tab$means$age)])))))
expect_equal(class(tab$means), class(tabPg$means))
