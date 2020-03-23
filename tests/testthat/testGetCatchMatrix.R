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

context("save Catch at age")
tempf <- tempfile()
saveCatchMatrix(pred, tempf, savemeans = T)
tt <- read.csv(tempf, sep="\t", comment.char = "#")
expect_equal(rank(tt$age), rank(order(tt$age)))

saveCatchMatrix(pred, tempf)
tt <- read.csv(tempf, sep="\t", comment.char = "#")
expect_equal(nrow(tt),20)
expect_equal(ncol(tt),101)
expect_equal(names(tt)[1], "age")

file.remove(tempf)

context("get catch at age 3+")
pred <- readRDS(system.file("extdata", "testresources", "ecaPredExampleAge3_24.rds", package = "Rstox"))
tab<-getCatchMatrix(pred)
expect_equal(nrow(tab$means), 24-3+1)
expect_equal(tab$means$age[1], "3")
expect_equal(nrow(tab$cv), 24-3+1)
expect_equal(tab$cv$age[1], "3")


