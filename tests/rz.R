require(robCompositions)
data(aitchison359)
xOrig <- x <- aitchison359

x[x < 5] <- 0

imp <- impCoda(x, method='roundedZero')
imp2 <- alrEM(x, pos=2, dl=rep(5,3))$xImp


