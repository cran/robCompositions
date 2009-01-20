adjust <- function(x){
	# x ... object from class "imp"
	if(class(x) != "imp") stop("object x must be from class imp")
	xneu=xi$xImp
	s1 <- rowSums(xi$xOrig, na.rm=TRUE)
	for(i in 1:nrow(xi$xImp)){
		s <- sum(xi$xImp[i, !xi$wind[i,]])
		s2 <- sum(xi$xImp[i, xi$wind[i,]])
		fac <- s / (s + s2)
		s1[i] <-  s1[i] / fac
	}
	impS <- s1/rowSums(xi$xImp)
	for(i in 1:ncol(xi$xImp)){
		xneu[,i] <- xi$xImp[,i] * impS
	}
	x$xImp <- xneu
	invisible(x)
}