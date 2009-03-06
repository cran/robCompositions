alrEM <- function(x, pos=ncol(x), dl=rep(0.05, ncol(x)-1), eps=0.0001, maxit=50) {
	#require(compositions)
	stopifnot(all(x[,pos] != 0 & length(which(is.na(x[,pos]))) == 0))
	
	m=matrix(rep(dl,each=nrow(x)),ncol=length(dl))
	phi <- log(m/x[,pos]) 
	xOrig <- x
	if(length(colnames(x)) < 1) names <- paste("V", 1:ncol(x), sep="") else names <- colnames(x)
	x <- cbind(x[, -pos], x[, pos])
	## close the X rows to 1:
	xc <- acomp(x)
	## convert zeros into missing values:
	x[x==0] <- NA
	## transform x into y=alr(x,pos):
	# x is already in the correct order
	y <- data.frame(alr(x))
	w <- is.na(y)
	wr <- apply(y, 1, function(x) any(is.na(x)) )
	wrr <- which(wr)
	it <- 0
	d <- 99999999
	y <- as.matrix(y)
	y[w] <- 0 
	it <- 0
	amountMiss <- length(which(w))
	
	
	#w2 <- apply(x, 1, function(x){
	#			  length(which(is.na(x)))
	#	  })
	#plot(ilr(xOrig[w2==0,]), xlim=c(-5,5), ylim=c(-8,5.5))
	#points(ilr(x[wr>0,]), col=gray(0.9), pch=3)
	#gr <- seq(0.7,0.3, length.out=8)
	
	
	while( d > eps & it <= maxit ){
		it <- it + 1
		yold <- y
		for(i in 1:ncol(y)){ 
			lm1 <- lm( y[,i,drop=FALSE] ~ y[,-i,drop=FALSE] ) 
			yhat <- predict(lm1, new.data=y[,-i])	  
			s <- sd(y[,i], na.rm=TRUE)
			ex <- (phi[,i] - yhat)/s 
			yhat2 <- yhat - s*dnorm(ex)/pnorm(ex)	   
			y[w[,i],i] <- ifelse(yhat2[w[,i]] >= phi[w[,i],i], phi[w[,i],i], yhat2[w[,i]]) 
		}
		d <- sum(abs(y)- sum(abs(yold)))/amountMiss
		#print(length(which(w2>0)))
		#points(ilr(alr.inv(y[w2>0,])), col= gray(gr[it]), pch=3)
	}
	
	
	ximp <- alrInv(y)
	colnames(ximp)[which(colnames(ximp) =="")] <- names[pos]
	ximp <- ximp[, names]
	
	res <- list(xOrig=xOrig, xImp=ximp, wind=NULL, it=it, eps=eps) 
	invisible(res)
}


