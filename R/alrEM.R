alrEM <- function(x, pos=ncol(x), dl=rep(0.05, ncol(x)-1), eps=0.0001, maxit=50) {
  stopifnot(all(x[,pos] != 0 & length(which(is.na(x[,pos]))) == 0))
  m=matrix(rep(dl,each=nrow(x)),ncol=length(dl))
  phi <- log(m/x[,pos]) 
  xOrig <- x
  if(length(colnames) < 1) names <- letters[1:ncol(x)] else names <- colnames(x)
  nam <- colnames(x)[pos]
  x <- cbind(x[, -pos], x[, pos])
  w <- which(colnames(x) == "x[, pos]")
  colnames(x)[w] <- nam
  ## close the X rows to 1:
  xc <- constSum(x)
  ## convert zeros into missing values:
  x[x==0] <- NA
  ## transform x into y=alr(x,pos):
  # x is already in the correct order
  y <- alr(x)$x.alr
  y <- data.frame(y)
  w <- is.na(y)
  wr <- apply(y, 1, function(x) any(is.na(x)) )
  wrr <- which(wr)
  it <- 0
  d <- 99999999
  y <- as.matrix(y)
  y[w] <- 0 
  it <- 0
  amountMiss <- length(which(w))
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
  }
  

  ximp <- suppressWarnings(invalr(y))
  w <- which(colnames(ximp) == "rat")
  colnames(ximp)[w] <- nam
  colnames(ximp)[which(colnames(ximp) =="")] <- names[pos]
  ximp <- ximp[, names]
  
  res <- list(xOrig=xOrig, xImp=ximp, wind=NULL, iter=it, eps=eps) 
  invisible(res)
}


