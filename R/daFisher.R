daFisher <- function(x,grp,coda=TRUE,method="classical",plotScore=FALSE)
{

# Fisher LDA:
if(length(grp) != dim(x)[1]){
	stop(paste("grp must be of length",dim(x)[1]))
}
if(dim(x)[2] < 1){
	stop("matrix or data.frame expected.")
}
if(coda){
	x <- ilr(x)
}
	


p <- ncol(x)
ni <- table(grp)
ng <- length(ni)
n <- sum(ni)
pi <- ni/n
if (method=="classical"){
  muil <- by(x,factor(grp),mean)
  sigil <- by(x,factor(grp),cov)
}
else {
#  require(rrcov)
  res <- by(x,factor(grp),CovMcd)
  print(res)
  muil <- lapply(res,getCenter)
  sigil <- lapply(res,getCov)
}

mui <- matrix(unlist(muil),ng,p,byrow=TRUE)
mu <- pi%*%mui
hlp <- diag(sqrt(pi))%*%(mui-rep(1,ng)%*%mu)
B <- t(hlp)%*%hlp
sigi <- array(unlist(sigil),dim=c(p,p,ng))
W <- apply(sigi*array(sort(rep(pi,p*p)),dim=c(p,p,ng)),c(1,2),sum)
adir <- matrix(as.real(eigen(solve(W)%*%B)$vec),ncol=p)
adirs <- t(t(adir)/(sqrt(diag(t(adir)%*%W%*%adir))))
if(plotScore){
  plot(x%*%adirs[,1:2],col=grp, pch=grp, cex=1.5)
}
res <- list(B=B,W=W,loadings=adir,coda=coda)
class(res) <- "daFisher"
invisible(res)
}



