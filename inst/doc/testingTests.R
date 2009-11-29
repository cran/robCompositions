library(robCompositions)
data(machineOperators)
x <- machineOperators

res <- function(reps=50, est="standard"){
	require(Matrix)
   out <- sapply(X=1:100, function(x){adtest(mvrnorm(100, mu=c(10,10,10), Sigma=Hilbert(3)), R=reps, locscatt=est)$p} )
   #out <- sapply(X=1:100, function(x){adtest(rnorm(100), R=reps, locscatt=est)$p} )
   outM <- length(which(out < 0.05))/length(out)
  output <- list(out=out, outM=outM)
  invisible(output)
}

set.seed(123)
a50 <- res()
set.seed(123)
a100 <- res(100)
set.seed(123)
a200 <- res(200)
set.seed(123)
a500 <- res(500)
set.seed(123)
a1000 <- res(1000)
set.seed(123)
a10000 <- res(10000)

set.seed(123)
ar50 <- res(est="robust")
set.seed(123)
ar100 <- res(100, est="robust")
set.seed(123)
ar200 <- res(200, est="robust")
set.seed(123)
ar500 <- res(500, est="robust")
set.seed(123)
ar1000 <- res(1000, est="robust")
set.seed(123)
ar10000 <- res(10000, est="robust")

res2 <- function(x=ilrx, reps=10, est="standard"){
  cat("\n 50: ")
  flush.console()
  out <- replicate(reps, adtest(x, R=50, locscatt=est)$p)
    cat("\n 100: ")
  flush.console()
  out2 <- replicate(reps, adtest(x, R=100, locscatt=est)$p)
    cat("\n 200: ")
  flush.console()
  out3 <- replicate(reps, adtest(x, R=200, locscatt=est)$p)
    cat("\n 500: ")
  flush.console()
  out4 <- replicate(reps, adtest(x, R=500, locscatt=est)$p)
    cat("\n 1000: ")
  flush.console()
  out5 <- replicate(reps, adtest(x, R=1000, locscatt=est)$p)
      cat("\n 10000: ")
  flush.console()
  out6 <- replicate(reps, adtest(x, R=10000, locscatt=est)$p)
    cat("\n ")
  d <- data.frame(n50=out, n100=out2, n200=out3,
                  n500=out4, n1000=out5, n10000=out6)
  invisible(d)
}

set.seed(123)

y <- res2(ilr(machineOperators), reps=5)
yr <-  res2(ilr(machineOperators), reps=5, est="robust")

pdf("bxp.pdf", width=7, height=7)

op <- par(xaxt="n")
plot(0:7, seq(0.11, 0.42, length.out=length(0:7)), type="n", xlab="", ylab="", main="")
b <- boxplot(y, xlab="value of parameter R", ylab="p-value",
             main="Resulting p-values of the A-D test (5 replications)",
             at=seq(0.5, 6, length.out=6),
             boxwex=0.3, add=TRUE)
b <- boxplot(yr, xlab="", ylab="",
             main="",
             at=seq(1, 6.5, length.out=6),
             boxwex=0.3, add=TRUE, col="lightgrey")
par(op)
axis(side=1, at=seq(0.7,6.5, length.out=6), labels=c(50,100,200,500,1000,10000))
legend("topright", fill=c("white","lightgrey"),
       legend=c("standard", "robust"))

dev.off()


###############################################################################################

## standard Gaussian.

library(robustbase)
library(MASS)
n=100
p=5

x <- mvrnorm(n=n, mu=rep(0,p), Sigma=diag(p))

n <- c(10,25,50,100,200,1000)
p <- c(2,3,5,7,10)
R=100


res <- function(R=100, n=c(25,50,100,200), p=c(2,3,5,7,10), sr="standard"){
  d <- matrix(, ncol=length(n), nrow=length(p)) 
  for(i in 1:length(n)){
     for(j in 1:length(p)){	
        d[j,i] <- mean(replicate(R, try(adtest(mvrnorm(n=n[i], mu=rep(0,p[j]), Sigma=diag(p[j])), locscatt = sr, R=200)$p)) < 0.05)
    }
	print(paste(n[i], "/ necessary:", n))
  }
  d
}

set.seed(123)
df <- res(R=50)
set.seed(123)
dfr <- res(R=50, sr="robust")



##############################################################################
genData <- function(n=1000, out=0.05, 
		Sigma=1*c(1,1)%*%t(c(1,1))+0.05*c(1,-1)%*%t(c(1,-1))){
	## Gruppe ohne Ausreisser:
	z <- mvrnorm(n, mu=c(0,0), Sigma=Sigma)
	N <- dim(z)[1]
	n1 <- N - floor(n*out)
	n2 <- N - floor(2*n*out)
	if(out > 0){
		z[(n1+1):N, ] <- mvrnorm(floor(n*out), mu=c(0,6), Sigma=Sigma) ## erste Ausreissergruppe (Euclidean+Aitchison)
	}
	z <- invilr(z) #ilr.inv(z)
	sum=runif(n1,0,1)  #rnorm(n1,10,1)
	z[1:n1, ] <- z[1:n1,] * sum
	if(out > 0){ 
		sum1=runif(floor(2*n*out),13,17) #rnorm(n2-n1,15,1)
		z[(n2+1):N, ] <- z[(n2+1):N, ] * sum1
		z[(n1+1):n2, ] <- z[(n1+1):n2, ] * 10
	}
	## generate missings
	zmiss <- z
	s <- c(0.2, 0.1, 0.05, 0.05)
	for(i in 1:ncol(z)){
		zmiss[sample(1:n2, floor(s[i]*n2)), i] <- NA #1:index
	}
	list(zmiss=zmiss, z2=z, good=n2)
}

set.seed(123)
g <- genData(n=100, out=0)$z2
x <- xOrig <- g/rowSums(g)
dl <- apply(x, 2, quantile, 0.05)
for(i in 1:ncol(x)-1){
	x[x[,i] < dl[i], i] <- 0
}
#x <- x[-which(rowSums(x) == 0),]

das <- function(xOrig, xImp){
	da <- function(x,y){
		p <- length(x)
		1/p * sum((log(x[1:(p-1)]/x[2:p]) - log(y[1:(p-1)]/y[2:p]))^2, na.rm=TRUE)
	}
	das <- 0
	for(i in 1:nrow(xOrig)){
		das <- das + da(x=xOrig[i,], y=xImp[i,])
	}
	das
}

pr <- function(xOrig, x, resALR, resILR, ...){
	cat("\n impCoda:", round(das(xOrig, resILR$xImp)/length(which(x==0)),3))
	cat("\n alrEM:  ", round(das(xOrig, resALR$xImp)/length(which(x==0)),3))
	cat("\n ------ \n")
	w <- apply(x, 1, function(x) any(x==0))
	cat("\n xOrig: \n")
	print(xOrig[w,])
	cat("\n\n xImp ILR: \n")
	print(resILR$xImp[w,])
	cat("\n\n xImp ALR: \n")
	print(resALR$xImp[w,])	
}

require(compositions)
require(robCompositions)
resALR <- alrEM(x, dl=dl[1:2], )
resILR <- adjust(impCoda(x, method="roundedZero", maxit=10))
pr(xOrig=xOrig, x=x, resALR=resALR, resILR=resILR)

plot(resILR, which=1)
#class(resALR) <- class(resILR)
#plot(resALR, which=1)
plot(resILR, which=3)