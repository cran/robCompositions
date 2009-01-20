`impCoda` <-
function(x, maxit=10, eps=0.5, method="ltsReg", closed=FALSE, init="KNN", k=5){

## MT & KH, 1. Version April 2008
## MT 01. August 2008 (modification).
## MT 17. Oktober 2008 (adaption)
## for method pca: classical, mcd, gridMAD
## for regression: lm, ltsReg
## if closed  == FALSE, ilr is applied.

if( is.vector(x) ) stop("x must be a matrix or data frame")
stopifnot((method %in% c("ltsReg", "ltsReg2", "classical", "lm")))
if( k > nrow(x)/4 ) warning("k might be too large")

xcheck <- x

##index of missings / non-missings
w <- is.na(x)
wn <- !is.na(x)
w2 <- apply(x, 1, function(x){
          length(which(is.na(x)))
})

if(method == "gmean"){
### mean imputation im Simplex:
geometricmean <- function (x) {
    if (any(na.omit(x == 0)))
        0
    else exp(mean(log(unclass(x)[is.finite(x) & x > 0])))
}
gm <- apply(x, 2, function(x) {
  geometricmean(x[complete.cases(x)])
})

xmean <- x
for(i in 1:ncol(x)){
  xmean[w[,i], i] <- gm[i]
}
res <- list(xOrig=xcheck, xImp=xmean, criteria=0, iter=0, maxit=maxit, w=length(which(w)), wind=w)
} else if ( method=="meanClosed" ){
  xmean <- x
  xmean <- impute(xmean)
  res <- list(xOrig=xcheck, xImp=xmean, criteria=0, iter=0, maxit=maxit, w=length(which(w)), wind=w)
} else{



##sort the columns of the data according to the amount of missings in the variables

indM <- sort(apply(x,2,function(x) length(which(is.na(x)))),index.return=TRUE,decreasing=TRUE)$ix
cn <- colnames(x)

## first step - replace all NAs with values with 'nearest neighbour' algorithm

#if(init=="NN"){
#  library(templdistC)
#  x <- templdist.C(x)
#}
if(init=="KNN"){
  x <- impKNNa(x, k=k, metric="Aitchison", normknn=TRUE)$xImp #"Aitchison"
}
if(init=="KNNclosed"){
  x <- impKNNa(x, k=k, metric="Euclidean")$xImp
}


#x=acomp(x) #Aitchison compositions (for ilr)
#x2 <- acomp(xcheck) # with missings

##PCA algorithmus

it=0
criteria <- 10000000

###########################################
###  start the iteration

while(it <= maxit & criteria >= eps){

xold <- x
it=it+1
for(i in 1:ncol(x)){

#change the first column with that one with the highest amount of NAs
#in the step
xNA=x[,indM[i]]
x1=x[,1]
x[,1]=xNA
x[,indM[i]]=x1

if( closed == FALSE ) xilr=ilr(x) else xilr=x
#xilr2 <- ilr(x2) # warum keine Missings mehr? 

#apply the PCA algorithm -> ximp
ind <- cbind(w[, indM[i]], rep(FALSE, dim(w)[1]))
if(method=="classical" | method =="mcd" | method == "gridMAD"){
  xilr <- impPCA(xilr, indexMiss=ind, eps=1,
               indexObs=!ind, method=method)
}

#if( method == "em" ){
#  s <- prelim.norm(as.matrix(xilr)) 
#  thetahat <- em.norm(s, showits=FALSE)   
#  xilr <- imp.norm(s, thetahat, as.matrix(xilr))   
#}
#
#if( method == "lls" ){
#  xilr <- suppressWarnings(llsImpute(xmiss, 3, verbose = FALSE)@completeObs)
#}

if(method == "ltsReg" | method == "lm"){
     #beta=ltsReg(xilr[,1]~xilr[,2],xilr)$coefficients
  xilr <- data.frame(xilr)
  c1 <- colnames(xilr)[1]
  colnames(xilr)[1] <- "V1"
  reg1 = get(method)(V1 ~ ., data=xilr)
  colnames(xilr)[1] <- c1
  ##imp= cbind(rep(1, nrow(xilr)), xilr[,-1]) %*% reg1$coef  
  xilr[w[, indM[i]], 1] <- fitted(reg1)[w[, indM[i]]]   ##imp[w[, indM[i]]] ## xilr[w[, indM[i]], 1]
}
if(method == "ltsReg2"){
  xilr <- data.frame(xilr)
  c1 <- colnames(xilr)[1]
  colnames(xilr)[1] <- "V1"
  reg1 = ltsReg(V1 ~ ., data=xilr)
  imp= as.matrix(cbind(rep(1, nrow(xilr)), xilr[,-1])) %*% reg1$coef 
  colnames(xilr)[1] <- c1
  ##imp= cbind(rep(1, nrow(xilr)), xilr[,-1]) %*% reg1$coef  
  xilr[w[, indM[i]], 1] <- fitted(reg1)[w[, indM[i]]]  +  
    rnorm(length(imp[w[, indM[i]]]), 0, sd=0.5*sqrt(mad(xilr[,1]))) 
  xilr <- data.frame(xilr)
##imp[w[, indM[i]]] + rnorm(length(imp[w[, indM[i]]]), 0, sd=0.5*sqrt(mad(xilr[,1]))) 
}
#if( method == "rf" ){
#  xilr[w[, indM[i]], 1] <- NA
#  reg1 <- rfImpute(xilr[,1] ~ xilr[,-1], data=xilr)
#  xilr[w[, indM[i]], 1] <- reg1[w[, indM[i]]] 
#}

if( closed == FALSE ) x=invilr(xilr) else x=xilr

#return the order of columns

xNA=x[,1]
x1=x[,indM[i]]
x[,1]=x1
x[,indM[i]]=xNA

#print(paste(it,"-te Iteration:"))
#print(x[1,])


}


#print(get("method"))
#print(x[1,3])
#print(summary(reg1))
#print(method)

criteria <- sum(abs(as.matrix(xold) - as.matrix(x)), na.rm=TRUE)  ## DIRTY: (na.rm=TRUE)

colnames(x) <- colnames(xcheck)
}
res <- list(xOrig=xcheck, xImp=x, criteria=criteria, iter=it, maxit=maxit, w=length(which(w)), wind=w)
}
class(res) <- "imp"
invisible(res)
}

