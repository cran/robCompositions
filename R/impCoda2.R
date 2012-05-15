`impCoda2` <-
		function(x, maxit=10, eps=0.5, method="ltsReg", closed=FALSE, 
				init="KNN", k=5, dl=NULL, noise=0.1, bruteforce=FALSE){
	
	## MT & KH, 1. Version April 2008
	## MT 01. August 2008 (modification).
	## MT 17. Oktober 2008 (adaption)
	## for method pca: classical, mcd, gridMAD
	## for regression: lm, ltsReg
	## if closed  == FALSE, ilr is applied.
	
	`ilrM` <-
			function(x, info=TRUE){
		x.ilr=matrix(NA,nrow=nrow(x),ncol=ncol(x)-1)
		D=ncol(x)
		for (i in 1:ncol(x.ilr)){
			x.ilr[,i]=sqrt((D-i)/(D-i+1))*log(((apply(as.matrix(x[,(i+1):D,drop=FALSE]),1,prod))^(1/(D-i)))/(x[,i]))
		} 
#		invisible(-x.ilr)
		if(info)  {res <- list(xilr=-x.ilr,
					xOrig=x)
			class(res) <- "ilrTransform"
		} else {
			res <- -x.ilr
		}
		res
	}
	`invilrM` <-
			function(x.ilr){
		if(class(x.ilr) =="ilrTransform" ){
			fac <- rowSums(x.ilr$xOrig)
			x.ilr <- x.ilr$xilr
			y <- matrix(0,nrow=nrow(x.ilr),ncol=ncol(x.ilr)+1)
			D=ncol(x.ilr)+1
			y[,1]=-sqrt((D-1)/D)*x.ilr[,1]
			for (i in 2:ncol(y)){
				for (j in 1:(i-1)){
					y[,i]=y[,i]+x.ilr[,j]/sqrt((D-j+1)*(D-j))
				}
			}
			for (i in 2:(ncol(y)-1)){
				y[,i]=y[,i]-sqrt((D-i)/(D-i+1))*x.ilr[,i]
			}
			yexp=exp(-y)
			x.back=yexp/apply(yexp,1,sum) * fac # * rowSums(derOriginaldaten)
			invisible(x.back)			
		} else {
			y=matrix(0,nrow=nrow(x.ilr),ncol=ncol(x.ilr)+1)
			D=ncol(x.ilr)+1
			y[,1]=-sqrt((D-1)/D)*x.ilr[,1]
			for (i in 2:ncol(y)){
				for (j in 1:(i-1)){
					y[,i]=y[,i]+x.ilr[,j]/sqrt((D-j+1)*(D-j))
				}
			}
			for (i in 2:(ncol(y)-1)){
				y[,i]=y[,i]-sqrt((D-i)/(D-i+1))*x.ilr[,i]
			}
			yexp=exp(-y)
			x.back=yexp/apply(yexp,1,sum) # * rowSums(derOriginaldaten)
			invisible(x.back)
			#return(yexp)
		}
		x.back
	}
	
	
	
	if( is.vector(x) ) stop("x must be a matrix or data frame")
	stopifnot((method %in% c("ltsReg", "ltsReg2", "classical", "lm", "roundedZero","roundedZeroRobust")))
	if( k > nrow(x)/4 ) warning("k might be too large")
#	if(method == "roundedZero") init <- "roundedZero"
	
	xcheck <- x
	
#	if(method == "roundedZero"){
#		x[x==0] <- NA
#	}
	
	## values below detection limit has to be marked with -dl or with NaN.
	
	## detection limit:
	if( is.null(dl) & !any(x < 0) ){
	  	stop("provide information about the detection limit or say dl=FALSE")
	}
	if( !is.null(dl) & length(dl) < ncol(x)) stop(paste("dl has to be a vector of ", ncol(x)))
	if( length(dl) == ncol(x) & is.numeric(dl)){
	  wDL <- matrix(NA, ncol=ncol(x), nrow=nrow(x))
	  for(i in 1:ncol(x)){
		 wDL[,i] <- x[,i] < dl[i]  
	  }	  
	}
	if( any(x < 0)) {
		cat("negative values are taken as detection limits")
		wDL <- x < 0
		wDLn <- !wDL
		wDL2 <- apply(x, 1, function(x){
					length(which(x < 0))
				})
		dl <- apply(x, 2, function(x){
					if(min(x) < 0) min(x) else if(min(x) >= 0) NA
				})
		dl <- as.numeric(dl)
	}
	
	## structural zeros == 0
	if( any(x==0) ){
		cat("\n any x == 0 is considered as structural zeros.")
		cat("\n see the help for more information.\n ")		
		wS <- x == 0
		wSn <- !wS
		wS2 <- apply(x, 1, function(x){
					length(which(x==0))
				})
	}
	
	## real missings marked with NA
	if( is.na(x)){
		cat("\n NA values are considered as missing values and not values below detection limit.")
		wNA <- is.na(x)
		wNAn <- !is.na(x)
		wNA2 <- apply(x, 1, function(x){
					length(which(is.na(x)))
				})
	}
	
    ##sort the columns of the data according to the amount of missings in the variables:
	if( is.null(dl)){
		indM <- sort(apply(x,2,function(x) length(which(is.na(x)))),index.return=TRUE,decreasing=TRUE)$ix
	} else {
		indM <- sort(apply(wDL, 2, sum),index.return=TRUE,decreasing=TRUE)$ix
	}

	cn <- colnames(x)	
	
	## replace all real NAs with values with 'nearest neighbour' algorithm
	x <- impKNNa(x, k=k, metric="Aitchison", normknn=TRUE)$xImp #"Aitchison"

	## replace all values below DL with 2/3 DL times runif.
	if( length(dl) == ncol(x)){
		for(i in 1:length(dl)){
			if(length(wDL[,i]) > 0) x[wDL,i] <- dl[i]*runif(sum(wDL),1/3,2/3)
		}
	}


		
		
		
		#x=acomp(x) #Aitchison compositions (for ilr)
		#x2 <- acomp(xcheck) # with missings
		
		##PCA algorithmus
		
		it=0
		criteria <- 10000000
		error <- rep(0, ncol(x))
		
		###########################################
		###  start the iteration
		
		##require(StatDA)
		##ternary(acomp(x))
		#plot(ilr(x[w2==0,]), xlim=c(-5,5), ylim=c(-8,0.5))
		#points(ilr(x[w2>0,]), col=gray(0.9), pch=3)
		#gr <- seq(0.7,0.3, length.out=8)
		
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
					xilr[w[, indM[i]], 1] <- fitted(reg1)[w[, indM[i]]]  
					error[indM[i]] <- noise*sd(xilr[,1])#sqrt(mad(xilr[,1]))
					#+  
					#    rnorm(length(imp[w[, indM[i]]]), 0, sd=0.5*sqrt(mad(xilr[,1]))) 
					#  xilr <- data.frame(xilr)
					###imp[w[, indM[i]]] + rnorm(length(imp[w[, indM[i]]]), 0, sd=0.5*sqrt(mad(xilr[,1]))) 
				}
#				if(method == "roundedZero"){
#					xilr <- ilrM(x)
#					phi <- ilr(cbind(rep(dl[indM[i]], nrow(x)), x[,-1,drop=FALSE]))[,1]
#					xilr <- data.frame(xilr)
#					c1 <- colnames(xilr)[1]
#					colnames(xilr)[1] <- "V1"
#					reg1 = lm(V1 ~ ., data=xilr)
#					yhat2 <- predict(reg1, new.data=xilr[,-i]) 	
#					#colnames(xilr)[1] <- c1
#					#s <- sd(xilr[,1], na.rm=TRUE)
#					#ex <- (phi - yhat)/s
#					#yhat2 <- yhat - s*dnorm(ex)/pnorm(ex)
#					if(bruteforce){ 
#						xilr[w[, indM[i]], 1] <- ifelse(yhat2[w[, indM[i]]] <= phi[w[, indM[i]]], phi[w[, indM[i]]], yhat2[w[, indM[i]]] )
#					} else {
#						s <- sd(reg1$res, na.rm=TRUE)
#						ex <- (phi - yhat2)/s 
#						yhat2 <- yhat2 - s*dnorm(ex)/pnorm(ex)
#						xilr[w[, indM[i]], 1] <- yhat2[w[, indM[i]]]
#			        }
#				}
#				if(method == "roundedZeroRobust"){
#					xilr <- ilrM(x)
#					phi <- ilr(cbind(rep(dl[indM[i]], nrow(x)), x[,-1,drop=FALSE]))[,1]
#					xilr <- data.frame(xilr)
#					c1 <- colnames(xilr)[1]
#					colnames(xilr)[1] <- "V1"
#					reg1 = rlm(V1 ~ ., data=xilr, method="MM")
#					yhat2 <- predict(reg1, new.data=xilr[,-i]) 	
#					#colnames(xilr)[1] <- c1
#					#s <- sd(xilr[,1], na.rm=TRUE)
#					#ex <- (phi - yhat)/s
#					#yhat2 <- yhat - s*dnorm(ex)/pnorm(ex)
#					if(bruteforce){ 
#						xilr[w[, indM[i]], 1] <- ifelse(yhat2[w[, indM[i]]] <= phi[w[, indM[i]]], phi[w[, indM[i]]], yhat2[w[, indM[i]]] )
#					} else {
				##						s <- mad(reg1$res, na.rm=TRUE)
				##					s <- reg1$s
				#				ex <- (phi - yhat2)/s 
				#				yhat2 <- yhat2 - s*dnorm(ex)/pnorm(ex)
				#				xilr[w[, indM[i]], 1] <- yhat2[w[, indM[i]]]
				##			}
				#	}
				#if( method == "rf" ){
				#  xilr[w[, indM[i]], 1] <- NA
				#  reg1 <- rfImpute(xilr[,1] ~ xilr[,-1], data=xilr)
				#  xilr[w[, indM[i]], 1] <- reg1[w[, indM[i]]] 
				#}
				
				if( closed == FALSE ) x=invilr(xilr) else x=xilr
#				if( closed == FALSE && method %in% c("roundedZero","roundedZeroRobust")) x=invilrM(xilr) else x=xilr			
				#return the order of columns
				
				xNA=x[,1]
				x1=x[,indM[i]]
				x[,1]=x1
				x[,indM[i]]=xNA
				
				
			}
			
			
			
			criteria <- sum( ((xold - x)/x)^2, na.rm=TRUE) #sum(abs(as.matrix(xold) - as.matrix(x)), na.rm=TRUE)  ## DIRTY: (na.rm=TRUE)
			#print(paste(method, ",", it, ",", "criteria=",round(criteria,3)))
			if(closed == FALSE) colnames(x) <- colnames(xcheck)
			
		}
		
		if( method == "ltsReg2"){ # finally, add an error for method ltsReg2 
			for(i in 1:ncol(x)){
				xNA=x[,indM[i]]
				x1=x[,1]
				x[,1]=xNA
				x[,indM[i]]=x1
				if( closed == FALSE ) xilr=ilr(x) else xilr=x
				ind <- cbind(w[, indM[i]], rep(FALSE, dim(w)[1]))	
				xilr <- data.frame(xilr)
				#c1 <- colnames(xilr)[1]
				#colnames(xilr)[1] <- "V1"
				#reg1 = ltsReg(V1 ~ ., data=xilr)
				#imp= as.matrix(cbind(rep(1, nrow(xilr)), xilr[,-1])) %*% reg1$coef 
				#colnames(xilr)[1] <- c1
				xilr[w[, indM[i]], 1] <- xilr[w[, indM[i]], 1] +  
						rnorm(length(which(w[, indM[i]])), 0, sd=error[indM[i]]) 
				xilr <- data.frame(xilr)
				if( closed == FALSE ) x=invilr(xilr) else x=xilr
				xNA=x[,1]
				x1=x[,indM[i]]
				x[,1]=x1
				x[,indM[i]]=xNA
			}
		}
		
		res <- list(xOrig=xcheck, xImp=x, criteria=criteria, iter=it, 
				maxit=maxit, w=length(which(w)), wind=w)
		
	
	
	class(res) <- "imp"
	invisible(res)
}

