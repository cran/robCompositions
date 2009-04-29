outCoDa <- function(x, alpha=0.05, method="robust", h=1/2){
	if(dim(x)[2] < 2) stop("need data with at least 2 variables")
	
	covEst <- function(x, type) {
		standard <- function(x){
				list(mean=colMeans(x, na.rm=TRUE), varmat=cov(x))  
		}
		robust <- function(x){
				v <- covMcd(x)
				list(mean=v$center, varmat=v$cov)
		}
		switch(type,
				standard = standard(x),
				robust = robust(x))
	}
		
	z <- ilr(x)
	cv <- covEst(z, method)
	dM <- sqrt(mahalanobis(z, center=cv$mean, cov=cv$varmat))
	limit <- sqrt(qchisq(p=1-alpha, df=ncol(x)-1))
	res <- list(mahalDist = dM, limit = limit, 
			    outlierIndex = dM > limit)
	class(res) <- "outCoDa"
    invisible(res)
}