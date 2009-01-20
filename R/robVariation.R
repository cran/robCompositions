`robVariation` <-
function(x){
    rvars <- matrix(0, ncol=ncol(x), nrow=ncol(x))
    for( i in 1:ncol(x)){
      for( j in 1:ncol(x)){
        if( i < j ) rvars[i,j] <- (mad(log(x[,i]/x[,j])))^2
      }
    }
    invisible(rvars) 
}

