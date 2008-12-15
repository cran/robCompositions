`aDist` <-
function(x,y){
    d <- 0
    p <- length(x)
    for(i in 1:(p-1)){
      for(j in (i+1):p){
        d <- d + (log(x[i]/x[j]) - log(y[i]/y[j]))^2
      }
    }
    d=d/p
    sqrt(d)
  }

