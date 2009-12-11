\name{pfa}
\alias{pfa}
\title{ Factor analysis for compositional data }
\description{
Computes the principal factor analysis of the input data. 
}
\usage{
pfa(x, factors, data = NULL, covmat = NULL, n.obs = NA, 
subset, na.action, start = NULL, 
scores = c("none", "regression", "Bartlett"), 
rotation = "varimax", maxiter = 5, control = NULL, ...)
}
\arguments{
  \item{x}{ (robustly) scaled input data  }
  \item{factors}{ number of factors  }
  \item{data}{ default value is NULL }
  \item{covmat}{ (robustly) computed covariance or correlation matrix }
  \item{n.obs}{ number of observations  }
  \item{subset}{ 	if a subset is used  }
  \item{na.action}{ what to do with NA values }
  \item{start}{ starting values  }
  \item{scores}{ which method should be used to calculate the scores }
  \item{rotation}{ 	if a rotation should be made  }
  \item{maxiter}{ maximum number of iterations  }
  \item{control}{ default value is NULL }
  \item{\dots}{ arguments for creating a list }
}
\details{
  The main difference to usual implementations is that 
  uniquenesses are nor longer of diagonal form. This kind of factor analysis is designed for centered log-ratio
  transformed compositional data. A robust version using the isometric log-ratio transformation could be chosen 
  (see the example below).
}
\value{
  \item{loadings }{A matrix of loadings, one column for each factor. The factors 
  are ordered in decreasing order of sums of squares of loadings.}
  \item{uniquness }{uniquness}
  \item{correlation }{correlation matrix}
  \item{criteria}{The results of the optimization: the value of the 
  negativ log-likelihood and information of the iterations used.}
  \item{factors }{the factors }
  \item{dof }{degrees of freedom}
  \item{method }{\dQuote{principal}}
  \item{n.obs }{number of observations if available, or NA}
  \item{call }{The matched call.}
  \item{STATISTIC, PVAL }{The significance-test statistic and p-value, if they can be computed}
}
\references{ C. Reimann, P. Filzmoser, R.G. Garrett, and R. Dutter (2008): 
Statistical Data Analysis Explained. 
\emph{Applied Environmental Statistics with R}. 
John Wiley and Sons, Chichester, 2008.

P. Filzmoser, K. Hron, C. Reimann, R. Garrett (2009): Robust Factor Analysis for Compositional Data. 
\emph{Computers and Geosciences}, \bold{35} (9), 1854--1861.
}
\author{ Peter Filzmoser, Karel Hron }
\examples{
data(expenditures)
x <- expenditures

# construct orthonormal basis: 
V <- matrix(0,nrow=ncol(x),ncol=ncol(x)-1)
for (i in 1:ncol(V)){
  V[1:i,i] <- 1/i
  V[i+1,i] <- (-1)
  V[,i] <- V[,i]*sqrt(i/(i+1))
}

z <- ilr(x) #ilr transformed data
y <- z %*% t(V) #clr transformed data

set.seed(200)
require(robustbase)
z.mcd <- covMcd(z)
mean_z <- z.mcd$center
mean_y <- V %*% mean_z 

var_z <- z.mcd$cov
var_y <- V %*% var_z %*% t(V)

#classical scaling
y.sc <- scale(y,scale=FALSE) #only centering

#robust scaling
#y.rsc <- scale(y,mean_y,scale=FALSE) #only centering


res1logcentr <- pfa(y.sc, factors=1, scores="Bartlett", rotation="varimax")


#res1Rlogcentr <- pfa(y.rsc, factors=1, covmat=var_y, scores="Bartlett", rotation="varimax")

}
\keyword{ multivariate }