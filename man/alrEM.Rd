\name{alrEM}
\alias{alrEM}
\title{ alr-EM Imputation for Rounded Zeros }
\description{
  A modified EM alr-algorithm for replacing rounded zeros in
compositional data sets.
}
\usage{
alrEM(x, pos = ncol(x), dl = rep(0.05, ncol(x) - 1), eps = 1e-04, maxit = 50)
}
\arguments{
  \item{x}{ Compositional data }
  \item{pos}{ Position of the rationing variable for alr transformation }
  \item{dl}{ Detection limit for each part }
  \item{eps}{ convergence criteria }
  \item{maxit}{ maximum number of iterations }
}
\details{
Statistical analysis of compositional data including zeros runs into problems, because log-ratios cannot be applied. 
Usually, rounded zeros are considerer as missing not at random missing values.
The algorithm first applies an additive log-ratio transformation to the compositions. Then the rounded zeros are imputed
using a modified EM algorithm.
Details can be found in Palarea-Albaladej and Martinn-Fernandez (2008). 

The major difference between their MATLAB implementation and this R function is that they use the reg.sweep methodology for 
regression analysis. Nevertheless, we use the standard lm function from R which based on QR-decompositons, which is more
accurate than sweep.
}
\value{
  \item{xOrig }{Original data frame or matrix}
  \item{xImp }{Imputed data}
  \item{wind }{Index of the missing values in the data}
  \item{iter }{Number of iterations}
  \item{eps }{eps}
}
\references{ Palarea-Albaladej, J. and Mart'in-Fern'andez, J.A. (2008) A modified EM alr-algorithm for replacing rounded zeros in
compositional data sets. \emph{Computers and Geosciences}, \bold{34}, 902-917 
}
\author{ Matthias Templ and Karel Hron }
\seealso{ \code{\link{impCoda}} }
\examples{
data(arcticLake)
x <- arcticLake
## generate rounded zeros artificially:
x[x < 5] <- 0
xia <- alrEM(x, pos=2, dl=rep(5,3))
xia$xImp
}
\keyword{ multivariate }