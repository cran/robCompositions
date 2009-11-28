\name{outCoDa}
\alias{outCoDa}
\title{ Outlier detection for compositional data }
\description{
Outlier detection for compositional data using standard and robust statistical methods.
}
\usage{
outCoDa(x, alpha = 0.05, method = "robust", h = 1/2)
}
\arguments{
  \item{x}{ compositional data }
  \item{alpha}{ significance level }
  \item{method}{either \dQuote{robust} (default) or \dQuote{standard} }
  \item{h}{ the size of the subsets for the robust covariance estimation according the MCD-estimator for which the determinant is
          minimized (the default is (n+p+1)/2).}
}
\details{
The outlier detection procedure is based on (robust) Mahalanobis distances after a isometric logratio transformation of the data. 
Observations with squared Mahalanobis distance 
greater equal a certain quantile of the Chi-squared distribution are 
marked as outliers.

If method \dQuote{robust} is chosen, the outlier detection is based on the homogeneous majority of the compositional data set. 
If method \dQuote{standard} is used, standard measures of location
 and scatter are applied during the outlier detection procedure.
}
\value{
  \item{mahalDist }{resulting Mahalanobis distance}
  \item{limit }{(1 - alpha) quantile of the Chi-squared distribution}
  \item{outlierIndex }{logical vector indicating outliers and non-outliers}
}
\references{
Egozcue J.J., V. Pawlowsky-Glahn, G. Mateu-Figueras and
C. Barcel'o-Vidal (2003) Isometric logratio transformations for
compositional data analysis. \emph{Mathematical Geology}, \bold{35}(3)
279-300. \\
 
Filzmoser, P., and Hron, K. (2008)
  Outlier detection for compositional data using robust methods. \emph{Math. Geosciences}, \bold{40} 233-248.\\
  
Rousseeuw, P.J., Van Driessen, K. (1999) 
  A fast algorithm for the minimum covariance determinant estimator. 
  \emph{Technometrics}, \bold{41}  
  212-223. 
 }
\author{ Matthias Templ, Karel Hron }
\note{
It is highly recommended to use the robust version of the procedure.
}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
data(expenditures)
oD <- outCoDa(expenditures)
oD
}
\keyword{ multivariate }
