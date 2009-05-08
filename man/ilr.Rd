\name{ilr}
\alias{ilr}
\title{ Isometric log-ratio transformation }
\description{
  An isometric log-ratio transformation with a special choice of the balances according to Hron et al. (2009).
}
\usage{
ilr(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ object of class data frame or matrix with positive entries}
}
\details{
  This choice of the balances separates all the relative information of the part \eqn{x_1} 
  from the remaining parts. It is useful for estimating missing values in 
  \eqn{x_1} by regression of the remaining variables.
}
\value{
  The ilr transformed data.
}
\references{ Egozcue J.J., V. Pawlowsky-Glahn, G. Mateu-Figueras and
C. Barcel'o-Vidal (2003) Isometric logratio transformations for
compositional data analysis. \emph{Mathematical Geology}, \bold{35}(3)
279-300. \\

Hron, K. and Templ, M. and Filzmoser, P. (2008) Imputation of missing values for compositional data using classical and robust methods 
\emph{Research Report SM-2008-4, Vienna University of Technology}, 15 pages.
 }
\author{ Karel Hron, Matthias Templ }
\seealso{ \code{\link{invilr}} }
\examples{
require(MASS)
Sigma <- matrix(c(5.05,4.95,4.95,5.05), ncol=2, byrow=TRUE)
z <- invilr(mvrnorm(100, mu=c(0,2), Sigma=Sigma))
require(compositions)
plot(acomp(z))
plot(invilr(z))
}
\keyword{ math }
