\name{invilr}
\alias{invilr}
\title{ Inverse isometric log-ratio transformation }
\description{
The inverse transformation of \sQuote{ilr()}.
}
\usage{
invilr(x.ilr)
}
\arguments{
  \item{x.ilr}{ data frame or matrix }
}
\details{
For details on the choice of the balances, please, see at the research report for which the link is given below.
}
\value{
The transformed data.
}
\references{ Egozcue J.J., V. Pawlowsky-Glahn, G. Mateu-Figueras and
C. Barcel'o-Vidal (2003) Isometric logratio transformations for
compositional data analysis. \emph{Mathematical Geology}, \bold{35}(3)
279-300 \\

Hron, K. and Templ, M. and Filzmoser, P. (2010) Imputation of missing values for compositional data using classical and robust methods 
\emph{Computational Statistics and Data Analysis}, In Press, Corrected Proof, ISSN: 0167-9473, DOI:10.1016/j.csda.2009.11.023  
}
\author{ Karel Hron }
\seealso{ \code{\link{ilr}} }
\examples{
require(MASS)
Sigma <- matrix(c(5.05,4.95,4.95,5.05), ncol=2, byrow=TRUE)
set.seed(123)
z <- mvrnorm(100, mu=c(0,2), Sigma=Sigma)
x <- invilr(z)
head(x)
}
\keyword{ math }
