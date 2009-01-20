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
}
\value{
The transformed data.
}
\references{ Egozcue J.J., V. Pawlowsky-Glahn, G. Mateu-Figueras and
C. Barcel'o-Vidal (2003) Isometric logratio transformations for
compositional data analysis. \emph{Mathematical Geology}, \bold{35}(3)
279-300 \\

Hron, K. and Templ, M. and Filzmoser, P. (2008) Imputation of missing values for compositional data using classical and robust methods 
\emph{Research Report SM-2008-4, Vienna University of Technology}, 15 pages.
}
\author{ Karel Hron }
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
require(MASS)
Sigma <- matrix(c(5.05,4.95,4.95,5.05), ncol=2, byrow=TRUE)
set.seed(123)
z <- mvrnorm(100, mu=c(0,2), Sigma=Sigma)
#require(compositions)
#plot(acomp(z))
plot(invilr(z))
}
\keyword{ math }
