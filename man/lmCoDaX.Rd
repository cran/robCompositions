\name{lmCoDaX}
\alias{lmCoDaX}
\title{
 Robust regression of non-compositional response on compositional predictors
}
\description{
Delivers appropriate inference for regression of y on a compositional matrix X.
}
\usage{
lmCoDaX(y, X, method = "robust")
}
\arguments{
  \item{y}{
The response which should be non-compositional
}
  \item{X}{
The compositional predictors as a matrix, data.frame or numeric vector
}
  \item{method}{
if robust, lts-regression is applied while with method equals \dQuote{classical} the conventional least squares estimates are applied.
}
}
\details{
Compositional explanatory variables should not be directly used in a linear regression model
because any inference statistic can become misleading. While various approaches for this
problem were proposed, here an approach based on the isometric logratio (ilr) transformation
is used. 
}
\value{
An object of class \sQuote{lts} or \sQuote{lm} and two summary objects.
}
\references{
 Filzmoser, P., Hron, K., Thompsonc, K. (2012)
Linear regression with compositional explanatory variables. \emph{Journal of Applied Statistics}, accepted for publication.
}
\author{
Peter Filzmoser
}
\seealso{
\code{\link{lm}}
}
\examples{
## coming soon
}
\keyword{ models }
