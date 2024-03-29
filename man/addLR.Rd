% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/addLR.R
\name{addLR}
\alias{addLR}
\title{Additive logratio coordinates}
\usage{
addLR(x, ivar = ncol(x), base = exp(1))
}
\arguments{
\item{x}{D-part compositional data}

\item{ivar}{Rationing part}

\item{base}{a positive or complex number: 
the base with respect to which logarithms are computed. Defaults to \code{exp(1)}.}
}
\value{
A list of class \dQuote{alr} which includes the following content:
\item{x.alr}{the resulting coordinates} \item{varx}{the rationing variable}
\item{ivar}{the index of the rationing variable, indicating the column
number of the rationing variable in the data matrix \emph{x}}
\item{cnames}{the column names of \emph{x}} The additional information such
as \emph{cnames} or \emph{ivar} is useful when an inverse mapping is
applied on the \sQuote{same} data set.
}
\description{
The additive logratio coordinates map D-part compositional data from
the simplex into a (D-1)-dimensional real space.
}
\details{
The compositional parts are divided by the rationing part before the
logarithm is taken.
}
\examples{

data(arcticLake)
x <- arcticLake
x.alr <- addLR(x, 2)
y <- addLRinv(x.alr)
## This exactly fulfills:
addLRinv(addLR(x, 3))
data(expenditures)
x <- expenditures
y <- addLRinv(addLR(x, 5))
head(x)
head(y)
## --> absolute values are preserved as well.

## preserve only the ratios:
addLRinv(x.alr, ivar=2, useClassInfo=FALSE)


}
\references{
Aitchison, J. (1986) \emph{The Statistical Analysis of
Compositional Data} Monographs on Statistics and Applied Probability.
Chapman and Hall Ltd., London (UK). 416p.
}
\seealso{
\code{\link{addLRinv}}, \code{\link{pivotCoord}}
}
\author{
Matthias Templ
}
\keyword{manip}
