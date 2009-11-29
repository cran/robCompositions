\name{alr}
\alias{alr}
\title{
additive log-ratio transformation
}
\description{
The alr transformation opens compositional data into an unconstrained
form in the real space.
}
\usage{
alr(x, ivar=ncol(x))
}
\arguments{
  \item{x}{Multivariate compositional data}
  \item{ivar}{Rationing variable}
}
\details{
The variables divided by its rationing variable before the logarithm is taken. This results in an D-1 dimensional space, with D the dimension of the 
data matrix \emph{x}.
}
\value{
A list of class \dQuote{alr} which includes the following content:
\item{x.alr}{the transformed data}
\item{varx}{the rationing variable}
\item{ivar}{the index of the rationing variable, indicating the column number of the rationing variable in the data matrix \emph{x}}
\item{cnames}{the column names of \emph{x}}
The additional information such as \emph{cnames} or \emph{ivar} is usefull when a back-transformation is applied on the \sQuote{same} data set.
}
\references{
Aitchison, J. (1986) \emph{The Statistical Analysis of Compositional
Data} Monographs on Statistics and Applied Probability. Chapman \&
Hall Ltd., London (UK). 416p.
}
\author{
Matthias Templ
}
\seealso{
\code{\link{ilr}}
}
\examples{
data(arcticLake)
x <- arcticLake
x.alr <- alr(x, 2)
y <- invalr(x.alr)
## This exactly fulfills:
invalr(alr(x, 3))
data(expenditures)
x <- expenditures
y <- invalr(alr(x, 5))
head(x)
head(y)
## --> absolute values are preserved as well.

## preserve only the ratios:
invalr(x.alr, ivar=2, useClassInfo=FALSE)




}
\keyword{ manip }