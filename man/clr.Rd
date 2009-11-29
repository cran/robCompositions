\name{clr}
\alias{clr}
\title{
centered log-ratio transformation
}
\description{
The clr transformation opens compositional data into an unconstrained
form in the real space.
}
\usage{
clr(x)
}
\arguments{
  \item{x}{
multivariate data ideally of class data.frame or matrix
}
}
\details{
The compositions divided by its geometric mean before the logarithm is taken. 
}
\value{
The transformed data, including
\item{x.clr}{clr transformed data}
\item{gm}{the geometric means of the original composition.}
}
\references{
Aitchison, J. (1986) \emph{The Statistical Analysis of Compositional
Data} Monographs on Statistics and Applied Probability. Chapman \&
Hall Ltd., London (UK). 416p.
}
\author{
Matthias Templ
}
\note{
The resulting transformed data set is singular by definition.
}
\seealso{
\code{\link{invclr}}, \code{\link{alr}}, \code{\link{ilr}}, \code{\link{invalr}}, \code{\link{invilr}}
}
\examples{
data(expenditures)
eclr <- clr(expenditures)
inveclr <- invclr(eclr)
head(expenditures)
head(inveclr)
head(invclr(eclr$x.clr))
}
\keyword{ manip }