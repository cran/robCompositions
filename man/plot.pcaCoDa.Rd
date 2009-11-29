\name{plot.pcaCoDa}
\alias{plot.pcaCoDa}
\title{ Plot method }
\description{
Provides robust compositional biplots.
}
\usage{
plot.pcaCoDa(x, y, ...)
}
\arguments{
  \item{x}{ object of class \sQuote{pcaCoDa} }
  \item{y}{ ... }
  \item{\dots}{ ... }
}
\details{
The robust compositional biplot according to Aitchison and Greenacre (2002), computed from resulting (robust) loadings and scores, 
is performed.
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}
\references{ 
 Aitchison, J. and Greenacre, M. (2002). Biplots of compositional data. \emph{Applied Statistics}, \bold{51}, 375-392. \\

 Filzmoser, P., Hron, K., Reimann, C. (2009)
 Principal Component Analysis for Compositional Data with Outliers. \emph{Environmetrics}, accepted for publication.
 }
\author{ M. Templ, K. Hron }
\seealso{ \code{\link{pcaCoDa}}}
\examples{
data(expenditures)
p1 <- pcaCoDa(expenditures)
p1
plot(p1)
}
\keyword{ aplot }
