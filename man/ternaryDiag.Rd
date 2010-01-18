\name{ternaryDiag}
\alias{ternaryDiag}
\title{ Ternary diagram}
\description{
  This plot shows the relative proportions of three variables (compositional parts) in one diagramm. 
  Before plotting, the data are scaled.
}
\usage{
ternaryDiag(x, name = colnames(x), grid = TRUE, gridCol=grey(0.6), ...)
}
\arguments{
  \item{x}{ matrix or data.frame with 3 columns }
  \item{name}{ names of the variables }
  \item{grid}{ if TRUE the grid is plotted }
  \item{gridCol}{ color for the grid lines }
  \item{\dots}{ further parameters, see, e.g., \code{par()} }
}
\details{
The relative proportions of each variable are plotted.
}
\references{ C. Reimann, P. Filzmoser, R.G. Garrett, and R. Dutter: Statistical Data Analysis
   Explained. Applied Environmental Statistics with R. John Wiley and Sons, Chichester, 2008. }
\author{ Peter Filzmoser <\email{P.Filzmoser@tuwien.ac.at}>
\url{http://www.statistik.tuwien.ac.at/public/filz/}, Matthias Templ
}
\seealso{
\code{\link[StatDA]{ternary}}
}
\examples{
data(arcticLake)
ternaryDiag(arcticLake)
}
\keyword{ multivariate }
\keyword{ aplot }