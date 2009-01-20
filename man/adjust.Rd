\name{adjust}
\alias{adjust}
\title{ Adjusting for original scale }
\description{
  Results from the model based iterative methods provides the results in another scale (but the 
  ratios are still the same). This function rescale the output to the original scale.
}
\usage{
adjust(x)
}
\arguments{
  \item{x}{ object from class \sQuote{imp} }
}
\details{
It is self-explaining if you try the examples.
}
\value{
The object of class \sQuote{imp} but with the adjusted imputed data.
}
\references{ 
Hron, K. and Templ, M. and Filzmoser, P. (2008) Imputation of missing values for compositional data using classical and robust methods 
\emph{Research Report SM-2008-4, Vienna University of Technology}, 15 pages.
 }
\author{ Matthias Templ }
\seealso{ \code{\link{impCoda}} }
\examples{
data(aitchison395)
x <- aitchison395
x[1,3] <- x[2,4] <- x[3,3] <- x[3,4] <- NA
xi <- impCoda(x)
x
xi$xImp
adjust(xi)$xImp
}
\keyword{ manip }