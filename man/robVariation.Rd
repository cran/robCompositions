\name{robVariation}
\alias{robVariation}
\title{ Robust variation matrix }
\description{
  Estimates the variation matrix with robust methods.
}
\usage{
robVariation(x)
}
\arguments{
  \item{x}{ data frame or matrix with positive entries }
}
\details{
  The variation matrix is estimated for a given compositional data set. Instead of using the classical standard deviations the 
  \code{\link[stats]{MAD}} is used.
}
\value{
 The robust variation matrix.
}
\references{ Aitchison, J. (1986) \emph{The Statistical Analysis of Compositional
Data} Monographs on Statistics and Applied Probability. Chapman \&
Hall Ltd., London (UK). 416p. }
\author{ Matthias Templ }
\seealso{ \code{\link[compositions]{variation}} }
\examples{
data(aitchison395)
robVariation(aitchison395)
# require(compositions)
# variation(aitchison395)
}
\keyword{ multivariate }
\keyword{ robust }
