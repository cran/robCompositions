\name{aitchison359}
\alias{aitchison359}
\docType{data}
\title{ Artic lake sediment data }
\description{
  Sand, silt, clay compositions of 39 sediment samples at different water depths in an Arctic lake.
  This data set can be found on page 359 of the Aitchison book (see reference).
}
\usage{data(aitchison359)}
\format{
  A data frame with 39 observations on the following 3 variables.
  \describe{
    \item{\code{V1}}{numeric vector of percentages of sand}
    \item{\code{V2}}{numeric vector of percentages of silt}
    \item{\code{V3}}{numeric vector of percentages of clay}
  }
}
\details{
 Please note, that the original data set consists of four variables. However, usually the first three are mainly used in applications.
}
\source{
Aitchison, J. (1986) \emph{The Statistical Analysis of Compositional
Data} Monographs on Statistics and Applied Probability. Chapman \&
Hall Ltd., London (UK). 416p.
}
\examples{
data(aitchison359)
}
\keyword{datasets}
