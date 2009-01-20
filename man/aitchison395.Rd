\name{aitchison395}
\alias{aitchison395}
\docType{data}
\title{ Household expenditures data }
\description{
  This data set from Aitchison (1986), p. 395, describes household expenditures (in former Hong Kong dollars) on five commundity groups. 
}
\usage{data(aitchison395)}
\format{
  A data frame with 20 observations on the following 5 variables.
  \describe{
    \item{\code{V1}}{housing (including fuel and light)}
    \item{\code{V2}}{foodstuff}
    \item{\code{V3}}{alcohol}
    \item{\code{V4}}{tobacco}
    \item{\code{V5}}{ other goods (including clothing, footwear and durable goods)}
  }
}
\details{
  This data set contains household expenditures on five commodity groups of 20 single men. The
variables represent housing (including fuel and light), foodstuff, alcohol and tobacco, other goods (including
clothing, footwear and durable goods) and services (including transport and vehicles). Thus they represent
the ratios of the men's income spent on the mentioned expenditures. 
}
\source{
Aitchison, J. (1986) \emph{The Statistical Analysis of Compositional
Data} Monographs on Statistics and Applied Probability. Chapman \&
Hall Ltd., London (UK). 416p.
}
\examples{
data(aitchison395)
aitchison395[1,3]
aitchison395[1,3] <- NA
impKNNa(aitchison395)$xImp[1,3]
}
\keyword{datasets}
