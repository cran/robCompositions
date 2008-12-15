\name{aDist}
\alias{aDist}
\title{Aitchison distance}
\description{
  Computes the Aitchison distance between two observations.
}
\usage{
aDist(x, y)
}
\arguments{
  \item{x}{ a vector }
  \item{y}{ a vector }
}
\details{
  This distance measure accounts for the relative scale property of the 
  Aitchison distance, and measures the distance between two compositions. 
  It is not designed to apply it on one whole matrix, 
  such as function \sQuote{acomp()} in package \sQuote{compositions}, 
  but is suitable to compare different matrices.
}
\value{
  The Aitchison distance between two compositions.
}
\references{ Aitchison, J. (1986) \emph{The Statistical Analysis of Compositional
Data} Monographs on Statistics and Applied Probability. Chapman \&
Hall Ltd., London (UK). 416p. }
\author{ Matthias Templ }
\seealso{ \code{\link{ilr}} }
\examples{
data(aitchison395)
x <- xOrig <- aitchison395
## Aitchison distance between the first 2 observations:
aDist(x[,1], x[,2])

## set some missing values:
x[1,3] <- x[3,5] <- x[2,4] <- x[5,3] <- x[8,3] <- NA

## impute them:
xImp <- impCoda(x, method="ltsReg")$xImp

## calculate the relative Aitchsion distance between xOrig and xImp:
das <- 0
for(i in 1:nrow(xOrig)){
  das <- das + aDist(x=xOrig[i,], y=xImp[i,])
}
das
}
\keyword{ math }
\keyword{ arith }
