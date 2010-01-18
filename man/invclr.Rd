\name{invclr}
\alias{invclr}
\title{
Inverse centred log-ratio transformation
}
\description{
Applies the inverse centred log-ratio transformation.
}
\usage{
invclr(x, useClassInfo = TRUE)
}
\arguments{
  \item{x}{
an object of class \dQuote{clr}, \dQuote{data.frame} or \dQuote{matrix}
}
  \item{useClassInfo}{
if the object is of class clr, the useClassInfo is used to determine if the class information should be used. If yes, also 
absolute values may be preserved.
}
}
\value{
the transformed data set.
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
\code{\link{clr}}, \code{\link{alr}}, \code{\link{ilr}}, \code{\link{invalr}}, \code{\link{invilr}}
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