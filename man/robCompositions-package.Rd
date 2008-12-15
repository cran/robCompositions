\name{robCompositions-package}
\alias{robCompositions-package}
\alias{robCompositions}
\docType{package}
\title{
Robust Estimation for Compositional Data.
}
\description{
This first version of the package includes advanced methods for
        imputation of compositional data including robust methods. The
        package will be enhanced with other multivariate methods for
        compositional data in near future.
}
\details{
\tabular{ll}{
Package: \tab robCompositions\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2008-12-15\cr
License: \tab GPL 2\cr
LazyLoad: \tab yes\cr
}

}
\author{
Peter Filzmoser, Karel Hron, Matthias Templ

Maintainer: Matthias Templ <templ@tuwien.ac.at>
}
\references{
Aitchison, J. (1986) \emph{The Statistical Analysis of Compositional
Data} Monographs on Statistics and Applied Probability. Chapman \&
Hall Ltd., London (UK). 416p. \\

Hron, K. and Templ, M. and Filzmoser, P. (2008) Imputation of missing values for compositional data using classical and robust methods 
\emph{Research Report SM-2008-4, Vienna University of Technology}, 15 pages.
}
\keyword{ package }
\seealso{
\code{\link[<compositions>:<compositions>-package]{<compositions>}} 
}
\examples{
## k nearest neighbor imputation
data(aitchison395)
aitchison395[1,3]
aitchison395[1,3] <- NA
impKNNa(aitchison395)$xImp[1,3]

## iterative model based imputation
data(aitchison395)
x <- aitchison395
x[1,3]
x[1,3] <- NA
xi <- impCoda(x)$xImp
xi[1,3]
s1 <- sum(x[1,-3])
impS <- sum(xi[1,-3])
xi[,3] * s1/impS

xi <- impKNNa(aitchison395)
xi
summary(xi)
plot(xi, which=1)
plot(xi, which=2)
plot(xi, which=3)
}
