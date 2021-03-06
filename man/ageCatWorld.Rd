% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataSets.R
\docType{data}
\name{ageCatWorld}
\alias{ageCatWorld}
\title{child, middle and eldery population}
\format{
A data frame with 195 rows and 4 variables
}
\usage{
data(ageCatWorld)
}
\description{
Percentages of childs, middle generation and eldery population in 195 countries.
}
\details{
\itemize{
 \item{\code{<15 }}{Percentage of people with age below 15}
 \item{\code{15-60 }}{Percentage of people with age between 15 and 60}
 \item{\code{60+ }}{Percentage of people with age above 60}
 \item{\code{country }}{country of origin}
}

The rows sum up to 100.
}
\examples{

data(ageCatWorld)
str(ageCatWorld)
summary(ageCatWorld)
rowSums(ageCatWorld[, 1:3])
ternaryDiag(ageCatWorld[, 1:3])
plot(pivotCoord(ageCatWorld[, 1:3]))
}
\references{
Fiserova, E. and Hron, K. (2012). Statistical Inference in Orthogonal Regression for Three-Part Compositional Data Using a Linear Model with Type-II Constraints. \emph{Communications in Statistics - Theory and Methods}, 41 (13-14), 2367-2385.
}
\author{
extracted by Karel Hron and Eva Fiserova, implemented by Matthias Templ
}
\keyword{data}
