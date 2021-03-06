% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coord.R
\name{coord}
\alias{coord}
\alias{print.coord}
\title{Coordinate representation of compositional tables}
\usage{
coord(x, SBPr, SBPc)

\method{print}{coord}(x, ...)
}
\arguments{
\item{x}{an object of class \dQuote{table}, \dQuote{data.frame} or \dQuote{matrix}}

\item{SBPr}{sequential binary partition for rows}

\item{SBPc}{sequential binary partition for columns}

\item{...}{further arguments passed to the print function}
}
\value{
Row and column balances and odds ratios as coordinate representations of the independence and interaction tables, respectively.
\item{row_balances}{row balances}
\item{row_bin}{binary partition for rows}
\item{col_balances}{column balances}
\item{col_bin}{binary parition for columns}
\item{odds_ratios_coord}{odds ratio coordinates}
}
\description{
General approach to orthonormal coordinates for compositional tables
}
\details{
A contingency or propability table can be considered as a two-factor composition, we refer to compositional tables. 
This function constructs orthonomal coordinates for compositional tables using
the balances approach for given sequential binary partitions on rows and columns of the compositional table.
}
\examples{
x <- rbind(c(1,5,3,6,8,4),c(6,4,9,5,8,12),c(15,2,68,42,11,6),
           c(20,15,4,6,23,8),c(11,20,35,26,44,8))
x
SBPc <- rbind(c(1,1,1,1,-1,-1),c(1,-1,-1,-1,0,0),c(0,1,1,-1,0,0),
              c(0,1,-1,0,0,0),c(0,0,0,0,1,-1))
SBPc
SBPr <- rbind(c(1,1,1,-1,-1),c(1,1,-1,0,0),c(1,-1,0,0,0),c(0,0,0,1,-1))
SBPr
result <- coord(x, SBPr,SBPc)
result
data(socExp)

}
\references{
Facevicova, K., Hron, K., Todorov, V., Templ, M. (2018)
General approach to coordinate representation of compositional tables. 
\emph{Scandinavian Journal of Statistics}, 45(4), 879-899.
}
\author{
Kamila Facevicova, and minor adaption by Matthias Templ
}
