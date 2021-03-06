% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataSets.R
\docType{data}
\name{electionATbp}
\alias{electionATbp}
\title{Austrian presidential election data}
\format{
A data frame with 2202 observations and 10 variables
}
\source{
OpenData Austria, \url{https://www.data.gv.at/}
}
\usage{
data(electionATbp)
}
\description{
Results the Austrian presidential election in October 2016.
}
\details{
Votes for the candidates Hofer and Van der Bellen.

\itemize{
 \item{\code{GKZ }}{Community code}
 \item{\code{Name }}{Name of the community}
 \item{\code{Eligible }}{eligible votes}
 \item{\code{Votes_total }}{total votes}
 \item{\code{Votes_invalid }}{invalid votes}
 \item{\code{Votes_valid }}{valid votes}
 \item{\code{Hofer_total }}{votes for Hofer}
 \item{\code{Hofer_perc }}{votes for Hofer in percentages}
 \item{\code{VanderBellen_total }}{votes for Van der Bellen}
 \item{\code{VanderBellen_perc }}{votes for Van der Bellen in percentages}
}
}
\examples{

data(electionATbp)
str(electionATbp)
}
\author{
Peter Filzmoser
}
\keyword{data}
