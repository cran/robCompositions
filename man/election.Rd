% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataSets.R
\docType{data}
\name{election}
\alias{election}
\title{election data}
\format{
A data frame with 16 observations and 8 variables
}
\source{
German Federal Statistical Office
}
\usage{
data(election)
}
\description{
Results of a election in Germany 2013 in different
federal states
}
\details{
Votes for the political parties
in the elections (compositional variables), and their relation to the unemployment rate
and the average monthly income (external non-compositional variables). Votes are for the Christian Democratic Union and Christian Social Union of Bavaria, also
called The Union (CDU/CSU), Social Democratic Party (SDP), The Left (DIE LINKE),
Alliance '90/The Greens (GRUNE), Free Democratic Party (FDP) and the rest of the
parties participated in the elections (other parties). The votes are examined in absolute
values (number of valid votes). The unemployment in the federal states is reported in
percentages, and the average monthly income in Euros.

\itemize{
 \item{\code{CDU_CSU }}{Christian Democratic Union and Christian Social Union of Bavaria, also
called The Union}
 \item{\code{SDP }}{Social Democratic Party}
 \item{\code{GRUENE }}{Alliance '90/The Greens}
 \item{\code{FDP }}{Free Democratic Party}
 \item{\code{DIE_LINKE }}{The Left}
 \item{\code{other_parties }}{Votes for the rest of the
parties participated in the elections}
 \item{\code{unemployment }}{Unemployment in the federal states in percentages}
 \item{\code{income }}{Average monthly income in Euros}
}
}
\examples{

data(election)
str(election)
}
\references{
Eurostat, \url{https://ec.europa.eu/eurostat/data}
}
\author{
Petra Klynclova, Matthias Templ
}
\keyword{data}
