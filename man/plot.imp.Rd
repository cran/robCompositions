% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.imp.R
\name{plot.imp}
\alias{plot.imp}
\title{Plot method for objects of class imp}
\usage{
\method{plot}{imp}(
  x,
  ...,
  which = 1,
  ord = 1:ncol(x),
  colcomb = "missnonmiss",
  plotvars = NULL,
  col = c("skyblue", "red"),
  alpha = NULL,
  lty = par("lty"),
  xaxt = "s",
  xaxlabels = NULL,
  las = 3,
  interactive = TRUE,
  pch = c(1, 3),
  ask = prod(par("mfcol")) < length(which) && dev.interactive(),
  center = FALSE,
  scale = FALSE,
  id = FALSE,
  seg.l = 0.02,
  seg1 = TRUE
)
}
\arguments{
\item{x}{object of class \sQuote{imp}}

\item{\dots}{other parameters to be passed through to plotting functions.}

\item{which}{if a subset of the plots is required, specify a subset of the
numbers 1:3.}

\item{ord}{determines the ordering of the variables}

\item{colcomb}{if colcomb\eqn{=}\dQuote{missnonmiss}, observations with
missings in any variable are highlighted. Otherwise, observations with
missings in any of the variables specified by colcomb are highlighted in the
parallel coordinate plot.}

\item{plotvars}{Parameter for the parallel coordinate plot. A vector giving
the variables to be plotted.  If NULL (the default), all variables are
plotted.}

\item{col}{a vector of length two giving the colors to be used in the plot.
The second color will be used for highlighting.}

\item{alpha}{a numeric value between 0 and 1 giving the level of
transparency of the colors, or NULL. This can be used to prevent
overplotting.}

\item{lty}{a vector of length two giving the line types.  The second line
type will be used for the highlighted observations.  If a single value is
supplied, it will be used for both non-highlighted and highlighted
observations.}

\item{xaxt}{the x-axis type (see \code{\link{par}}).}

\item{xaxlabels}{a character vector containing the labels for the x-axis.
If NULL, the column names of x will be used.}

\item{las}{the style of axis labels (see \code{\link{par}}).}

\item{interactive}{a logical indicating whether the variables to be used for
highlighting can be selected interactively (see \sQuote{Details}).}

\item{pch}{a vector of length two giving the symbol of the plotting points.
The symbol will be used for the highlighted observations.  If a single value
is supplied, it will be used for both non-highlighted and highlighted
observations.}

\item{ask}{logical; if TRUE, the user is asked before each plot, see
\code{\link{par}}(ask=.).}

\item{center}{logical, indicates if the data should be centered prior
plotting the ternary plot.}

\item{scale}{logical, indicates if the data should be centered prior
plotting the ternary plot.}

\item{id}{reads the position of the graphics pointer when the (first) mouse
button is pressed and returns the corresponding index of the observation.
(only used by the ternary plot)}

\item{seg.l}{length of the plotting symbol (spikes) for the ternary plot.}

\item{seg1}{if TRUE, the spikes of the plotting symbol are justified.}
}
\value{
None (invisible NULL).
}
\description{
This function provides several diagnostic plots for the imputed data set in
order to see how the imputated values are distributed in comparison with the
original data values.
}
\details{
The first plot (which \eqn{== 1}) is a multiple scatterplot where for the
imputed values another plot symbol and color is used in order to highlight
them. Currently, the ggpairs functions from the GGally package is used.

Plot 2 is a parallel coordinate plot in which imputed values in certain
variables are highlighted.  In parallel coordinate plots, the variables are
represented by parallel axes.  Each observation of the scaled data is shown
as a line.  If interactive is TRUE, the variables to be used for
highlighting can be selected interactively. Observations which includes
imputed values in any of the selected variables will be highlighted.  A
variable can be added to the selection by clicking on a coordinate axis.  If
a variable is already selected, clicking on its coordinate axis will remove
it from the selection. Clicking anywhere outside the plot region quits the
interactive session.

Plot 3 shows a ternary diagram in which imputed values are highlighted, i.e.
those spikes of the chosen plotting symbol are colored in red for which of
the values are missing in the unimputed data set.
}
\examples{

data(expenditures)
expenditures[1,3]
expenditures[1,3] <- NA
xi <- impKNNa(expenditures)
xi
summary(xi)
\dontrun{plot(xi, which=1)}
plot(xi, which=2)
plot(xi, which=3)
plot(xi, which=3, seg1=FALSE)

}
\references{
Aitchison, J. (1986) \emph{The Statistical Analysis of
Compositional Data} Monographs on Statistics and Applied Probability.
Chapman and Hall Ltd., London (UK). 416p.

Wegman, E. J. (1990) \emph{Hyperdimensional data analysis using parallel
coordinates} Journal of the American Statistical Association 85, 664--675.
}
\seealso{
\code{\link{impCoda}}, \code{\link{impKNNa}}
}
\author{
Matthias Templ
}
\keyword{aplot}
\keyword{hplot}
