% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Transformations.R
\name{fcenLR}
\alias{fcenLR}
\title{fcenLR transformation (functional)}
\usage{
fcenLR(z, z_step, density)
}
\arguments{
\item{z}{grid of points defining the abscissa}

\item{z_step}{step of the grid of the abscissa}

\item{density}{grid evaluation of the lambda-density}
}
\value{
\item{\code{out}}{grid evaluation of the lambda-density in L^2(lambda)}
}
\description{
fcenLR[lambda] transformation: mapping from B^2(lambda) into L^2(lambda)
}
\examples{
# Example (normal density)
t = seq(-4.7,4.7, length = 1000)
t_step = diff(t[1:2])

mean = 0; sd = 1.5
f = dnorm(t, mean, sd)
f1 = f/trapzc(t_step,f)

f.fcenLR = fcenLR(t,t_step,f) 
f.fcenLRinv = fcenLRinv(t.fine,t_step,f.fcenLR)

plot(t,f.fcenLR, type="l",las=1, ylab="fcenLR(density)", 
  cex.lab=1.2,cex.axis=1.2, col="darkblue",lwd=2)
abline(h=0, col="red")

plot(t,f.fcenLRinv, type="l",las=1, 
  ylab="density",cex.lab=1.2,cex.axis=1.2, col="darkblue",lwd=2,lty=1)
lines(t,f1,lty=2,lwd=2,col="gold")   
}
\references{
Talska, R., Menafoglio, A., Hron, K., Egozcue, J. J., Palarea-Albaladejo, J. (2020). Weighting the domain of probability densities in functional data analysis.\emph{Stat}(2020). https://doi.org/10.1002/sta4.283
}
\author{
R. Talska\email{talskarenata@seznam.cz}, A. Menafoglio, K. Hron\email{karel.hron@upol.cz}, J. J. Egozcue, J. Palarea-Albaladejo
}
