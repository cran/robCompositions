\name{adtestWrapper}
\alias{adtestWrapper}
\title{ Wrapper for Anderson-Darling tests }
\description{
A set of Anderson-Darling tests (Anderson and Darling, 1952) are applied as proposed by Aitchison
(Aichison, 1986). 
}
\usage{
adtestWrapper(x, alpha = 0.05, R = 1000, robustEst = FALSE)
}
\arguments{
  \item{x}{ compositional data of class data.frame or matrix }
  \item{alpha}{ significance level }
  \item{R}{ Number of Monte Carlo simulations in order to provide p-values. }
  \item{robustEst}{ logical }
}
\details{
First, the data is tranformed using the \sQuote{ilr}-transformation. 
After applying this tranformation 

- all (D-1)-dimensional marginal, univarite distributions are tested using the univariate 
Anderson-Darling Test for normality.

- all 0.5 (D-1)(D-2)-dimensional bivariate angle distributions are tested using the Anderson-Darling
angle Test for normality.

- the (D-1)-dimensional radius distribution is tested using
the Anderson-Darling radius Test for normality. 
}
\value{
  \item{res }{ a list including each test result }
  \item{check }{ information about the rejection of the Null hypothesis}
  \item{alpha}{ the underlying significance level }
  \item{info}{ further information which is used by the print and summary method. }
  \item{est}{ standard for standard estimations and robust for robust estimation }  
}
\references{ 
Anderson, T.W. and Darling, D.A. (1952) 
  \emph{Asymptotic theory of certain goodness-of-fit criteria based 
  on stochastic processes} Annals of Mathematical Statistics, \bold{23}  
  193-212. 
  
 Aitchison, J. (1986) \emph{The Statistical Analysis of Compositional
Data} Monographs on Statistics and Applied Probability. Chapman \&
Hall Ltd., London (UK). 416p.
 }
\author{ Matthias Templ and Karel Hron }
\seealso{ \code{\link{adtest}}, \code{\link{ilr}} }
\examples{
data(machineOperators)
a <- adtestWrapper(machineOperators, R=50) # choose higher value of R
a
summary(a)
}
\keyword{ htest }