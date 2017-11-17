\name{KAPhasesChronoModel}
\alias{KAPhasesChronoModel}
\docType{data}
\title{
 Ksar Akil chronology build with ChronoModel software. Phases part.
}
\description{
MCMC samples from the posterior distribution of dates included in the Ksar Akil chronology build with ChronoModel software. The MCMC samples are in calendar year (BC/AD).
}
\usage{data("KAPhasesChronoModel")}

\format{
A data frame with 30000 observations on the following 9 variables, two variables for each group of dates and the MCMC iteration number. These four phases are in stratigraphic order, the order from the oldest to the youngest phase being : IUP, Ahmarian, UP and EPI.These observations come from 3 parallel chains. 
  \describe{
    \item{\code{iter}}{a numeric vector corresponding to iteration number}
    \item{\code{EPI.alpha}}{a numeric vector containing the output of the MCMC algorithm for the minimum of the phase "EPI". }
    \item{\code{EPI.beta}}{a numeric vector containing the output of the MCMC algorithm for the maximum of the phase "EPI". }
    \item{\code{UP.alpha}}{a numeric vector containing the output of the MCMC algorithm for the the minimum of the phase "UP". }
    \item{\code{UP.beta}}{a numeric vector containing the output of the MCMC algorithm for the maximum of the phase "UP".}
    \item{\code{Ahmarian.alpha}}{a numeric vector containing the output of the MCMC algorithm for the the minimum of the phase "Ahmarian". }
    \item{\code{Ahmarian.beta}}{a numeric vector containing the output of the MCMC algorithm for the maximum of the phase "Ahmarian".}
    \item{\code{IUP.alpha}}{a numeric vector containing the output of the MCMC algorithm for the minimum of the phase "IUP". }
    \item{\code{IUP.beta}}{a numeric vector containing the output of the MCMC algorithm for the maximum of the phase "IUP". }
  } 
}

\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
Bosch, M. et al. (2015) New chronology for Ksar Akil (Lebanon) supports Levantine route of modern human dispersal into Europe. Proceedings of the National Academy of Sciences, 112, 7683--6.
}
\references{
Lanos, P. et al. (2016) Chronomodel  : Chronological Modelling of Archaeological Data using Bayesian Statistics (Version 1.5). www.chronomodel.fr

Vibet. M.-A. et al. (2016) ChronoModel V1.5 User's manual. 
}
\examples{
##load data
data(KAPhasesChronoModel)
}

\keyword{datasets}
\keyword{MCMC output}
\keyword{ChronoModel}

\author{
Anne Philippe <Anne.Philippe@univ-nantes.fr> and

Marie-Anne Vibet <Marie-Anne.Vibet@univ-nantes.fr>
}