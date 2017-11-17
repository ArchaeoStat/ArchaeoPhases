\name{Phases}
\alias{Phases}
\docType{data}
\title{
Phases}
\description{
Contains the output of the MCMC algorithm for all the phases (beginning and end) of two successive phases modelled in ChronoModel.
Phase 1 is assued to be older than Phase 2. The MCMC samples are in calendar year (BC/AD).
}
\usage{data(Phases)}
\format{
  A data frame with 30000 observations on the following 5 variables.
  \describe{
    \item{\code{iter}}{a numeric vector corresponding to iteration number}
    \item{\code{Phase.1.alpha}}{a numeric vector containing the output of the MCMC algorithm for the beginning of the phase "Phase 1" }
    \item{\code{Phase.1.beta}}{a numeric vector containing the output of the MCMC algorithm for the end of the phase "Phase 1" }
    \item{\code{Phase.2.alpha}}{a numeric vector containing the output of the MCMC algorithm for the the beginning of the phase "Phase 2" }
    \item{\code{Phase.2.beta}}{a numeric vector containing the output of the MCMC algorithm for the end of the phase "Phase 2"}
  }
}
\examples{
  data(Phases)
  attach(Phases)
  PhasePlot(Phase.1.alpha, Phase.1.beta)
  PhaseTimeRange(Phase.1.alpha, Phase.1.beta)

  PhasesGap(Phase.1.beta, Phase.2.alpha)
  PhasesTransition(Phase.1.beta, Phase.2.alpha)
}
\author{
Anne Philippe <Anne.Philippe@univ-nantes.fr> and

Marie-Anne Vibet <Marie-Anne.Vibet@univ-nantes.fr>
}
\keyword{datasets}
\keyword{MCMC output}
\keyword{ChronoModel}
