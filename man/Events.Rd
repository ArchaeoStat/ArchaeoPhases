\name{Events}
\alias{Events}
\docType{data}
\title{
Events
}
\description{
Contains the output of the MCMC algorithm for four events modelled by ChronoModel. The MCMC samples are in calendar year (BC/AD). }
\usage{data(Events)}
\format{
  A data frame with 30000 observations on the following 5 variables.
  \describe{
    \item{\code{iter}}{a numeric vector corresponding to iteration number}
    \item{\code{Event.1}}{a numeric vector containing the output of the MCMC algorithm for the parameter Event 1 }
    \item{\code{Event.12}}{a numeric vector containing the output of the MCMC algorithm for the parameter Event 12 }
    \item{\code{Event.2}}{a numeric vector containing the output of the MCMC algorithm for the parameter Event 2 }
    \item{\code{Event.22}}{a numeric vector containing the output of the MCMC algorithm for the parameter Event 22}
  }
}
\examples{
data(Events)
}
\keyword{datasets}
\keyword{MCMC output}
\keyword{ChronoModel}

\author{
Anne Philippe <Anne.Philippe@univ-nantes.fr> and

Marie-Anne Vibet <Marie-Anne.Vibet@univ-nantes.fr>
}

