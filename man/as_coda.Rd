% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllGenerics.R, R/coerce.R
\docType{methods}
\name{as_coda}
\alias{as_coda}
\alias{as_coda-method}
\alias{as_coda,MCMC-method}
\title{Coerce to Coda}
\usage{
as_coda(from, ...)

\S4method{as_coda}{MCMC}(from, chains = 1)
}
\arguments{
\item{from}{from An object to be coerced.}

\item{...}{Currently not used.}

\item{chains}{An \code{\link{integer}} specifying the number of parallel chains
(defaults to \eqn{1}).}
}
\value{
An \code{\link[coda:mcmc.list]{coda::mcmc.list}} object.
}
\description{
Extracts parallel chains from an \code{\linkS4class{MCMC}} object to create an
\code{mcmc.list} object for use with \pkg{coda} diagnostic tools.
}
\examples{
if (requireNamespace("coda", quietly = TRUE)) {
  ## Load coda
  library(coda)

  ## Coerce to MCMC
  eve <- as_events(mcmc_events, calendar = CE(), iteration = 1)

  ## Coerce to coda
  mc <- as_coda(eve[, 1:2], chains = 3)
  plot(mc)

  ## Autocorrelation
  autocorr.plot(mc)

  ## Gelman-Rubin diagnostic
  ## The multivariate criterion can not be evaluated when a phase
  ## contains only one date. This induces colinearity problems.
  gelman.diag(mc)
  gelman.plot(mc)
}
}
\seealso{
\code{\link[coda:mcmc]{coda::mcmc()}}, \code{\link[coda:mcmc.list]{coda::mcmc.list()}}

Other read methods: 
\code{\link{as_events}()},
\code{\link{as_phases}()},
\code{\link{check}},
\code{\link{read_bcal}()},
\code{\link{read_chronomodel}},
\code{\link{read_oxcal}()}
}
\author{
A. Philippe, M.-A. Vibet
}
\concept{read methods}
