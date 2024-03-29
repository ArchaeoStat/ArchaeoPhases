% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllGenerics.R, R/sensitivity.R
\docType{methods}
\name{sensitivity}
\alias{sensitivity}
\alias{sensitivity-method}
\alias{sensitivity,EventsMCMC-method}
\title{Sensitivity}
\usage{
sensitivity(...)

\S4method{sensitivity}{EventsMCMC}(..., positions = NULL, level = 0.95)
}
\arguments{
\item{...}{Any \code{\linkS4class{EventsMCMC}} object.}

\item{positions}{A \code{\link{numeric}} vector specifying the positions of the columns
corresponding to the MCMC chains of interest, or a \code{\link{character}} vector of
column names.}

\item{level}{A length-one \code{\link{numeric}} vector giving the confidence level.}
}
\value{
A \code{\link{data.frame}}.
}
\description{
Calculates the ranges of summary statistics from the output of two or more
runs of the MCMC algorithm.
}
\details{
This function is useful for estimating the sensitivity of calibration results
to different model parameters.
}
\examples{
## Coerce to MCMC
eve <- as_events(mcmc_events, calendar = CE(), iteration = 1)

## Returns 0's
sensitivity(eve, eve)
}
\seealso{
\code{\link[=summary]{summary()}}

Other statistics: 
\code{\link{interval_credible}()},
\code{\link{interval_hdr}()},
\code{\link{summary}()}
}
\author{
T. S. Dye, N. Frerebeau
}
\concept{statistics}
