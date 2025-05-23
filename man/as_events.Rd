% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllGenerics.R, R/events.R
\docType{methods}
\name{as_events}
\alias{as_events}
\alias{as_events-method}
\alias{as_events,matrix-method}
\alias{as_events,data.frame-method}
\title{Coerce to Events}
\usage{
as_events(from, ...)

\S4method{as_events}{matrix}(from, calendar, iteration = NULL)

\S4method{as_events}{data.frame}(from, calendar, iteration = NULL)
}
\arguments{
\item{from}{from An object to be coerced.}

\item{...}{Currently not used.}

\item{calendar}{A \code{\link[aion:TimeScale-class]{aion::TimeScale}} object specifying the source
calendar (see \code{\link[aion:calendar]{aion::calendar()}}).}

\item{iteration}{A length-one \code{\link{numeric}} vector specifying the index of the
iteration column.}
}
\value{
An \code{\linkS4class{EventsMCMC}} object.
}
\description{
Coerce to Events
}
\examples{
## Coerce to events
eve <- as_events(mcmc_events, calendar = CE(), iteration = 1)

## Plot first event
plot(eve[, 1], interval = "hdr")

## Colorfull plot
plot(eve, col.density = c("#4477AA", "#EE6677", "#228833", "#CCBB44"))

## Plot events
plot(eve, calendar = CE(), interval = "credible", level = 0.68)
plot(eve, calendar = BP(), interval = "hdr", level = 0.68)

## Plot only 95\% credible interval
plot(eve, density = FALSE, interval = "credible", lwd = 3, tcl = 0)
}
\seealso{
Other read methods: 
\code{\link{as_coda}()},
\code{\link{as_phases}()},
\code{\link{check}},
\code{\link{read_bcal}()},
\code{\link{read_chronomodel}},
\code{\link{read_oxcal}()}
}
\author{
A. Philippe, M.-A. Vibet, N. Frerebeau
}
\concept{read methods}
