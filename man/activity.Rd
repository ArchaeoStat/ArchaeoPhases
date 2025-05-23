% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllGenerics.R, R/activity.R, R/plot.R
\docType{methods}
\name{activity}
\alias{activity}
\alias{activity-method}
\alias{activity,EventsMCMC-method}
\alias{activity,CumulativeEvents-method}
\alias{plot,ActivityEvents,missing-method}
\title{Activity Plot}
\usage{
activity(object, ...)

\S4method{activity}{EventsMCMC}(
  object,
  from = min(object),
  to = max(object),
  grid = getOption("ArchaeoPhases.grid")
)

\S4method{activity}{CumulativeEvents}(object)

\S4method{plot}{ActivityEvents,missing}(
  x,
  calendar = get_calendar(),
  main = NULL,
  sub = NULL,
  ann = graphics::par("ann"),
  axes = TRUE,
  frame.plot = axes,
  panel.first = NULL,
  panel.last = NULL,
  ...
)
}
\arguments{
\item{object}{An \code{\linkS4class{EventsMCMC}} or a \code{\linkS4class{CumulativeEvents}} object.}

\item{...}{Other \link[graphics:par]{graphical parameters} may also be passed as
arguments to this function, particularly, \code{border}, \code{col}, \code{lwd} or \code{lty}.}

\item{from}{A length-one \code{\link{numeric}} vector giving the earliest date to
estimate for (expressed in \emph{\link[aion:RataDie-class]{rata die}}).}

\item{to}{A length-one \code{\link{numeric}} vector giving the latest date to estimate
for (expressed in \emph{\link[aion:RataDie-class]{rata die}}.}

\item{grid}{A length-one \code{\link{numeric}} vector specifying the number of equally
spaced points of the temporal grid.}

\item{x}{An \code{\linkS4class{ActivityEvents}} object.}

\item{calendar}{A \code{\link[aion:TimeScale-class]{aion::TimeScale}} object specifying the target
calendar (see \code{\link[aion:calendar]{aion::calendar()}}).}

\item{main}{A \code{\link{character}} string giving a main title for the plot.}

\item{sub}{A \code{\link{character}} string giving a subtitle for the plot.}

\item{ann}{A \code{\link{logical}} scalar: should the default annotation (title and x
and y axis labels) appear on the plot?}

\item{axes}{A \code{\link{logical}} scalar: should axes be drawn on the plot?}

\item{frame.plot}{A \code{\link{logical}} scalar: should a box be drawn around the
plot?}

\item{panel.first}{An an \code{expression} to be evaluated after the plot axes are
set up but before any plotting takes place. This can be useful for drawing
background grids.}

\item{panel.last}{An \code{expression} to be evaluated after plotting has taken
place but before the axes, title and box are added.}
}
\value{
\itemize{
\item \code{activity()} returns an \code{\linkS4class{ActivityEvents}} object.
\item \code{plot()} is called it for its side-effects: it results in a graphic being
displayed (invisibly returns \code{x}).
}
}
\description{
Plots the first derivative of the \code{\link{tempo}} plot Bayesian estimate.
}
\examples{
## Coerce to MCMC
eve <- as_events(mcmc_events, calendar = CE(), iteration = 1)
eve <- eve[1:10000, ]

## Tempo plot
tmp <- tempo(eve)
plot(tmp, interval = "credible", panel.first = grid())
plot(tmp, interval = "gauss", panel.first = grid())

## Activity plot
act <- activity(tmp)
plot(act, panel.first = grid())
}
\references{
Dye, T. S. (2016). Long-term rhythms in the development of Hawaiian social
stratification. \emph{Journal of Archaeological Science}, 71: 1-9.
\doi{10.1016/j.jas.2016.05.006}.
}
\seealso{
Other event tools: 
\code{\link{elapse}()},
\code{\link{occurrence}()},
\code{\link{tempo}()}
}
\author{
A. Philippe, M.-A. Vibet, T. S. Dye, N. Frerebeau
}
\concept{event tools}
