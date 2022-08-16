% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllGenerics.R, R/bind.R
\docType{methods}
\name{sort}
\alias{sort}
\alias{sort,MCMC-method}
\title{Sort an MCMC Object}
\usage{
\S4method{sort}{MCMC}(x, decreasing = FALSE)
}
\arguments{
\item{x}{An \code{\linkS4class{MCMC}} object.}

\item{decreasing}{A \code{\link{logical}} scalar: should the sort order be decreasing?}
}
\value{
An object of the same sort as \code{x}.
}
\description{
Sort (or order) an object into ascending or descending temporal order.
}
\examples{
## Events
(eve <- as_events(events, calendar = "CE", iteration = 1))

eve[1:1000, ] # Select the first 1000 iterations
eve[, 1:2]    # Select the first 2 events

cbind2(eve[, 1:2], eve[, 3:4]) # Combine two MCMC objects
sort(eve, decreasing = TRUE)   # Sort events in descending order

## Phases
(pha <- as_phases(phases, start = c(1, 3), calendar = "CE", iteration = 1))

pha[1:1000, , ]          # Select the first 1000 iterations
pha[, 1, , drop = FALSE] # Select the first phase
}
\seealso{
Other mutators: 
\code{\link{bind}},
\code{\link{mutator}},
\code{\link{subset}()}
}
\author{
N. Frerebeau
}
\concept{mutators}