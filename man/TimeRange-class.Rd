% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllClasses.R
\docType{class}
\name{TimeRange-class}
\alias{TimeRange-class}
\alias{.TimeRange}
\title{Time Range}
\description{
An S4 class to represent time ranges.
}
\section{Slots}{

\describe{
\item{\code{start,end}}{A \code{numeric} \code{\link{matrix}} giving the lower and upper
boundaries expressed in \emph{\link[aion:RataDie-class]{rata die}}.}

\item{\code{labels}}{A \code{\link{character}} vector specifying the name of the events/phases.}

\item{\code{hash}}{A \code{\link{character}} string giving the 32-byte MD5 hash of the
original data file.}
}}

\section{Coerce}{

In the code snippets below, \code{x} is a \code{CumulativeEvents} object.
\describe{
\item{\code{as.data.frame(x)}}{Coerces to a \code{\link{data.frame}}.}
}
}

\seealso{
Other classes: 
\code{\link{ActivityEvents-class}},
\code{\link{AgeDepthModel-class}},
\code{\link{CumulativeEvents-class}},
\code{\link{DurationsMCMC-class}},
\code{\link{EventsMCMC-class}},
\code{\link{MCMC-class}},
\code{\link{OccurrenceEvents-class}},
\code{\link{PhasesMCMC-class}}
}
\author{
N. Frerebeau
}
\concept{classes}
\keyword{internal}
