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
\item{\code{hash}}{A \code{\link{character}} string giving the 32-byte MD5 hash of the
original data file.}
}}

\note{
This class inherits from \code{\link[aion:TimeIntervals-class]{aion::TimeIntervals}}.
}
\section{Coerce}{

In the code snippets below, \code{x} is a \code{TimeRange} object.
\describe{
\item{\code{as.data.frame(x)}}{Coerces to a \code{\link{data.frame}}.}
}
}

\section{Plot}{

In the code snippets below, \code{x} is a \code{TimeRange} object.
\describe{
\item{\code{plot(x)}}{Results in a graphic being displayed
(invisibly returns \code{x}).}
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
