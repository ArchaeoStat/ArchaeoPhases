\name{app_ArchaeoPhases}
\alias{app_ArchaeoPhases}
\title{Run ArchaeoPhases shiny apps}
\usage{
app_ArchaeoPhases()
}
\arguments{
none
}
\description{
A wrapper for \code{\link{runApp}} to start interactive shiny apps for the R package ArchaeoPhases.
}
\details{
The ArchaeoPhases package provides a function from which a shiny app can be started: \code{app_ArchaeoPhases()}. 
The \code{app_ArchaeoPhases()} function is just a wrapper for \code{\link{runApp}}. 
Via the \code{...} argument further arguments can be directly passed to \code{\link{runApp}}. 
See \code{?shiny::runApp} for further details on valid arguments.
}
\examples{

\dontrun{
app_ArchaeoPhases()

}

}
\author{
  Anne Philippe <Anne.Philippe@univ-nantes.fr> and
  
  Marie-Anne Vibet <Marie-Anne.Vibet@univ-nantes.fr>
}

\seealso{
\code{\link{runApp}}
}

