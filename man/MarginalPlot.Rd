\name{MarginalPlot}
\alias{MarginalPlot}
\title{Plot of a marginal posterior density}
\usage{
MarginalPlot(a_chain, level = 0.95, GridLength = 1024, title = "Characteristics of a date", subtitle = NULL,
                caption = "ArchaeoPhases", x.label = "Calendar year", y.label = NULL, y.grid = TRUE,
                x.scale = "calendar", elapsed.origin.position = NULL, x.min = NULL, x.max = NULL, 
                height = 7, width = 7, units = "in", file = NULL, newWindow=TRUE)
}
\arguments{
  \item{a_chain}{numeric vector containing the output of the MCMC algorithm for a one-parameter. The MCMC samples should be in calendar year (BC/AD).}
  \item{level}{probability corresponding to the level of confidence used for the credible interval}
  \item{GridLength}{the number of equally spaced points at which the density is to be estimated (for density() function)}
  \item{title}{title of the graph}
  \item{subtitle}{subtitle of the graph}
  \item{caption}{caption of the graph}
  \item{x.label}{x axis label of the graph}
  \item{y.label}{y axis label of the graph}
  \item{y.grid}{switch for horizontal grids}
  \item{x.scale}{one of "calendar" for calendar years, "BP" for years before present, or "elapsed" for years after a specified origin}
  \item{elapsed.origin.position}{the position of the column corresponding to the origin for elapsed time calculations}
  \item{x.min}{minimum x axis value}
  \item{x.max}{maximum x axis value}
  \item{height}{height of the graph in units}
  \item{width}{width of the graph in units}
  \item{units}{recognized by ggsave function, one of "in", "cm", "mm"}
  \item{file}{the name of the file to be saved. If NULL then no graph is saved.}
  \item{newWindow}{whether the plot is drawn within a new window or not}
}
\value{
Draws a plot of the estimated marginal posterior density for the one-parameter and adds the mean and the credible interval at the desired level
}
\description{
This function draws the density of a one-parameter and adds summary statistics.
}
\details{
The density is estimated using density() function with n=GridLength.
}
\author{
  Anne Philippe <Anne.Philippe@univ-nantes.fr> and

  Marie-Anne Vibet <Marie-Anne.Vibet@univ-nantes.fr>
}
\examples{
  data(Events); attach(Events)

  MarginalPlot(a_chain = Event.1, level = 0.95)
  MarginalPlot(Event.1, level = 0.50, y.grid = FALSE)

  MarginalPlot(Event.2, level = 0.95, title="Characteristics of Event 2")
}
\keyword{ marginal posterior density }
\keyword{ credible interval }
\keyword{ mean}

