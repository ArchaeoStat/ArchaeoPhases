\name{MultiMarginalPlot}
\alias{MultiMarginalPlot}
\title{Plot of marginal posterior densities of several dates}
\usage{
MultiMarginalPlot(data, position, level = 0.95, GridLength = 1024, 
    x.scale = rep("calendar", length(position)), elapsed.origin = NULL,
    title="Characteristics of several dates", subtitle = NULL,caption = "ArchaeoPhases",
    x.label = "Calendar year",y.label = NULL,y.grid = TRUE,x.min = NULL, x.max = NULL, 
    legend.title = "Legend", height = 7, width = 7, units = "in",
    file = NULL, newWindow=TRUE)
}
\arguments{
  \item{data}{dataframe containing the output of the MCMC algorithm. The MCMC samples should be in calendar year (BC/AD). }
  \item{position}{numeric vector containing the position of the column corresponding to the MCMC chains of interest}
  \item{level}{probability corresponding to the level of confidence used for the credible interval}
  \item{GridLength}{the number of equally spaced points at which the density is to be estimated (for density() function)}
  \item{x.scale}{one of "calendar" for calendar years, "BP" for years before present, or "elapsed" for years after a specified origin}
  \item{elapsed.origin}{the position of the column corresponding to the origin for elapsed time calculations}
  \item{title}{title of the graph}
  \item{subtitle}{subtitle of the graph}
  \item{caption}{caption of the graph}
  \item{x.label}{x axis label of the graph}
  \item{y.label}{y axis label of the graph}
  \item{y.grid}{switch for horizontal grids}
  \item{x.min}{minimum x axis value}
  \item{x.max}{maximum x axis value}
  \item{legend.title}{the title of the legend}
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
This function draws the densities of several dates and adds summary statistics. The input (MCMC chains) should either be in calendar dates or converted in calendar dates using x.scale vector or elapse.origin. 
}
\details{
The density is estimated using density() function with n=GridLength.
}
\author{
  Anne Philippe <Anne.Philippe@univ-nantes.fr> and

  Marie-Anne Vibet <Marie-Anne.Vibet@univ-nantes.fr>
}
\examples{
  data(Events); 
  MultiMarginalPlot(Events, position = c(2,3,4), level = 0.95)

}
\keyword{ marginal posterior density }
\keyword{ credible interval }
\keyword{ mean}

