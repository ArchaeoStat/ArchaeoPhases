\name{TempoActivityPlot}
\alias{TempoActivityPlot}
\title{Plot of the activity of events}
\usage{
TempoActivityPlot(data, position, plot.result = NULL, level = 0.95,
                               title = "Activity plot",
                               subtitle = NULL, caption = "ArcheoPhases",
                               x.label = "Calendar year",
                               y.label = "Activity",
                               line.types = c("solid"),
                               width = 7, height = 7, units = "in",
                               x.min = NULL, x.max = NULL,
                               file = NULL, x.scale = "calendar",
                               elapsed.origin.position = NULL, 
                               newWindow=TRUE, print.data.result = FALSE)
}
\arguments{
  \item{data}{dataframe containing the output of the MCMC algorithm. The MCMC samples should be in calendar year (BC/AD). }
  \item{position}{numeric vector containing the position of the column corresponding to the MCMC chains of interest}
  \item{plot.result}{a list containing the data to plot, typically the result of a previous run of TempoActivityPlot()}
  \item{level}{probability corresponding to the level of confidence used for the credible interval}
  \item{title}{title of the graph}
  \item{subtitle}{subtitle of the graph}
  \item{caption}{caption of the graph}
  \item{x.label}{x axis label of the graph}
  \item{y.label}{y axis label of the graph}
  \item{line.types}{type of the lines drawn of the graph in the order of legend.labels}
  \item{height}{height of the graph in units}
  \item{width}{width of the graph in units}
  \item{units}{recognized by ggsave function, one of "in", "cm", "mm"}
  \item{x.min}{minimum x axis value}
  \item{x.max}{maximum x axis value}
  \item{x.scale}{one of "calendar" for calendar years, "BP" for years before present, or "elapsed" for years after a specified origin}
  \item{elapsed.origin.position}{the position of the column corresponding to the origin for elapsed time calculations}
  \item{file}{the name of the file to be saved. If NULL then no graph is saved.}
  \item{newWindow}{whether the plot is drawn within a new window or not}
  \item{print.data.result}{If TRUE, the list containing the data to plot will be given}}
\value{
It is the derivative of the TempoPlot bayesian estimate. 
It may also return a list containing the data to plot (if print.data.result = TRUE). The result is given in calendar year (in format BC/AD).
}
\description{
A statistical graphic designed for the archaeological study of rhythms of the long term that embodies a theory of archaeological evidence for the occurrence of events.
}
\author{
  Anne Philippe <Anne.Philippe@univ-nantes.fr>, Thomas S. Dye  <TSD@tsdye.com> and

  Marie-Anne Vibet <Marie-Anne.Vibet@univ-nantes.fr>
}
\references{
Dye, T.S. (2016) Long-term rhythms in the development of Hawaiian social stratification. Journal of Archaeological Science, 71, 1--9.
}
\examples{
  data(Events); 
  TempoActivityPlot(Events[1:1000,], c(2:5), print.data.result=FALSE)
  TempoActivityPlot(Events[1:1000,], c(2:5), print.data.result=FALSE)
}
\keyword{ Tempo Activity plot}
\keyword{ mean }

