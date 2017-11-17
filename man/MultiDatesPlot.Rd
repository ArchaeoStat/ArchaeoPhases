\name{MultiDatesPlot}
\alias{MultiDatesPlot}
\title{ Plot of the endpoints of credible intervals or HPD intervals of a series of dates}
\usage{
MultiDatesPlot(data, position, level = 0.95, intervals = "CI", order ="default", 
                            title = "Plot of intervals",
                            subtitle = NULL,
                            caption = "ArchaeoPhases",
                            labelXaxis = "Calendar Year",
                            labelYaxis = NULL,
                            height = 7, width = 7, units = "in",
                            x.min = NULL, x.max = NULL,
                            x.scale = "calendar",
                            elapsed.origin.position = NULL,
                            dumbbell.size = 3, dot.guide = FALSE,
                            dot.guide.size = 0.25, y.grid = FALSE,
                            file = NULL, newWindow=TRUE, print.data.result = FALSE)
}
\arguments{
  \item{data}{dataframe containing the output of the MCMC algorithm. The MCMC samples should be in calendar year (BC/AD). }
  \item{position}{numeric vector containing the position of the column corresponding to the MCMC chains of interest}
  \item{level}{probability corresponding to the level of confidence used to estimate the credible interval}
  \item{intervals}{"CI" corresponds to the credible intervals, "HPD" to the highest density regions}
  \item{order}{"default" the y axis follows the order of the csv file. "increasing" the y axis is ordered according to the values of the interval (CI or the first HPD interval)}
  \item{title}{title of the graph}
  \item{subtitle}{subtitle of the graph}
  \item{caption}{caption of the graph}
  \item{labelXaxis}{x axis label of the graph}
  \item{labelYaxis}{y axis label of the graph}
  \item{height}{height of the graph in units}
  \item{width}{width of the graph in units}
  \item{units}{recognized by ggsave function, one of "in", "cm", "mm"}
  \item{x.min}{minimum x axis value}
  \item{x.max}{maximum x axis value}
  \item{x.scale}{one of "calendar" for calendar years, "BP" for years before present, or "elapsed" for years after a specified origin}
  \item{elapsed.origin.position}{the position of the column corresponding to the origin for elapsed time calculations}
  \item{dumbbell.size}{size of the symbols used to plot dates}
  \item{dot.guide}{switch for guides from y-axis to plot symbols}
  \item{dot.guide.size}{size of the dot guides}
  \item{y.grid}{switch for horizontal grids}
  \item{file}{the name of the file to be saved. If NULL then no graph is saved.}
  \item{newWindow}{whether the plot is drawn within a new window or not}
  \item{print.data.result}{If TRUE, the list containing the data to plot will be given}
}
\description{
Draws a plot of segments corresponding to the endpoints of the intervals (CI or HPD) of each selected date. The result is given in calendar year (in format BC/AD).
}
\author{
  Anne Philippe <Anne.Philippe@univ-nantes.fr>, Thomas S. Dye  <TSD@tsdye.com>  and

  Marie-Anne Vibet <Marie-Anne.Vibet@univ-nantes.fr>
}
\examples{
  data(Events)
  MultiDatesPlot(Events, c(2,4,3), level = 0.95, intervals ="CI", title = "Plot of CI intervals")
  MultiDatesPlot(Events, c(2,4,3), level = 0.95, intervals ="HPD", title = "Plot of HPD intervals")
  MultiDatesPlot(Events, c(2,4,3), level = 0.95, intervals ="HPD", , order="increasing")
}
\keyword{ credible interval }
\keyword{ highest posterior density regions }
