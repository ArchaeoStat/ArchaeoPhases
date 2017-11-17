\name{OccurrencePlot}
\alias{OccurrencePlot}
\title{Plot of the occurence of events}
\usage{
OccurrencePlot(data, position, plot.result = NULL, level = 0.95, 
                           intervals = "CI",
                           title = "Occurrence plot",
                           subtitle = NULL,
                           caption = "ArchaeoPhases",
                           labelXaxis = "Calendar year",
                           labelYaxis = NULL,
                           language = "English", occurrence = "occurrence",
                           height = 7, width = 7, units = "in",
                           x.min = NULL, x.max = NULL, x.scale = "calendar",
                           elapsed.origin.position = NULL,
                           dumbbell.size = 1, dot.guide = FALSE,
                           dot.guide.size = 0.25, y.grid = FALSE,
                           file = NULL, 
                           newWindow=TRUE, print.data.result = FALSE)
}
\arguments{
  \item{data}{dataframe containing the output of the MCMC algorithm. The MCMC samples should be in calendar year (BC/AD). }
  \item{position}{numeric vector containing the position of the column corresponding to the MCMC chains of interest}
  \item{plot.result}{a list containing the data to plot, typically the result of a previous run of OccurrencePlot()}
  \item{level}{probability corresponding to the level of confidence used for the credible interval}
  \item{intervals}{"CI" corresponds to the credible intervals, "HPD" to the highest density regions}
  \item{title}{title of the graph}
  \item{subtitle}{subtitle of the graph}
  \item{caption}{caption of the graph}
  \item{labelXaxis}{x axis label of the graph}
  \item{labelYaxis}{y axis label of the graph}
  \item{language}{English by default}
  \item{occurrence}{Text for the y-values}
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
\value{
It calculates the calendar date t corresponding to the smallest date such that the number of events observed before t is equal to k.
The result is given in calendar year (in format BC/AD). It may also return a list containing the data to plot (if print.data.result = TRUE).
}
\description{
A statistical graphic designed for the archaeological study of the timing of the occurrence of events. 
}
\details{
If we have k events. 
We can estimate the calendar date t corresponding to the smallest date such that the number of events observed before t is equal to k.  
The OccurrencePlot estimates theses dates and give the credible interval or the highest posterior density (HPD) region of theses dates associated to a desired level of confidence.
}
\author{
  Anne Philippe <Anne.Philippe@univ-nantes.fr>, Thomas S. Dye  <TSD@tsdye.com> and

  Marie-Anne Vibet <Marie-Anne.Vibet@univ-nantes.fr>
}

\examples{
  data(Events); 
  OccurrencePlot(Events[1:1000,], c(2:5),  print.data.result = FALSE)
}
\keyword{ tempo plot}
\keyword{ credible interval }
\keyword{ mean }

