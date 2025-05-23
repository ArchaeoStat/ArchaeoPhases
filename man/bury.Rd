% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllGenerics.R, R/depth.R, R/plot.R
\docType{methods}
\name{bury}
\alias{bury}
\alias{bury-method}
\alias{bury,EventsMCMC,numeric-method}
\alias{bury,EventsMCMC-method}
\alias{predict,AgeDepthModel-method}
\alias{bury,AgeDepthModel-method}
\alias{plot,AgeDepthModel,missing-method}
\title{Age-Depth Modeling}
\usage{
bury(object, depth, ...)

\S4method{bury}{EventsMCMC,numeric}(object, depth)

\S4method{predict}{AgeDepthModel}(object, newdata)

\S4method{plot}{AgeDepthModel,missing}(
  x,
  level = 0.95,
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
\item{object}{An \code{\linkS4class{EventsMCMC}} object.}

\item{depth}{A \code{\link{numeric}} vector giving of the depths of the dated samples.}

\item{...}{Other \link[graphics:par]{graphical parameters} may also be passed as
arguments to this function, particularly, \code{border}, \code{col}, \code{lwd}, \code{lty} or
\code{pch}.}

\item{newdata}{A \code{\link{numeric}} vector giving the depths at which ages will be
predicted. If \code{missing}, the original data points are used.}

\item{x}{An \code{\linkS4class{AgeDepthModel}} object.}

\item{level}{A length-one \code{\link{numeric}} vector giving the confidence level.}

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
\item \code{bury()} returns an \code{\linkS4class{AgeDepthModel}} object.
\item \code{predict()} returns an \code{\linkS4class{EventsMCMC}} object.
\item \code{plot()} is called it for its side-effects: it results in a graphic being
displayed (invisibly returns \code{x}).
}
}
\description{
Computes the age-depth curve from the output of the MCMC algorithm and the
known depth of each dated samples.
}
\details{
We assume it exists a function \eqn{f} relating the age and the depth
\eqn{age = f(depth)}. We estimate the function using local regression
(also called local polynomial regression): \eqn{f = loess(age ~ depth)}.
This estimated function \eqn{f} depends on the unknown dates. However,
from the posterior distribution of the age/date sequence, we can evaluate
the posterior distribution of the age function for each desired depth.
}
\examples{
## Coerce to MCMC
eve <- matrix(rnorm(6000, (1:6)^2), ncol = 6, byrow = TRUE)
eve <- as_events(eve, calendar = CE())

## Compute an age-depth curve
age <- bury(eve, depth = 1:6)
plot(age)

## Predict new values
new <- predict(age, newdata = 1.5:5.5)
summary(new)

plot(eve)
plot(new)
}
\references{
Jha, D. K., Sanyal, P. & Philippe, A. (2020). Multi-Proxy Evidence of Late
Quaternary Climate and Vegetational History of North-Central India:
Implication for the Paleolithic to Neolithic Phases. \emph{Quaternary Science
Reviews}, 229: 106121. \doi{10.1016/j.quascirev.2019.106121}.

Ghosh, S., Sanyal, P., Roy, S., Bhushan, R., Sati, S. P., Philippe, A. &
Juyal, N. (2020). Early Holocene Indian Summer Monsoon and Its Impact on
Vegetation in the Central Himalaya: Insight from dD and d13C Values of Leaf
Wax Lipid. \emph{The Holocene}, 30(7): 1063-1074. \doi{10.1177/0959683620908639}.
}
\seealso{
Other age-depth modeling tools: 
\code{\link{interpolate}()}
}
\author{
A. Philippe
}
\concept{age-depth modeling tools}
