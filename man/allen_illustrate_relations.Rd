% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/allen-mcmc.R
\name{allen_illustrate_relations}
\alias{allen_illustrate_relations}
\title{Data for an Illustrative Graphic}
\usage{
allen_illustrate_relations(relations = "basic")
}
\arguments{
\item{relations}{A \code{\link{character}} string specifying the relation.
It must be one of "\code{basic}", "\code{concurrent}", "\code{distinct}",
"\code{stratigraphic}", "\code{branching}", "\code{transformation}", "\code{reticulation}",
"\code{sequence}", "\code{branch}", "\code{transform}", or "\code{reticulate}" (see details).}
}
\value{
A \code{data.frame} to be passed to \code{\link[=allen_plot]{allen_plot()}}.
}
\description{
Create a data frame that can be used as input for an illustrative plot.
Useful for describing the Allen operators: illustrate the full
set of Allen relations, concurrent Allen relations, and relations with
distinct endpoints. Also, useful for describing the chronological domains of
stratification, branching, transformation, and reticulation.
}
\details{
The illustrative graphics include:
\describe{
\item{\code{basic}}{the 13 basic Allen relations (default);}
\item{\code{concurrent}}{concurrent relations;}
\item{\code{distinct}}{relations with distinct endpoints;}
\item{\code{stratigraphic}}{basic relations established by an observation of superposition;}
\item{\code{branching}}{basic branching relations;}
\item{\code{transformation}}{basic relations of transformation;}
\item{\code{reticulation}}{basic relations of reticulation;}
\item{\code{sequence}}{composite relations in a stratigraphic sequence;}
\item{\code{branch}}{composite relations of branching;}
\item{\code{transform}}{composite relations of transformation; or}
\item{\code{reticulate}}{composite relations of reticulation.}
}
}
\seealso{
Other Allen's intervals: 
\code{\link{allen_analyze}()},
\code{\link{allen_analyze_relations}()},
\code{\link{allen_complement}()},
\code{\link{allen_composition}()},
\code{\link{allen_converse}()},
\code{\link{allen_illustrate}()},
\code{\link{allen_intersect}()},
\code{\link{allen_joint_concurrency}()},
\code{\link{allen_observe}()},
\code{\link{allen_observe_frequency}()},
\code{\link{allen_plot}()},
\code{\link{allen_relation}()},
\code{\link{allen_relation_code}()},
\code{\link{allen_union}()}
}
\author{
T. S. Dye
}
\concept{Allen's intervals}
\keyword{internal}
