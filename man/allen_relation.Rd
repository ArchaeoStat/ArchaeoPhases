% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllenDataFrame.R
\name{allen_relation}
\alias{allen_relation}
\title{Allen relation of two definite intervals}
\usage{
allen_relation(start_1, end_1, start_2, end_2)
}
\arguments{
\item{start_1}{The start date of the first interval}

\item{end_1}{The end date of the first interval}

\item{start_2}{The start date of the second interval}

\item{end_2}{The end date of the second interval}
}
\value{
A one-letter code indicating the Allen relation
}
\description{
Calculates the Allen relation of two definite intervals and
reports the one-letter code for the interval proposed by Thomas Alspaugh.
Stops with an error if the end of an interval is earlier than its start.
}
\author{
Thomas S. Dye
}