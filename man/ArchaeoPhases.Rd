\docType{package}
\name{ArchaeoPhases}
\alias{ArchaeoPhases}
\alias{ArchaeoPhases-package}
\title{ArchaeoPhases: Post-Processing of the Markov Chain Simulated by 'ChronoModel', 'Oxcal' or 'BCal'}
\description{
This package provides a list of functions for the statistical analysis of archaeological phases. It is based on the post-processing of the Markov Chains whose stationary distribution is the posterior distribution of a series of dates. Such MCMC output can be simulated by different applications as for instance 'ChronoModel' (see <http://www.chronomodel.fr>), 'Oxcal' (see <https://c14.arch.ox.ac.uk/oxcal.html>) or 'BCal' (see <http://bcal.shef.ac.uk/>). The only requirement is to have a CSV file containing a sample from the posterior distribution.

}
\section{ArchaeoPhases functions}{

The most important functions are \code{\link{TempoPlot}} that is "a statistical graphic designed for the
 archaeological study of rhythms of the long term that embodies a theory of archaeological evidence for the occurrence of events", 
 \code{\link{PhaseTimeRange}} or \code{\link{MultiPhaseTimeRange}} that calculate time intervals to characterize archaeological phases or
 periods, 
 \code{\link{PhasesTransition}} or \code{\link{MultiPhasesTransition}} that calculate time intervals to characterize the transition between two
 successive phases or periods, 
 \code{\link{PhasesGap}} or \code{\link{PhasesGap}} that are Testing procedures to check the presence of a gap between two successive phases or periods. A gap interval 
is estimated if we accept its existence. 
 See the help files for these functions for examples. See the vignette for more complete documentation
}