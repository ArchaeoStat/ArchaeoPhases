\name{CreateMinMaxGroup}
\alias{CreateMinMaxGroup}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Constructing the minimum and the maximum for a group of dates(phase)  
}
\description{
Constructs a dataframe containing the output of the MCMC algorithm corresponding to the minimum and the maximum of a group of dates (phase)
}
\usage{
CreateMinMaxGroup(data, position, name ="Phase", add=NULL, 
exportFile=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{dataframe containing the output of the MCMC algorithm }
  \item{position}{numeric vector containing the position of the column corresponding to the MCMC chains of all dates included in the phase of interest}
  \item{name}{name of the current group of dates or phase }
  \item{add}{the name of the dataframe in which the current minimum and maximum should be added. Null by default.}
  \item{exportFile}{the name of the final file that will be saved if chosen. Null by default. }
}
\value{
A dataframe containing the minimum and the maximum of the group of dates included in the phase of interest. These values may be added to an already existing file "add" if given. 
}
\author{
  Anne Philippe <Anne.Philippe@univ-nantes.fr> and
  
  Marie-Anne Vibet <Marie-Anne.Vibet@univ-nantes.fr>
}

\examples{
    data(Events)
    Temp = CreateMinMaxGroup(Events, c(2,4), name = "Phase2")
    Temp = CreateMinMaxGroup(Events, c(3,5), name = "Phase1", add=Temp, 
    exportFile = "MinMaxPhases.csv")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Minimum of a group of dates }
\keyword{ Maximum of a group of dates }
