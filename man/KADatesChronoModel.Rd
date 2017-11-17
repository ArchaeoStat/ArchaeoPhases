\name{KADatesChronoModel}
\alias{KADatesChronoModel}
\docType{data}
\title{
 Ksar Akil chronology build with ChronoModel software. Dates part.
}
\description{
MCMC samples from the posterior distribution of dates included in the Ksar Akil chronology build with ChronoModel software.
}
\usage{data("KADatesChronoModel")}

\format{
A data frame with 30000 observations on the following 17 variables, one variable per AMS date and the MCMC iteration number. These observations come from 3 parallel chains. The MCMC samples are in calendar year (BC/AD).
  \describe{
    \item{\code{iter}}{a numeric vector corresponding to iteration number}
    
    \item{\code{Layer.V}}{a numeric vector containing the output of the MCMC algorithm for the date of the shell extracted from layer V. }
    \item{\code{Layer.VI}}{a numeric vector containing the output of the MCMC algorithm for the date of the shell extracted from layer VI.}
    \item{\code{Layer.XI}}{a numeric vector containing the output of the MCMC algorithm for the date of the shell extracted from layer XI.}
    \item{\code{Layer.XII}}{a numeric vector containing the output of the MCMC algorithm for the date of the shell extracted from layer XII.}

    \item{\code{Layer.XVI.1}}{a numeric vector containing the output of the MCMC algorithm for the date of a shell extracted from layer XVI.}
    \item{\code{Layer.XVI.2}}{a numeric vector containing the output of the MCMC algorithm for the date of a shell extracted from layer XVI.}
    \item{\code{Layer.XVI.3}}{a numeric vector containing the output of the MCMC algorithm for the date of a shell extracted from layer XVI.}
    \item{\code{Layer.XVI.4}}{a numeric vector containing the output of the MCMC algorithm for the date of a shell extracted from layer XVI. }
    
    \item{\code{Layer.XVII.1}}{a numeric vector containing the output of the MCMC algorithm for the date of a shell extracted from layer XVII.}
    \item{\code{Layer.XVII.2}}{a numeric vector containing the output of the MCMC algorithm for the date of a shell extracted from layer XVII.}
    \item{\code{Layer.XVII.3}}{a numeric vector containing the output of the MCMC algorithm for the date of a shell extracted from layer XVII.}
        \item{\code{Layer.XVII.4}}{a numeric vector containing the output of the MCMC algorithm for the date of a shell extracted from layer XVII. }
        
    \item{\code{Layer.XVIII}}{a numeric vector containing the output of the MCMC algorithm for the date of the shell extracted from layer XVIII. }
    \item{\code{Layer.XIX}}{a numeric vector containing the output of the MCMC algorithm for the date of the shell extracted from layer XIX.}
    \item{\code{Layer.XX}}{a numeric vector containing the output of the MCMC algorithm for the date of the shell extracted from layer XX.}
    \item{\code{Layer.XXII}}{a numeric vector containing the output of the MCMC algorithm for the date of the shell extracted from layer XXII.}

  }
}

\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
Bosch, M. et al. (2015) New chronology for Ksar Akil (Lebanon) supports Levantine route of modern human dispersal into Europe. Proceedings of the National Academy of Sciences, 112, 7683--6.
}
\references{
Lanos, P. et al. (2016) Chronomodel  : Chronological Modelling of Archaeological Data using Bayesian Statistics (Version 1.5). www.chronomodel.fr

Vibet. M.-A. et al. (2016) ChronoModel V1.5 User's manual. 
}
\examples{
##load data
data("KADatesChronoModel")
}

\keyword{datasets}
\keyword{MCMC output}
\keyword{ChronoModel}

\author{
Anne Philippe <Anne.Philippe@univ-nantes.fr> and

Marie-Anne Vibet <Marie-Anne.Vibet@univ-nantes.fr>
}
