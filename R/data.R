#' Events
#'
#' A data set containing information on the ages of four dated events.
#' @format A [`data.frame`] with 30,000 rows and 5 variables:
#'  \describe{
#'   \item{`iter`}{Iteration of the MCMC algorithm.}
#'   \item{`E1`}{Information on event 1.}
#'   \item{`E2`}{Information on event 2.}
#'   \item{`E3`}{Information on event 3.}
#'   \item{`E4`}{Information on event 4.}
#'  }
#' @family datasets
#' @keywords datasets
"mcmc_events"

#' Phases
#'
#' A data set containing information on the start and end dates of two phases.
#' @format A [`data.frame`] with 30,000 rows and 5 variables:
#'  \describe{
#'   \item{`iter`}{Iteration of the MCMC algorithm.}
#'   \item{`P2_alpha`}{Start date of Phase 2.}
#'   \item{`P2_beta`}{End date of Phase 2.}
#'   \item{`P1_alpha`}{Start date of Phase 1.}
#'   \item{`P1_beta`}{End date of Phase 1.}
#'  }
#' @family datasets
#' @keywords datasets
"mcmc_phases"
