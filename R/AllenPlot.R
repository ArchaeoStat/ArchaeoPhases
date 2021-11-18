#' Plot a Nökel lattice
#'
#' Reads MCMC output and plots a Nökel lattice.
#'
#' @author Thomas S. Dye
#'
#' @param mcmc path to a csv file with MCMC output
#' @param phases_1 list of three-element vectors, the first element of which is
#' the phase name and the second and third elements are the column indices
#' for the beginning and end of the phase, respectively
#' @param phases_2 list of three-element vectors, the first element of which is
#' the phase name and the second and third elements are the column indices
#' for the beginning and end of the phase, respectively
#' @param app one of 'bcal', 'oxcal', or 'chronomodel' to specify which
#' Bayesian calibration application produced the MCMC output
#' @param quiet One of 'no' to allow messages and warnings,
#' 'partial' (default) to suppress messages and allow warnings, or 'yes'
#' to suppress messages and warnings.
#' @param ... Named arguments to be passed on to \code{allen_plot()}.
#'
#' @return A layout_tbl_graph object.
#'
#' @examples
#'
#' # Plot to the R graphics device
#' # plot_lattice()
#'
#' # Save layout_tbl_graph object to a variable
#' # then plot to the R graphics device
#'
#' # foo <- plot_lattice()
#' # foo
#'
#' @export

plot_lattice <- function(mcmc, phases_1, phases_2, app = "bcal", quiet = "partial", ...) {
    allen_plot(allen_relate_phases(mcmc = mcmc,
                                   phases_1 = phases_1,
                                   phases_2 = phases_2,
                                   app = app),
               ...)
}
