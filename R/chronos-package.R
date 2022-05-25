#' @details
#'  \tabular{ll}{
#'   **Package:** \tab chronos \cr
#'   **Type:** \tab Package \cr
#'   **Version:** \tab 0.0.0.9000 \cr
#'   **License:** \tab GPL-3 \cr
#'   **JSS:** \tab \doi{10.18637/jss.v093.c01} \cr
#'  }
#'
#' @section Package options:
#'  `chronos` uses the following [options()] to configure behaviour:
#'  * `chronos.grid`: a [`numeric`] value specifying the number of equally
#'    spaced points at which densities are to be estimated (defaults to
#'    \eqn{512}). Should be a power of \eqn{2}.
#'  * `chronos.precision`: an [`integer`] indicating the number of decimal
#'    places (defaults to \eqn{0}).
#'  * `chronos.progress`: a [`logical`] scalar specifying if progress bars
#'    should be displayed (defaults to [interactive()]).
#'
#' @author
#'  **Full list of authors and contributors** (alphabetic order)
#'
#'  \tabular{ll}{
#'   Thomas S. Dye \tab *University of Hawai'i at Manoa, USA* \cr
#'   Nicolas Frerebeau \tab *Université Bordeaux Montaigne, France* \cr
#'   Anne Philippe \tab *Université de Nantes, France* \cr
#'   Marie-Anne Vibet \tab *Université de Nantes, France* \cr
#'  }
#'
#'  **Package maintainer**
#'
#'  Anne Philippe\cr
#'  \email{anne.philippe@@univ-nantes.fr}
#'
#'  Laboratoire de Mathématiques Jean Leray (UMR 6629)\cr
#'  2, rue de la Houssinière\cr
#'  BP 92208\cr
#'  F-44322 Nantes Cedex 3\cr
#'  France
#' @name chronos-package
#' @aliases chronos
#' @docType package
#' @keywords internal
"_PACKAGE"

#' @import arkhe
#' @importFrom coda mcmc.list mcmc
#' @importFrom ggplot2 aes autoplot element_blank element_text facet_grid ggplot
#' geom_area geom_hline geom_path geom_rect geom_ribbon geom_segment geom_tile
#' guides guide_colorbar guide_legend scale_colour_manual scale_fill_manual
#' scale_fill_viridis_c scale_x_continuous scale_x_reverse scale_y_continuous
#' scale_y_discrete scale_y_reverse theme theme_bw vars
#' @importFrom ggridges geom_density_ridges
#' @importFrom methods as callGeneric callNextMethod new prototype setClass
#' setGeneric setMethod setValidity validObject .valueClassTest
#' @importFrom rlang .data
#' @importFrom stats density dnorm dunif loess median
#' @importFrom tools md5sum
#' @importFrom utils download.file file_test
NULL
