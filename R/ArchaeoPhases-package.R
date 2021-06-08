#' @details
#'  \tabular{ll}{
#'   **Package:** \tab ArchaeoPhases \cr
#'   **Type:** \tab Package \cr
#'   **Version:** \tab 1.5.0.9000 \cr
#'   **License:** \tab GPL-3 \cr
#'   **JSS:** \tab \doi{10.18637/jss.v093.c01} \cr
#'  }
#'
#' @section Package options:
#'  `ArchaeoPhases` uses the following [options()] to configure behaviour:
#'  * `ArchaeoPhases.precision`: an [`integer`] indicating the number of decimal
#'    places (defaults to \eqn{0}).
#'  * `ArchaeoPhases.progress`: a [`logical`] scalar specifying if progress bars
#'    should be displayed.
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
#' @name ArchaeoPhases-package
#' @aliases ArchaeoPhases
#' @docType package
#' @keywords internal
"_PACKAGE"

#' @importFrom coda mcmc.list mcmc
#' @importFrom digest digest
#' @importFrom ggplot2 aes autoplot facet_grid ggplot geom_area geom_path
#' geom_rect geom_segment guides guide_legend scale_x_continuous scale_x_reverse
#' scale_y_continuous scale_y_discrete theme theme_bw vars
#' @importFrom hdrcde hdr
#' @importFrom methods as callGeneric callNextMethod new prototype setClass
#' setGeneric setMethod validObject .valueClassTest
#' @importFrom rlang .data
NULL
