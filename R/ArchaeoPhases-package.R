#' @details
#'  \tabular{ll}{
#'   **Package:** \tab ArchaeoPhases \cr
#'   **Type:** \tab Package \cr
#'   **Version:** \tab 2.0.0 \cr
#'   **License:** \tab GPL-3 \cr
#'   **JSS:** \tab \doi{10.18637/jss.v093.c01} \cr
#'  }
#'
#' @section Package options:
#'  `ArchaeoPhases` uses the following [options()] to configure behaviour:
#'  * `ArchaeoPhases.calendar`: a [`TimeScale-class`] object (default calendar
#'    for printing).
#'  * `ArchaeoPhases.grid`: a [`numeric`] value specifying the number of equally
#'    spaced points at which densities are to be estimated (defaults to
#'    \eqn{512}). Should be a power of \eqn{2}.
#'  * `ArchaeoPhases.precision`: an [`integer`] indicating the number of decimal
#'    places (defaults to \eqn{0}).
#'  * `ArchaeoPhases.progress`: a [`logical`] scalar specifying if progress bars
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
#' @name ArchaeoPhases-package
#' @aliases ArchaeoPhases
#' @docType package
#' @keywords internal
"_PACKAGE"

#' @import arkhe
#' @import aion
#' @importFrom methods as cbind2 getGeneric new setGeneric setMethod setValidity
#' .valueClassTest
NULL
