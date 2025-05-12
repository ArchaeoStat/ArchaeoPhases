#' @details
#'  \tabular{ll}{
#'   **Version** \tab 2.0.0 \cr
#'   **License** \tab GPL-3 \cr
#'   **CRAN DOI** \tab \doi{10.32614/CRAN.package.ArchaeoPhases} \cr
#'   **Zenodo DOI** \tab \doi{10.5281/zenodo.8087121} \cr
#'   **JSS DOI** \tab \doi{10.18637/jss.v093.c01} \cr
#'  }
#'
#'  Laboratoire de Mathématiques Jean Leray (UMR 6629)\cr
#'  2, rue de la Houssinière\cr
#'  BP 92208\cr
#'  F-44322 Nantes Cedex 3\cr
#'  France
#'
#' @section Package options:
#'  \pkg{ArchaeoPhases} uses the following [options()] to configure behaviour:
#'  * `ArchaeoPhases.grid`: a [`numeric`] value specifying the number of equally
#'    spaced points at which densities are to be estimated (defaults to
#'    \eqn{512}). Should be a power of \eqn{2}.
#'  * `ArchaeoPhases.precision`: an [`integer`] indicating the number of decimal
#'    places (defaults to \eqn{0}).
#'  * `ArchaeoPhases.progress`: a [`logical`] scalar specifying if progress bars
#'    should be displayed (defaults to [interactive()]).
#'
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
