# ACCESSORS
#' @include AllClasses.R
NULL

# Extract ======================================================================
## [ ---------------------------------------------------------------------------
#' @export
#' @rdname subset
#' @aliases [,MCMC-method
setMethod(
  f = "[",
  signature = c(x = "MCMC"),
  function(x, i, j, ..., drop = TRUE) {
    z <- methods::callNextMethod()

    if (is.null(dim(z))) {
      return(z)
    }

    methods::initialize(x, z)
  }
)

## [[ --------------------------------------------------------------------------
#' @export
#' @rdname subset
#' @aliases [[,MCMC,ANY,missing-method
setMethod(
  f = "[[",
  signature = c(x = "MCMC", i = "ANY", j = "missing"),
  definition = function(x, i) {
    x[, i, drop = TRUE]
  }
)

#' @export
#' @rdname subset
#' @aliases [[,PhasesMCMC,ANY,missing-method
setMethod(
  f = "[[",
  signature = c(x = "PhasesMCMC", i = "ANY", j = "missing"),
  definition = function(x, i) {
    a <- x@start[[i]]
    b <- x@end[[i]]
    x[, c(a, b)]
  }
)

# Replace ======================================================================
## [<- -------------------------------------------------------------------------
#' @export
#' @rdname subset
#' @aliases [<-,MCMC-method
setMethod(
  f = "[<-",
  signature = c(x = "MCMC"),
  function(x, i, j, ..., value) {
    z <- methods::callNextMethod()
    methods::validObject(z)
    z
  }
)
