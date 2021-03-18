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
#' @aliases [[,PhasesMCMC,numeric,missing-method
setMethod(
  f = "[[",
  signature = c(x = "PhasesMCMC", i = "numeric", j = "missing"),
  definition = function(x, i) {
    a <- x@start[[i]]
    b <- x@end[[i]]
    z <- x@phases[[i]]
    tmp <- x[, c(a, b)]
    tmp <- methods::as(tmp, "matrix")
    methods::initialize(x, tmp, start = 1L, end = 2L, phases = z)
  }
)

#' @export
#' @rdname subset
#' @aliases [[,PhasesMCMC,character,missing-method
setMethod(
  f = "[[",
  signature = c(x = "PhasesMCMC", i = "character", j = "missing"),
  definition = function(x, i) {
    k <- which(levels(x@phases) == i)
    a <- x@start[[k]]
    b <- x@end[[k]]
    z <- x@phases[[k]]
    tmp <- x[, c(a, b)]
    tmp <- methods::as(tmp, "matrix")
    methods::initialize(x, tmp, start = 1L, end = 2L, phases = z)
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
