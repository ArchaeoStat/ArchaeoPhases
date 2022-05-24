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

    eve <- x@events
    if (!missing(j)) {
      if (is.character(j)) j <- match(j, eve)
      eve <- eve[j]
    }
    methods::initialize(x, z, events = eve)
  }
)

#' @export
#' @rdname subset
#' @aliases [,PhasesMCMC-method
setMethod(
  f = "[",
  signature = c(x = "PhasesMCMC"),
  function(x, i, j, ..., drop = TRUE) {
    z <- methods::callNextMethod()

    if (is.null(dim(z)) | length(dim(z)) == 2) {
      return(z)
    }

    pha <- x@phases
    if (!missing(j)) {
      if (is.character(j)) j <- match(j, pha)
      pha <- pha[j]
    }
    methods::initialize(x, z, phases = pha)
  }
)

## [[ --------------------------------------------------------------------------
#' @export
#' @rdname subset
#' @aliases [[,MCMC,numeric,missing-method
setMethod(
  f = "[[",
  signature = c(x = "MCMC", i = "numeric", j = "missing"),
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
    z <- x@phases[[i]]
    tmp <- x[, i, , drop = FALSE]
    methods::initialize(x, tmp, phases = z)
  }
)

#' @export
#' @rdname subset
#' @aliases [[,PhasesMCMC,character,missing-method
setMethod(
  f = "[[",
  signature = c(x = "PhasesMCMC", i = "character", j = "missing"),
  definition = function(x, i) {
    k <- which(x@phases == i)
    methods::callGeneric(x = x, i = k)
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

#' @export
#' @rdname subset
#' @aliases [<-,PhasesMCMC-method
setMethod(
  f = "[<-",
  signature = c(x = "PhasesMCMC"),
  function(x, i, j, ..., value) {
    z <- methods::callNextMethod()
    methods::validObject(z)
    z
  }
)
