# MUTATORS
#' @include AllClasses.R
NULL

#' @export
#' @rdname names
#' @aliases names,MCMC-method
setMethod(
  f = "names",
  signature = "MCMC",
  definition = function(x) x@labels
)

#' @export
#' @rdname names
#' @aliases names<-,MCMC-method
setMethod(
  f = "names<-",
  signature = "MCMC",
  definition = function(x, value) {
    x@labels <- value
    colnames(x) <- value
    methods::validObject(x)
    x
  }
)

#' @export
#' @rdname names
#' @aliases names,PhasesMCMC-method
setMethod(
  f = "names",
  signature = "PhasesMCMC",
  definition = function(x) x@labels
)

#' @export
#' @rdname names
#' @aliases names<-,PhasesMCMC-method
setMethod(
  f = "names<-",
  signature = "PhasesMCMC",
  definition = function(x, value) {
    x@labels <- value
    colnames(x) <- value
    methods::validObject(x)
    x
  }
)

# Getters ======================================================================
get_hash <- function(x) x@hash
