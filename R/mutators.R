# MUTATORS
#' @include AllClasses.R
NULL

#' @export
#' @rdname mutator
#' @aliases names,MCMC-method
setMethod(
  f = "names",
  signature = "MCMC",
  definition = function(x) x@events
)

#' @export
#' @rdname mutator
#' @aliases names,PhasesMCMC-method
setMethod(
  f = "names",
  signature = "PhasesMCMC",
  definition = function(x) x@phases
)

#' @export
#' @rdname mutator
#' @aliases names<-,PhasesMCMC-method
setMethod(
  f = "names<-",
  signature = "PhasesMCMC",
  definition = function(x, value) {
    x@phases <- value
    methods::validObject(x)
    x
  }
)

# Getters ======================================================================
#' @export
#' @rdname mutator
#' @aliases get_calendar,MCMC-method
setMethod(
  f = "get_calendar",
  signature = "MCMC",
  definition = function(x) x@calendar
)

#' @export
#' @rdname mutator
#' @aliases get_calendar,PhasesMCMC-method
setMethod(
  f = "get_calendar",
  signature = "PhasesMCMC",
  definition = function(x) x@calendar
)

#' @export
#' @rdname mutator
#' @aliases get_calendar,CumulativeEvents-method
setMethod(
  f = "get_calendar",
  signature = "CumulativeEvents",
  definition = function(x) x@calendar
)

#' @export
#' @rdname mutator
#' @aliases get_calendar,ActivityEvents-method
setMethod(
  f = "get_calendar",
  signature = "ActivityEvents",
  definition = function(x) x@calendar
)

#' @export
#' @rdname mutator
#' @aliases get_calendar,OccurrenceEvents-method
setMethod(
  f = "get_calendar",
  signature = "OccurrenceEvents",
  definition = function(x) x@calendar
)

#' @export
#' @rdname mutator
#' @aliases get_hash,MCMC-method
setMethod(
  f = "get_hash",
  signature = "MCMC",
  definition = function(x) x@hash
)

#' @export
#' @rdname mutator
#' @aliases get_hash,PhasesMCMC-method
setMethod(
  f = "get_hash",
  signature = "PhasesMCMC",
  definition = function(x) x@hash
)

#' @export
#' @rdname mutator
#' @aliases get_hash,CumulativeEvents-method
setMethod(
  f = "get_hash",
  signature = "CumulativeEvents",
  definition = function(x) x@hash
)

#' @export
#' @rdname mutator
#' @aliases get_hash,ActivityEvents-method
setMethod(
  f = "get_hash",
  signature = "ActivityEvents",
  definition = function(x) x@hash
)

#' @export
#' @rdname mutator
#' @aliases get_hash,OccurrenceEvents-method
setMethod(
  f = "get_hash",
  signature = "OccurrenceEvents",
  definition = function(x) x@hash
)
