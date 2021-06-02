# TIME SCALE
#' @include AllClasses.R AllGenerics.R
NULL

# Predicates ===================================================================
#' @export
#' @rdname calendar
#' @aliases is_BP,CumulativeEvents-method
setMethod(
  f = "is_BP",
  signature = "CumulativeEvents",
  definition = function(object) {
    get_calendar(object) == "BP"
  }
)

#' @export
#' @rdname calendar
#' @aliases is_CE,CumulativeEvents-method
setMethod(
  f = "is_CE",
  signature = "CumulativeEvents",
  definition = function(object) {
    get_calendar(object) == "BCAD"
  }
)

# Convert ======================================================================
## BP to BC/AD -----------------------------------------------------------------
#' @export
#' @rdname calendar
#' @aliases BP_to_BCAD,numeric-method
setMethod(
  f = "BP_to_BCAD",
  signature = "numeric",
  definition = function(object){
    index <- !is.na(object)
    if (any(object[index] < 0)) {
      stop("Post-bomb dates (< 0 BP) are not supported.", call. = FALSE)
    }
    tmp <- rep(NA, length(object))
    tmp[index & object < 1950] <- 1950 - object[index & object < 1950]
    tmp[index & object >= 1950] <- 1949 - object[index & object >= 1950]
    tmp
  }
)

#' @export
#' @rdname calendar
#' @aliases BP_to_BCAD,matrix-method
setMethod(
  f = "BP_to_BCAD",
  signature = "matrix",
  definition = function(object){
    tmp <- methods::callGeneric(object = as.vector(object))
    dim(tmp) <- dim(object)
    dimnames(tmp) <- dimnames(object)
    tmp
  }
)

#' @export
#' @rdname calendar
#' @aliases BP_to_BCAD,array-method
setMethod(
  f = "BP_to_BCAD",
  signature = "array",
  definition = function(object){
    tmp <- methods::callGeneric(object = as.vector(object))
    dim(tmp) <- dim(object)
    dimnames(tmp) <- dimnames(object)
    tmp
  }
)

#' @export
#' @rdname calendar
#' @aliases BP_to_BCAD,MCMC-method
setMethod(
  f = "BP_to_BCAD",
  signature = "MCMC",
  definition = function(object){
    ## Check current scale
    if (get_calendar(object) == "BCAD") return(object)
    tmp <- methods::callNextMethod(object)
    methods::initialize(object, tmp, calendar = "BCAD")
  }
)

#' @export
#' @rdname calendar
#' @aliases BP_to_BCAD,PhasesMCMC-method
setMethod(
  f = "BP_to_BCAD",
  signature = "PhasesMCMC",
  definition = function(object){
    ## Check current scale
    if (get_calendar(object) == "BCAD") return(object)
    tmp <- methods::callNextMethod(object)
    methods::initialize(object, tmp, calendar = "BCAD")
  }
)

#' @export
#' @rdname calendar
#' @aliases BP_to_BCAD,CumulativeEvents-method
setMethod(
  f = "BP_to_BCAD",
  signature = "CumulativeEvents",
  definition = function(object){
    ## Check current scale
    if (get_calendar(object) == "BCAD") return(object)
    object@year <- BP_to_BCAD(object@year)
    object@calendar <- "BCAD"
    object
  }
)

## BC/AD to BP -----------------------------------------------------------------
#' @export
#' @rdname calendar
#' @aliases BCAD_to_BP,numeric-method
setMethod(
  f = "BCAD_to_BP",
  signature = "numeric",
  definition = function(object){
    index <- !is.na(object)
    if (any(object[index] == 0)) {
      stop("0 BC/AD is not a valid year!", call. = FALSE)
    }
    if (any(object[index] > 1950)) {
      stop("Post-bomb dates (> 1950 AD) are not supported.", call. = FALSE)
    }
    tmp <- rep(NA, length(object))
    tmp[index & object > 0] <- abs(object[index & object > 0] - 1950)
    tmp[index & object < 0] <- abs(object[index & object < 0] - 1949)
    tmp
  }
)

#' @export
#' @rdname calendar
#' @aliases BCAD_to_BP,matrix-method
setMethod(
  f = "BCAD_to_BP",
  signature = "matrix",
  definition = function(object) {
    tmp <- methods::callGeneric(object = as.vector(object))
    dim(tmp) <- dim(object)
    dimnames(tmp) <- dimnames(object)
    tmp
  }
)

#' @export
#' @rdname calendar
#' @aliases BCAD_to_BP,array-method
setMethod(
  f = "BCAD_to_BP",
  signature = "array",
  definition = function(object) {
    tmp <- methods::callGeneric(object = as.vector(object))
    dim(tmp) <- dim(object)
    dimnames(tmp) <- dimnames(object)
    tmp
  }
)

#' @export
#' @rdname calendar
#' @aliases BCAD_to_BP,MCMC-method
setMethod(
  f = "BCAD_to_BP",
  signature = "MCMC",
  definition = function(object){
    ## Check current scale
    if (get_calendar(object) == "BP") return(object)
    tmp <- methods::callNextMethod(object = object)
    methods::initialize(object, tmp, calendar = "BP")
  }
)

#' @export
#' @rdname calendar
#' @aliases BCAD_to_BP,PhasesMCMC-method
setMethod(
  f = "BCAD_to_BP",
  signature = "PhasesMCMC",
  definition = function(object){
    ## Check current scale
    if (get_calendar(object) == "BP") return(object)
    tmp <- methods::callNextMethod(object = object)
    methods::initialize(object, tmp, calendar = "BP")
  }
)

#' @export
#' @rdname calendar
#' @aliases BCAD_to_BP,CumulativeEvents-method
setMethod(
  f = "BCAD_to_BP",
  signature = "CumulativeEvents",
  definition = function(object){
    ## Check current scale
    if (get_calendar(object) == "BP") return(object)
    object@year <- BCAD_to_BP(object@year)
    object@calendar <- "BP"
    object
  }
)
