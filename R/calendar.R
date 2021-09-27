# TIME SCALE
#' @include AllClasses.R AllGenerics.R
NULL

# Predicates ===================================================================
#' @export
#' @rdname calendar
#' @aliases is_BP,MCMC-method
setMethod(
  f = "is_BP",
  signature = "MCMC",
  definition = function(object) identical(get_calendar(object), "BP")
)

#' @export
#' @rdname calendar
#' @aliases is_BP,PhasesMCMC-method
setMethod(
  f = "is_BP",
  signature = "PhasesMCMC",
  definition = function(object) identical(get_calendar(object), "BP")
)

#' @export
#' @rdname calendar
#' @aliases is_BP,CumulativeEvents-method
setMethod(
  f = "is_BP",
  signature = "CumulativeEvents",
  definition = function(object) identical(get_calendar(object), "BP")
)

#' @export
#' @rdname calendar
#' @aliases is_BP,ActivityEvents-method
setMethod(
  f = "is_BP",
  signature = "ActivityEvents",
  definition = function(object) identical(get_calendar(object), "BP")
)

#' @export
#' @rdname calendar
#' @aliases is_CE,MCMC-method
setMethod(
  f = "is_CE",
  signature = "MCMC",
  definition = function(object) identical(get_calendar(object), "CE")
)

#' @export
#' @rdname calendar
#' @aliases is_CE,PhasesMCMC-method
setMethod(
  f = "is_CE",
  signature = "PhasesMCMC",
  definition = function(object) identical(get_calendar(object), "CE")
)

#' @export
#' @rdname calendar
#' @aliases is_CE,CumulativeEvents-method
setMethod(
  f = "is_CE",
  signature = "CumulativeEvents",
  definition = function(object) identical(get_calendar(object), "CE")
)

#' @export
#' @rdname calendar
#' @aliases is_CE,ActivityEvents-method
setMethod(
  f = "is_CE",
  signature = "ActivityEvents",
  definition = function(object) identical(get_calendar(object), "CE")
)

# Convert ======================================================================
## Elapsed origin --------------------------------------------------------------
#' @export
#' @rdname calendar
#' @aliases elapse,MCMC-method
setMethod(
  f = "elapse",
  signature = "MCMC",
  definition = function(object, origin = 1) {
    tmp <- object[, -origin] - object[, origin]
    methods::initialize(object, as(tmp, "matrix"), calendar = "elapsed")
  }
)

## BP to CE --------------------------------------------------------------------
#' @export
#' @rdname calendar
#' @aliases BP_to_CE,numeric-method
setMethod(
  f = "BP_to_CE",
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
#' @aliases BP_to_CE,matrix-method
setMethod(
  f = "BP_to_CE",
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
#' @aliases BP_to_CE,array-method
setMethod(
  f = "BP_to_CE",
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
#' @aliases BP_to_CE,MCMC-method
setMethod(
  f = "BP_to_CE",
  signature = "MCMC",
  definition = function(object){
    ## Check current scale
    if (is_CE(object)) return(object)
    tmp <- methods::callNextMethod(object)
    methods::initialize(object, tmp, calendar = "CE")
  }
)

#' @export
#' @rdname calendar
#' @aliases BP_to_CE,PhasesMCMC-method
setMethod(
  f = "BP_to_CE",
  signature = "PhasesMCMC",
  definition = function(object){
    ## Check current scale
    if (is_CE(object)) return(object)
    tmp <- methods::callNextMethod(object)
    methods::initialize(object, tmp, calendar = "CE")
  }
)

#' @export
#' @rdname calendar
#' @aliases BP_to_CE,CumulativeEvents-method
setMethod(
  f = "BP_to_CE",
  signature = "CumulativeEvents",
  definition = function(object){
    ## Check current scale
    if (is_CE(object)) return(object)
    object@year <- BP_to_CE(object@year)
    object@calendar <- "CE"
    object
  }
)

## CE to BP --------------------------------------------------------------------
#' @export
#' @rdname calendar
#' @aliases CE_to_BP,numeric-method
setMethod(
  f = "CE_to_BP",
  signature = "numeric",
  definition = function(object){
    index <- !is.na(object)
    if (any(object[index] == 0)) {
      stop("0 BCE/CE is not a valid year!", call. = FALSE)
    }
    if (any(object[index] > 1950)) {
      stop("Post-bomb dates (> 1950 CE) are not supported.", call. = FALSE)
    }
    tmp <- rep(NA, length(object))
    tmp[index & object > 0] <- abs(object[index & object > 0] - 1950)
    tmp[index & object < 0] <- abs(object[index & object < 0] - 1949)
    tmp
  }
)

#' @export
#' @rdname calendar
#' @aliases CE_to_BP,matrix-method
setMethod(
  f = "CE_to_BP",
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
#' @aliases CE_to_BP,array-method
setMethod(
  f = "CE_to_BP",
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
#' @aliases CE_to_BP,MCMC-method
setMethod(
  f = "CE_to_BP",
  signature = "MCMC",
  definition = function(object){
    ## Check current scale
    if (is_BP(object)) return(object)
    tmp <- methods::callNextMethod(object = object)
    methods::initialize(object, tmp, calendar = "BP")
  }
)

#' @export
#' @rdname calendar
#' @aliases CE_to_BP,PhasesMCMC-method
setMethod(
  f = "CE_to_BP",
  signature = "PhasesMCMC",
  definition = function(object){
    ## Check current scale
    if (is_BP(object)) return(object)
    tmp <- methods::callNextMethod(object = object)
    methods::initialize(object, tmp, calendar = "BP")
  }
)

#' @export
#' @rdname calendar
#' @aliases CE_to_BP,CumulativeEvents-method
setMethod(
  f = "CE_to_BP",
  signature = "CumulativeEvents",
  definition = function(object){
    ## Check current scale
    if (is_BP(object)) return(object)
    object@year <- CE_to_BP(object@year)
    object@calendar <- "BP"
    object
  }
)
