# TIME SCALE
#' @include AllClasses.R AllGenerics.R
NULL

# Convert ======================================================================
#' @export
#' @rdname convert
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
#' @rdname convert
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
#' @rdname convert
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
#' @rdname convert
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
#' @rdname convert
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
#' @rdname convert
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
