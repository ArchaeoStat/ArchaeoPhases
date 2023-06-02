# ELAPSED ORIGIN
#' @include AllGenerics.R
NULL

#' @export
#' @rdname elapse
#' @aliases elapse,MCMC-method
setMethod(
  f = "elapse",
  signature = "MCMC",
  definition = function(object, origin = 1) {
    tmp <- object[, -origin, drop = TRUE] - object[, origin, drop = TRUE]
    methods::initialize(object, methods::as(tmp, "matrix"),
                        labels = names(object)[-origin])
  }
)
