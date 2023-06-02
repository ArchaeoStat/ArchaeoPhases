# SORT
#' @include AllGenerics.R
NULL

#' @export
#' @rdname sort.list
#' @aliases sort.list,MCMC-method
setMethod(
  f = "sort.list",
  signature = c(x = "MCMC"),
  definition = function(x, decreasing = FALSE) {
    mid <- apply(X = x, MARGIN = 2, FUN = stats::median)
    order(mid, decreasing = decreasing)
  }
)

#' @export
#' @rdname sort
#' @aliases sort,MCMC-method
setMethod(
  f = "sort",
  signature = c(x = "MCMC"),
  definition = function(x, decreasing = FALSE) {
    i <- sort.list(x, decreasing = decreasing)
    x[, i, drop = FALSE]
  }
)

#' @export
#' @rdname sort.list
#' @aliases sort.list,PhasesMCMC-method
setMethod(
  f = "sort.list",
  signature = c(x = "PhasesMCMC"),
  definition = function(x, decreasing = FALSE) {
    bound <- as.data.frame(boundaries(x))
    mid <- apply(X = bound[, c("start", "end")], MARGIN = 1, FUN = stats::median)
    order(mid, decreasing = decreasing)
  }
)

#' @export
#' @rdname sort
#' @aliases sort,PhasesMCMC-method
setMethod(
  f = "sort",
  signature = c(x = "PhasesMCMC"),
  definition = function(x, decreasing = FALSE) {
    i <- sort.list(x, decreasing = decreasing)
    x[, i, , drop = FALSE]
  }
)
