# INTERPOLATE
#' @include AllGenerics.R
NULL

# Interpolate ==================================================================
#' @export
#' @rdname interpolate
#' @aliases interpolate,numeric,numeric-method
setMethod(
  f = "interpolate",
  signature = c(x = "numeric", y = "numeric"),
  definition = function(x, y) {
    arkhe::assert_length(y, length(x))

    n <- length(x)
    stats::runif(n, x, y)
  }
)

#' @export
#' @rdname interpolate
#' @aliases interpolate,EventsMCMC,missing-method
setMethod(
  f = "interpolate",
  signature = c(x = "EventsMCMC", y = "missing"),
  definition = function(x, e1 = 1, e2 = 2) {
    a <- x[, e1, drop = TRUE]
    b <- x[, e2, drop = TRUE]
    e <- interpolate(x = a, y = b)

    ## Build names
    eve <- names(x)
    eve <- c(eve[[e1]], paste(eve[[e1]], eve[[e2]], sep = "-"), eve[[e2]])

    ## Return an MCM object
    .EventsMCMC(
      cbind(a, e, b),
      labels = eve,
      hash = get_hash(x)
    )
  }
)
