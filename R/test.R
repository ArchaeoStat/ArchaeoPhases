# TEST
#' @include AllClasses.R AllGenerics.R
NULL

# Older ========================================================================
#' @export
#' @describeIn older Returns a length-one [`numeric`] vector (the posterior
#'  probability of the assumption: "event `x` is older than event `y`").
#' @aliases older,numeric,numeric-method
setMethod(
  f = "older",
  signature = c(x = "numeric", y = "numeric"),
  definition = function(x, y) {
    arkhe::assert_length(y, length(x))

    ## Bayesian test: x < y
    mean(x < y)
  }
)

#' @export
#' @describeIn older Returns a [`numeric`] matrix of posterior probabilities.
#' @aliases older,EventsMCMC-method
setMethod(
  f = "older",
  signature = c(x = "EventsMCMC", y = "missing"),
  definition = function(x, y) {
    n <- ncol(x)
    z <- matrix(nrow = n, ncol = n)
    dimnames(z) <- list(names(x), names(x))
    for (i in 1:n) {
      for (j in 1:n) {
        z[i, j] <- older(x[, i, drop = TRUE], x[, j, drop = TRUE])
      }
    }
    z
  }
)
