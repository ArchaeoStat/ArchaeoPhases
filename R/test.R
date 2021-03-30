# TEST
#' @include AllClasses.R AllGenerics.R
NULL

# Older ========================================================================
#' @export
#' @rdname test_older
#' @aliases test_older,MCMC-method
setMethod(
  f = "test_older",
  signature = c(x = "numeric", y = "numeric"),
  definition = function(x, y, ...) {
    ## Bayesian test: x < y
    mean(x < y)
  }
)
