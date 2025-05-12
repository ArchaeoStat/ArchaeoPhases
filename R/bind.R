# BIND MCMC
#' @include AllGenerics.R
NULL

#' @export
#' @rdname bind
#' @aliases cbind2,MCMC,MCMC-method
setMethod(
  f = "cbind2",
  signature = c(x = "MCMC", y = "MCMC"),
  definition = function(x, y) {
    mtx_x <- as(x, "matrix")
    mtx_y <- as(y, "matrix")

    eve <- c(names(x), names(y))
    if (any(duplicated(eve))) {
      warning(tr_("Duplicated event names!"), call. = FALSE)
    }

    .MCMC(
      cbind(mtx_x, mtx_y),
      labels = eve,
      hash = character(0)
    )
  }
)
