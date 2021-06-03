# BIND MCMC
#' @include AllClasses.R AllGenerics.R
NULL

# @export
# @rdname bind
# @aliases cbind2,MCMC,MCMC-method
setMethod(
  f = "cbind2",
  signature = c(x = "MCMC", y = "MCMC"),
  definition = function(x, y) {
    ## Validation
    if (get_calendar(x) != get_calendar(y)) {
      stop("All object must have the same calendar scale.", call. = FALSE)
    }

    mtx_x <- as(x, "matrix")
    mtx_y <- as(y, "matrix")

    eve <- c(names(x), names(y))
    if (any(duplicated(eve))) {
      warning("Duplicated event names!", call. = FALSE)
    }

    .MCMC(
      cbind(mtx_x, mtx_y),
      events = eve,
      calendar = get_calendar(x),
      hash = character(0)
    )
  }
)
