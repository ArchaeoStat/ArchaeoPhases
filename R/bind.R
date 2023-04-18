# BIND MCMC
#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname bind
#' @aliases cbind2,MCMC,MCMC-method
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

#' @export
#' @rdname sort.list
#' @aliases sort.list,MCMC-method
setMethod(
  f = "sort.list",
  signature = c(x = "MCMC"),
  definition = function(x, decreasing = FALSE) {
    decreasing <- ifelse(is_CE(x), decreasing, !decreasing)
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
    decreasing <- ifelse(is_CE(x), decreasing, !decreasing)
    bound <- boundaries(x)
    mid <- apply(X = bound, MARGIN = 1, FUN = stats::median)
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
    print(i)
    x[, i, , drop = FALSE]
  }
)
