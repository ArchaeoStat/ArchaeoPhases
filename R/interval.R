# INTERVAL ESTIMATION
#' @include AllClasses.R AllGenerics.R
NULL

# CI ===========================================================================
#' @export
#' @rdname ci
#' @aliases interval_credible,numeric-method
setMethod(
  f = "interval_credible",
  signature = "numeric",
  definition = function(object, level = 0.95){

    ## Order the sample
    sorted <- sort(object)
    # Sample size
    N <- length(object)
    # Number of data to be outside of the interval
    outside <- N * (1 - level)

    inf <- seq(from = 1, to = outside + 1, by = 1)
    sup <- seq(from = N - outside, to = N, by = 1)
    ind <- which.min(sorted[sup] - sorted[inf]) # Look for the shortest interval

    c(lower = sorted[inf][ind], upper = sorted[sup][ind])
  }
)

#' @export
#' @rdname ci
#' @aliases interval_credible,MCMC-method
setMethod(
  f = "interval_credible",
  signature = "MCMC",
  definition = function(object, level = 0.95){
    ci <- apply(X = object, MARGIN = 2, FUN = interval_credible, level = level)
    ci <- t(ci)
    colnames(ci) <- c("lower", "upper")
    attr(ci, "level") <- level
    attr(ci, "calendar") <- get_calendar(object)
    ci
  }
)

# HPDI =========================================================================
# @export
# @rdname hpdi
# @aliases interval_hpd,numeric-method
# setMethod(
#   f = "interval_hpd",
#   signature = "numeric",
#   definition = function(object, level = 0.95, ...) {
#     tmp <- hdrcde::hdr(object, prob = level * 100, ...)
#     matrix(data = tmp$hdr, ncol = 2, byrow = TRUE,
#            dimnames = list(NULL, c("lower", "upper")))
#   }
# )

#' @export
#' @rdname hpdi
#' @aliases interval_hpd,MCMC-method
setMethod(
  f = "interval_hpd",
  signature = "MCMC",
  definition = function(object, level = 0.95, ...) {
    n <- ncol(object)
    k <- seq_len(n)

    hpdi <- vector(mode = "list", length = n)
    names(hpdi) <- colnames(object)
    for (i in k) {
      tmp <- hdrcde::hdr(object[, i, drop = TRUE], prob = level * 100, ...)
      hpdi[[i]] <- matrix(data = tmp$hdr, ncol = 2, byrow = TRUE,
                          dimnames = list(NULL, c("lower", "upper")))
    }

    attr(hpdi, "level") <- level
    attr(hpdi, "calendar") <- get_calendar(object)
    hpdi
  }
)

#' Simplify
#'
#' Reduces the result of `interval_hpd()` to a `matrix`.
#' @param x A [`list`] of [`matrix`] (returned by `interval_hpd()`)
#' @return A [`data.frame`].
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
bind_hpdi <- function(x) {
  hpdi <- do.call(rbind.data.frame, x)
  n <- vapply(X = x, FUN = nrow, FUN.VALUE = integer(1))
  hpdi$id <- rep(names(x), times = n)
  hpdi
}
