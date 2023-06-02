# INTERVAL ESTIMATION
#' @include AllGenerics.R
NULL

# CI ===========================================================================
#' @export
#' @rdname interval_credible
#' @aliases interval_credible,MCMC-method
setMethod(
  f = "interval_credible",
  signature = c(x = "MCMC"),
  definition = function(x, level = 0.95, calendar = getOption("ArchaeoPhases.calendar")) {
    cred <- apply(X = x, MARGIN = 2, FUN = arkhe::interval_credible,
                  level = level, simplify = FALSE)
    names(cred) <- names(x)
    if (is.null(calendar)) return(cred)
    lapply(
      X = cred,
      FUN = function(x, calendar) {
        x[, 1] <- chronos::as_year(x[, 1], calendar = calendar)
        x[, 2] <- chronos::as_year(x[, 2], calendar = calendar)
        x
      },
      calendar = calendar
    )
  }
)

# HPDI =========================================================================
#' @export
#' @rdname interval_hdr
#' @aliases interval_hdr,MCMC,missing-method
setMethod(
  f = "interval_hdr",
  signature = c(x = "MCMC", y = "missing"),
  definition = function(x, level = 0.95, calendar = getOption("ArchaeoPhases.calendar"), ...) {
    hpd <- apply(X = x, MARGIN = 2, FUN = arkhe::interval_hdr,
                 level = level, ..., simplify = FALSE)
    names(hpd) <- names(x)
    if (is.null(calendar)) return(hpd)
    lapply(
      X = hpd,
      FUN = function(x, calendar) {
        x[, 1] <- chronos::as_year(x[, 1], calendar = calendar)
        x[, 2] <- chronos::as_year(x[, 2], calendar = calendar)
        x
      },
      calendar = calendar
    )
  }
)

# Helpers ======================================================================
#' Simplify
#'
#' Reduces the result of `credible()` or `hpdi()` to a `matrix`.
#' @param x A [`list`] of [`matrix`] (returned by `credible()` or `hpdi()`)
#' @return A [`data.frame`].
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
bind_intervals <- function(x) {
  hpd <- do.call(rbind.data.frame, x)
  rownames(hpd) <- NULL
  n <- vapply(X = x, FUN = nrow, FUN.VALUE = integer(1))
  hpd <- data.frame(name = rep(names(x), times = n), hpd)
  hpd
}

#' Interval Matching
#'
#' Tests if the values of an MCMC sample belong to an interval.
#' @param x A [`numeric`] vector (MCMC sample).
#' @param y A [`matrix`] (returned by `credible()` or `hpdi()`).
#' @return A [`logical`] vector.
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
is_credible <- function(x, y) {
  tmp <- apply(
    X = y,
    MARGIN = 1,
    FUN = function(i, mcmc) {
      mcmc >= i[[1L]] & mcmc <= i[[2L]]
    },
    mcmc = x,
    simplify = TRUE
  )
  rowSums(tmp) > 0
}
