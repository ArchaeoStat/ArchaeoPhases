# INTERVAL ESTIMATION
#' @include AllClasses.R AllGenerics.R
NULL

# CI ===========================================================================
#' @export
#' @rdname credible
#' @aliases credible,numeric-method
setMethod(
  f = "credible",
  signature = "numeric",
  definition = function(object, level = 0.95, BP = FALSE, ...) {
    ## Order the sample
    sorted <- sort(object, method = "radix") # Faster sorting with radix method

    ## Sample size
    N <- length(object)

    ## Number of data to be outside of the interval
    outside <- as.integer(N * (1 - level))
    inf <- seq(from = 1L, to = outside + 1L, by = 1L)
    sup <- seq(from = N - outside, to = N, by = 1L)

    ## Look for the shortest interval
    a <- sorted[sup]
    b <- sorted[inf]
    ind <- which.min(a - b)

    ## Reverse boundaries if BP scale
    x <- ifelse(BP, a[[ind]], b[[ind]])
    y <- ifelse(BP, b[[ind]], a[[ind]])

    cbind(lower = x, upper = y, p = level)
  }
)

#' @export
#' @rdname credible
#' @aliases credible,MCMC-method
setMethod(
  f = "credible",
  signature = "MCMC",
  definition = function(object, level = 0.95, ...) {
    cred <- apply(X = object, MARGIN = 2, FUN = credible,
                  level = level, BP = is_BP(object), simplify = FALSE)
    names(cred) <- names(object)
    attr(cred, "calendar") <- get_calendar(object)
    cred
  }
)

# HPDI =========================================================================
#' @export
#' @rdname hpdi
#' @aliases hpdi,numeric-method
setMethod(
  f = "hpdi",
  signature = "numeric",
  definition = function(object, level = 0.95, BP = FALSE, ...) {
    ## Compute density
    d <- stats::density(object, ..., n = getOption("chronos.grid"))
    x <- d$x
    y <- d$y / sum(d$y)

    ## Order the sample (faster sorting with radix method)
    sorted <- sort(y, decreasing = TRUE, method = "radix")
    i <- min(which(cumsum(sorted) >= sum(y) * level))
    h <- sorted[[i]]
    idx <- which(y >= h)

    gap <- which(diff(idx) > 1)
    inf <- idx[c(1, gap + 1)]
    sup <- idx[c(gap, length(idx))]

    int <- mapply(FUN = seq, from = inf, to = sup,
                  SIMPLIFY = FALSE, USE.NAMES = FALSE)
    p <- vapply(X = int, FUN = function(i, y) { sum(y[i]) },
                FUN.VALUE = numeric(1), y = y)

    ## Reverse boundaries if BP scale
    a <- if(BP) x[sup] else x[inf]
    b <- if(BP) x[inf] else x[sup]

    cbind(lower = a, upper = b, p = round(p, digits = 2))
  }
)

#' @export
#' @rdname hpdi
#' @aliases hpdi,MCMC-method
setMethod(
  f = "hpdi",
  signature = "MCMC",
  definition = function(object, level = 0.95, ...) {
    hpd <- apply(X = object, MARGIN = 2, FUN = hpdi, level = level,
                 BP = is_BP(object), ..., simplify = FALSE)
    names(hpd) <- names(object)
    attr(hpd, "calendar") <- get_calendar(object)
    hpd
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
  hpdi <- do.call(rbind.data.frame, x)
  n <- vapply(X = x, FUN = nrow, FUN.VALUE = integer(1))
  hpdi$id <- rep(names(x), times = n)
  hpdi
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
