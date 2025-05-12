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
  definition = function(x, level = 0.95, calendar = get_calendar()) {
    cred <- apply(X = x, MARGIN = 2, FUN = arkhe::interval_credible,
                  level = level, simplify = FALSE)
    names(cred) <- names(x)
    if (is.null(calendar)) return(cred)
    lapply(
      X = cred,
      FUN = function(x, calendar) {
        x[, 1] <- aion::as_year(x[, 1], calendar = calendar)
        x[, 2] <- aion::as_year(x[, 2], calendar = calendar)
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
  definition = function(x, level = 0.95, calendar = get_calendar(), ...) {
    hpd <- apply(X = x, MARGIN = 2, FUN = arkhe::interval_hdr,
                 level = level, ..., simplify = FALSE)
    names(hpd) <- names(x)
    if (is.null(calendar)) return(hpd)
    lapply(
      X = hpd,
      FUN = function(x, calendar) {
        x[, 1] <- aion::as_year(x[, 1], calendar = calendar)
        x[, 2] <- aion::as_year(x[, 2], calendar = calendar)
        x
      },
      calendar = calendar
    )
  }
)
