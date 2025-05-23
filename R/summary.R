# SUMMARY
#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname summary
#' @aliases summary,MCMC-method
setMethod(
  f = "summary",
  signature = "MCMC",
  definition = function(object, level = 0.95,
                        calendar = get_calendar()) {
    x <- apply(
      X = object,
      MARGIN = 2,
      FUN = stats_marginal,
      level = level,
      calendar = calendar
    )
    as.data.frame(t(x))
  }
)

#' @export
#' @rdname summary
#' @aliases summary,PhasesMCMC-method
setMethod(
  f = "summary",
  signature = "PhasesMCMC",
  definition = function(object, level = 0.95,
                        calendar = get_calendar()) {
    m <- dim(object)[2L]
    pha <- vector(mode = "list", length = m)
    names(pha) <- dimnames(object)[[2L]]

    for (j in seq_len(m)) {
      tmp <- object[, j, , drop = TRUE]
      tmp <- cbind(tmp, tmp[, 2] - tmp[, 1])

      tmp <- apply(
        X = tmp,
        MARGIN = 2,
        FUN = stats_marginal,
        level = level,
        calendar = calendar
      )
      colnames(tmp) <- c("start", "end", "duration")

      pha[[j]] <- as.data.frame(t(tmp))
    }

    pha
  }
)

#' Marginal Statistics
#'
#' @param x A [`numeric`] vector.
#' @param mean A [`logical`] scalar: should the mean of `x` be computed?
#' @param sd A [`logical`] scalar: should the standard deviation of `x` be
#'  computed?
#' @param map A [`logical`] scalar: should the mode of `x` be computed?
#' @param quantiles A [`logical`] scalar: should the quantiles of `x` be
#'  computed?
#' @param probs A [`numeric`] vector specifying the probabilities with values in
#'  \eqn{[0,1]} (see [stats::quantile()]).
#' @param credible A [`logical`] scalar: should the credible interval of `x` be
#'  computed?
#' @param level A length-one [`numeric`] vector giving the confidence level.
#' @return A [`numeric`] vector.
#' @keywords internal
#' @noRd
stats_marginal <- function(x, mean = TRUE, sd = TRUE, map = TRUE,
                           quantiles = TRUE, probs = c(0, 0.25, 0.5, 0.75, 1),
                           credible = TRUE, level = 0.95,
                           digits = getOption("ArchaeoPhases.precision"),
                           calendar = get_calendar()) {
  ## Defaults
  moy <- mod <- quant <- ec <- ci <- NA_real_

  ## Position
  if (mean) moy <- mean(x)
  if (map) mod <- map(x)
  if (quantiles) {
    quant <- stats::quantile(x, probs = probs, names = FALSE)
    names(quant) <- c("min", "q1", "median", "q3", "max")
  }

  ## Dispersion
  if (sd) ec <- stats::sd(x)
  if (credible) {
    ci <- arkhe::interval_credible(x, level = level)
    ci <- ci[, c("start", "end")]
  }

  ## Results
  tmp <- c(mad = mod, mean = moy, sd = ec, quant, ci)
  tmp <- Filter(Negate(is.na), tmp)

  if (!is.null(calendar)) tmp <- aion::as_year(tmp, calendar = calendar)
  if (!is.null(digits)) tmp <- round(tmp, digits = digits)
  tmp
}

map <- function(x, ...) {
  d <- stats::density(x, ...)
  d$x[[which.max(d$y)]]
}
