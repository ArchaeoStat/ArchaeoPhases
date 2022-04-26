# SUMMARY
#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname summary
#' @aliases summary,MCMC-method
setMethod(
  f = "summary",
  signature = "MCMC",
  definition = function(object, level = 0.95) {
    x <- apply(
      X = object,
      MARGIN = 2,
      FUN = stats_marginal,
      level = level,
      BP = is_BP(object)
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
  definition = function(object, level = 0.95) {
    pha <- as.list(object)
    k <- seq_along(pha)

    ## Reverse boundaries if BP scale
    BP <- is_BP(object)
    start <- ifelse(BP, 1, 2)
    end <- ifelse(BP, 2, 1)

    for (i in k) {
      tmp <- pha[[i]]
      tmp <- cbind(tmp[, , ], tmp[, , start] - tmp[, , end])

      tmp <- apply(
        X = tmp,
        MARGIN = 2,
        FUN = stats_marginal,
        level = level,
        BP = BP
      )
      colnames(tmp) <- c("start", "end", "duration")

      pha[[i]] <- as.data.frame(t(tmp))
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
                           credible = TRUE, level = 0.95, BP = FALSE) {
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
  if (credible) ci <- credible(x, level = level, BP = BP)[, -3]

  ## Results
  tmp <- c(mad = mod, mean = moy, sd = ec, quant, ci)
  tmp <- Filter(Negate(is.na), tmp)
  round(tmp, digits = getOption("chronos.precision"))
}

map <- function(x, ...) {
  d <- stats::density(x, ...)
  d$x[[which.max(d$y)]]
}
