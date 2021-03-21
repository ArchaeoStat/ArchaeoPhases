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
      level = level
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
    phases <- as.list(object)
    k <- seq_along(phases)
    for (i in k) {
      tmp <- phases[[i]]
      tmp <- cbind(tmp, tmp[, 2] - tmp[, 1])
      tmp <- methods::callNextMethod(object = tmp, level = level)
      row.names(tmp) <- c("start", "end", "duration")
      phases[[i]] <- tmp
    }
    phases
  }
)

stats_marginal <- function(x, level = 0.95) {
  ## Position
  moy <- mean(x)
  quant <- stats::quantile(x, c(0, 0.25, 0.5, 0.75, 1), names = FALSE)
  names(quant) <- c("min", "q1", "median", "q3", "max")
  ## Dispersion
  ec <- stats::sd(x)
  ci <- interval_credible(x, level = level)
  names(ci) <- c("CI_lower", "CI_upper")
  ## Results
  tmp <- c(mean = moy, sd = ec, quant, ci)
  round(tmp, digits = getOption("ArchaeoPhases.precision"))
}
