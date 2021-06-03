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

stats_marginal <- function(x, level = 0.95, BP = FALSE) {
  ## Position
  moy <- mean(x)
  quant <- stats::quantile(x, c(0, 0.25, 0.5, 0.75, 1), names = FALSE)
  names(quant) <- c("min", "q1", "median", "q3", "max")
  ## Dispersion
  ec <- stats::sd(x)
  ci <- interval_credible(x, level = level)
  ## Reverse boundaries if BP scale
  if (BP) {
    ci <- ci[c(2, 1)]
  }
  names(ci) <- c("CI_lower", "CI_upper")
  ## Results
  tmp <- c(mean = moy, sd = ec, quant, ci)
  round(tmp, digits = getOption("ArchaeoPhases.precision"))
}
