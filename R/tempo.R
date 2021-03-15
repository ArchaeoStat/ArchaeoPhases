# TEMPO PLOT
#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname tempo
#' @aliases tempo,MCMC-method
setMethod(
  f = "tempo",
  signature = "MCMC",
  definition = function(object, level = 0.95, count = TRUE, gauss = FALSE,
                        elapsed = FALSE, origin = 1, time = range(object),
                        n = 50 * ncol(object)) {
    ## Elapse
    if (elapsed) {
      if (is.null(origin))
        stop("Elapsed origin must be specified.", call. = FALSE)
      object <- object - object[, origin]
      time <- range(object) # Override user input
    }

    data_seq <- seq(from = time[[1L]], to = time[[2L]], length.out = n)

    ## Empirical cumulative distribution
    distr <- apply(
      X = object,
      MARGIN = 1,
      FUN = function(x, sequence, count, cols) {
        g <- stats::ecdf(x) # Returns a function
        y <- g(sequence)
        if (count) y <- y * cols
        y
      },
      sequence = data_seq,
      count = count,
      cols = ncol(object)
    )

    ## Mean estimate
    moy <- apply(X = distr, MARGIN = 1, FUN = mean)

    ## Credible interval
    if (gauss == TRUE) {
      ## Standard deviation
      ec <- apply(X = distr, MARGIN = 1, FUN = stats::sd)

      ## Gaussian credible intervals
      alpha <- 1 - level
      qu <- cbind(moy - stats::qnorm(1 - alpha / 2) * ec,
                  moy + stats::qnorm(1 - alpha / 2) * ec)
    }
    else {
      ## Credible intervals
      qu <- apply(X = distr, MARGIN = 1, FUN = interval_credible, level = level)
      qu <- t(qu)
    }
    colnames(qu) <- c("lower", "upper")

    .CumulativeEvents(
      year = data_seq,
      estimate = moy,
      lower = qu[, 1],
      upper = qu[, 2],
      gauss = gauss,
      level = level,
      calendar = ifelse(elapsed, "elapsed", get_calendar(object)),
      hash = get_hash(object)
    )
  }
)

#' @export
#' @rdname tempo
#' @aliases plot,CumulativeEvents,missing-method
setMethod(
  f = "plot",
  signature = c(x = "CumulativeEvents", y = "missing"),
  definition = function(x, calendar = c("BCAD", "BP")) {
    ## Validation
    calendar <- match.arg(calendar, several.ok = FALSE)
    data <- as.data.frame(x)

    ## Calendar scale
    if (get_calendar(x) == "elapsed") {
      gg_x_scale <- ggplot2::scale_x_continuous(name = "Elapsed years")
    } else {
      if (calendar == "BCAD") {
        if (get_calendar(x) == "BP") data$year <- BP_to_BCAD(data$year)
        gg_x_scale <- ggplot2::scale_x_continuous(name = "Years BC/AD")
      }
      if (calendar == "BP") {
        if (get_calendar(x) == "BCAD") data$year <- BCAD_to_BP(data$year)
        gg_x_scale <- ggplot2::scale_x_reverse(name = "Years cal BP")
      }
    }

    tempo_ci <- data.frame(
      year = c(data$year, data$year, rev(data$year)),
      ci = c(data$estimate, data$lower, rev(data$upper)),
      Legend = c(rep("Bayes estimate", nrow(data)),
                 rep("Credible interval", nrow(data) * 2))
    )

    ggplot2::ggplot(data = tempo_ci) +
      ggplot2::aes(x = .data$year, y = .data$ci, color = .data$Legend,
                   linetype = .data$Legend) +
      ggplot2::geom_path() +
      gg_x_scale +
      ggplot2::scale_y_continuous(name = "Cumulative events")
  }
)
