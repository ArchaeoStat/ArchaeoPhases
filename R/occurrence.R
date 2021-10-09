# OCCURRENCE PLOT
#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname occurrence
#' @aliases occurrence,MCMC-method
setMethod(
  f = "occurrence",
  signature = "MCMC",
  definition = function(object, interval = c("ci", "hpdi"), level = 0.95) {
    ## Validation
    interval <- match.arg(interval, several.ok = FALSE)

    ## Calendar scale
    desc <- ifelse(is_BP(object), TRUE, FALSE)

    ## Sort rows
    sorted <- apply(X = object, MARGIN = 1, FUN = sort, decreasing = desc)
    sorted <- as_mcmc(t(sorted))
    ord <- seq_len(ncol(object))

    ## Compute interval
    fun <- switch (
      interval,
      ci = interval_credible,
      hpdi = interval_hpd,
      stop(sprintf("There is no such interval: %s.", interval), call. = FALSE)
    )
    inter <- fun(sorted, level = level)

    ## Bind results in case of multiple intervals
    if (interval == "hpdi") {
      n <- vapply(X = inter, FUN = nrow, FUN.VALUE = integer(1))
      ord <- rep(ord, times = n)
      inter <- do.call(rbind, inter)
    }

    .OccurrenceEvents(
      events = ord,
      lower = inter[, 1],
      upper = inter[, 2],
      level = level,
      calendar = get_calendar(object),
      hash = get_hash(object)
    )
  }
)

#' @export
#' @method autoplot OccurrenceEvents
autoplot.OccurrenceEvents <- function(object, ..., colour = "black") {
  ## Calendar scale
  gg_x_scale <- scale_calendar(get_calendar(object))

  ## ggplot2
  data <- as.data.frame(object)
  ggplot2::ggplot(data = data) +
    ggplot2::aes(x = .data$lower, y = .data$events,
                 xend = .data$upper, yend = .data$events) +
    ggplot2::geom_segment(colour = colour, size = 2) +
    gg_x_scale +
    ggplot2::scale_y_continuous(name = "Occurrence")
}

#' @export
#' @rdname occurrence
#' @aliases autoplot,OccurrenceEvents-method
setMethod("autoplot", "OccurrenceEvents", autoplot.OccurrenceEvents)

#' @export
#' @method plot OccurrenceEvents
plot.OccurrenceEvents <- function(x, colour = "black", ...) {
  gg <- autoplot(object = x, ..., colour = colour) + ggplot2::theme_bw()
  print(gg)
  invisible(x)
}

#' @export
#' @rdname occurrence
#' @aliases plot,OccurrenceEvents,missing-method
setMethod("plot", c(x = "OccurrenceEvents", y = "missing"), plot.OccurrenceEvents)
