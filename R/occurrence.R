# OCCURRENCE PLOT
#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname occurrence
#' @aliases occurrence,MCMC-method
setMethod(
  f = "occurrence",
  signature = "MCMC",
  definition = function(object, interval = c("ci", "hpdi"), level = 0.95,
                        elapsed = FALSE, origin = 1) {
    ## Validation
    interval <- match.arg(interval, several.ok = FALSE)

    ## Elapse
    if (elapsed) {
      if (is.null(origin))
        stop("Elapsed origin must be specified.", call. = FALSE)
      object <- object - object[, origin]
    }

    ## Sort rows
    sorted <- apply(X = object, MARGIN = 1, FUN = sort, decreasing = FALSE)
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
      calendar = ifelse(elapsed, "elapsed", get_calendar(object)),
      hash = get_hash(object)
    )
  }
)

#' @export
#' @rdname occurrence
#' @aliases plot,OccurrenceEvents,missing-method
setMethod(
  f = "plot",
  signature = c(x = "OccurrenceEvents", y = "missing"),
  definition = function(x, calendar = c("BCAD", "BP")) {
    ## Validation
    calendar <- match.arg(calendar, several.ok = FALSE)
    data <- as.data.frame(x)

    ## Calendar scale
    if (get_calendar(x) == "elapsed") {
      gg_x_scale <- ggplot2::scale_x_continuous(name = "Elapsed years")
    } else {
      if (calendar == "BCAD") {
        if (get_calendar(x) == "BP") {
          data$lower <- BP_to_BCAD(data$lower)
          data$upper <- BP_to_BCAD(data$upper)
        }
        gg_x_scale <- ggplot2::scale_x_continuous(name = "Years BC/AD")
      }
      if (calendar == "BP") {
        if (get_calendar(x) == "BCAD") {
          data$lower <- BCAD_to_BP(data$lower)
          data$upper <- BCAD_to_BP(data$upper)
        }
        gg_x_scale <- ggplot2::scale_x_reverse(name = "Years cal BP")
      }
    }

    ## ggplot2
    ggplot2::ggplot(data = data) +
      ggplot2::aes(x = .data$lower, y = .data$events,
                   xend = .data$upper, yend = .data$events) +
      ggplot2::geom_segment(size = 2) +
      gg_x_scale +
      ggplot2::scale_y_continuous(name = "Occurrence")
  }
)
