# ACTIVITY PLOT
#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname activity
#' @aliases activity,MCMC-method
setMethod(
  f = "activity",
  signature = "MCMC",
  definition = function(object, elapsed = FALSE, origin = 1,
                        time = range(object), n = 50 * ncol(object)) {
    ## Elapse
    if (elapsed) {
      if (is.null(origin))
        stop("Elapsed origin must be specified.", call. = FALSE)
      object <- object - object[, origin]
      time <- range(object) # Override user input
    }

    data_min <- time[[1L]]
    data_max <- time[[2L]]
    x <- 10^c(0:10)

    # if (data_min != 0) {
    #   c <- sum(abs(data_min / x) > 1)
    #   if (c > 3) {
    #     data_min <- floor(data_min / x[c - 1]) * x[c - 1]
    #   }
    #   else {
    #     data_min <- floor(data_min / x[c]) * x[c]
    #   }
    # }
    #
    # if (data_max != 0) {
    #   d <- sum(abs(data_max / x) > 1)
    #   if (d > 3) {
    #     data_max <- ceiling(data_max / x[d - 1]) * x[d - 1]
    #   }
    #   else {
    #     data_max <- ceiling(data_max / x[d]) * x[d]
    #   }
    # }

    data_seq <- seq(from = data_min, to = data_max, length.out = n)

    ## Empirical cumulative distribution
    distr <- apply(
      X = object,
      MARGIN = 1,
      FUN = function(x, sequence) {
        g <- stats::ecdf(x) # Returns a function
        y <- g(sequence)
        y
      },
      sequence = data_seq
    )

    ## Mean estimate
    moy <- apply(X = distr, MARGIN = 1, FUN = mean)

    x <- data_seq[-1]
    y <- diff(moy) / diff(data_seq)

    .ActivityEvents(
      year = x,
      estimate = y,
      calendar = ifelse(elapsed, "elapsed", get_calendar(object)),
      hash = get_hash(object)
    )
  }
)

#' @export
#' @rdname activity
#' @aliases plot,ActivityEvents,missing-method
setMethod(
  f = "plot",
  signature = c(x = "ActivityEvents", y = "missing"),
  definition = function(x, calendar = c("BCAD", "BP")) {
    ## Validation
    calendar <- match.arg(calendar, several.ok = FALSE)
    data <- as.data.frame(x)

    ## Calendar scale
    if (get_calendar(x) == "elapsed") {
      gg_x_scale <- ggplot2::scale_x_continuous(name = "Elapsed years")
    } else {
      if (calendar == "BCAD") {
        if (get_calendar(x) == "BP")
          data$year <- BP_to_BCAD(data$year)
        gg_x_scale <- ggplot2::scale_x_continuous(name = "Years BC/AD")
      }
      if (calendar == "BP") {
        if (get_calendar(x) == "BCAD")
          data$year <- BCAD_to_BP(data$year)
        gg_x_scale <- ggplot2::scale_x_reverse(name = "Years cal BP")
      }
    }

    ggplot2::ggplot(data = data) +
      ggplot2::aes(x = .data$year, y = .data$estimate) +
      ggplot2::geom_area() +
      gg_x_scale +
      ggplot2::scale_y_continuous(name = "Activity")
  }
)
