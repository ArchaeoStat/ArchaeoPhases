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
    ## Tempo
    tmp <- tempo(object, level = 0.95, count = FALSE, gauss = FALSE,
                 elapsed = elapsed, origin = origin, time = time, n = n)
    ## Activity
    methods::callGeneric(object = tmp)
  }
)

#' @export
#' @rdname activity
#' @aliases activity,MCMC-method
setMethod(
  f = "activity",
  signature = "CumulativeEvents",
  definition = function(object) {
    tmp <- as.data.frame(object)
    a <- tmp$estimate
    b <- tmp$year

    x <- b[-1]
    y <- diff(a) / diff(b)

    .ActivityEvents(
      year = x,
      estimate = y,
      calendar = get_calendar(object),
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

#' @export
#' @rdname tempo
#' @aliases multiplot,ActivityEvents-method
setMethod(
  f = "multiplot",
  signature = "ActivityEvents",
  definition = function(..., calendar = c("BCAD", "BP")) {
    ## Validation
    calendar <- match.arg(calendar, several.ok = FALSE)

    ## Get names
    subst <- substitute(list(...))[-1]
    arg_names <- vapply(X = subst, FUN = deparse, FUN.VALUE = character(1))

    ## Get data
    dots <- list(...)
    tmp <- lapply(X = dots, FUN = as.data.frame)
    n <- vapply(X = tmp, FUN = nrow, FUN.VALUE = integer(1))

    ## Bind data
    tmp <- do.call(rbind, tmp)
    tmp$Legend <- rep(arg_names, n)

    ## Calendar scale
    cal <- unique(lapply(X = dots, FUN = get_calendar))
    if (length(cal) != 1) {
      stop("All object must have the same calendar scale.", call. = FALSE)
    }
    if (cal == "elapsed") {
      gg_x_scale <- ggplot2::scale_x_continuous(name = "Elapsed years")
    } else {
      if (calendar == "BCAD") {
        if (cal == "BP") tmp$year <- BP_to_BCAD(tmp$year)
        gg_x_scale <- ggplot2::scale_x_continuous(name = "Years BC/AD")
      }
      if (calendar == "BP") {
        if (cal == "BCAD") tmp$year <- BCAD_to_BP(tmp$year)
        gg_x_scale <- ggplot2::scale_x_reverse(name = "Years cal BP")
      }
    }

    ggplot2::ggplot(data = tmp) +
      ggplot2::aes(x = .data$year, y = .data$estimate, color = .data$Legend) +
      ggplot2::geom_path() +
      gg_x_scale +
      ggplot2::scale_y_continuous(name = "Activity")
  }
)
