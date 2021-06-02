# ACTIVITY PLOT
#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname activity
#' @aliases activity,MCMC-method
setMethod(
  f = "activity",
  signature = "MCMC",
  definition = function(object, time = range(object), n = 50 * ncol(object),
                        progress = getOption("ArchaeoPhases.progress")) {
    ## Tempo
    tmp <- tempo(object, level = 0.95, count = FALSE, gauss = FALSE,
                 time = time, n = n, progress = progress)
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
    ## Validation
    if (object@counts) {
      stop("Tempo must be computed as probabilities.", call. = FALSE)
    }

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
  definition = function(x) {
    ## Calendar scale
    gg_x_scale <- scale_calendar(get_calendar(x))

    data <- as.data.frame(x)
    ggplot2::ggplot(data = data) +
      ggplot2::aes(x = .data$year, y = .data$estimate) +
      ggplot2::geom_area() +
      gg_x_scale +
      ggplot2::scale_y_continuous(name = "Activity")
  }
)

#' @export
#' @rdname activity
#' @aliases multiplot,ActivityEvents-method
setMethod(
  f = "multiplot",
  signature = "ActivityEvents",
  definition = function(...) {
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
    scales <- vapply(X = dots, FUN = get_calendar, FUN.VALUE = character(1))
    cal <- unique(scales)
    if (length(cal) != 1) {
      stop("All object must have the same calendar scale.", call. = FALSE)
    }
    gg_x_scale <- scale_calendar(cal)

    ggplot2::ggplot(data = tmp) +
      ggplot2::aes(x = .data$year, y = .data$estimate, color = .data$Legend) +
      ggplot2::geom_path() +
      gg_x_scale +
      ggplot2::scale_y_continuous(name = "Activity")
  }
)
