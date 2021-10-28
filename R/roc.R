# RATE OF CHANGE
#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname roc
#' @aliases roc,MCMC-method
setMethod(
  f = "roc",
  signature = "MCMC",
  definition = function(object, from = min(object), to = max(object)) {
    ## Activity
    tmp <- activity(object, from = from, to = to)
    ## ROC
    methods::callGeneric(object = tmp)
  }
)

#' @export
#' @rdname roc
#' @aliases roc,CumulativeEvents-method
setMethod(
  f = "roc",
  signature = "CumulativeEvents",
  definition = function(object) {
    ## Validation
    if (object@counts) {
      stop("Tempo must be computed as probabilities.", call. = FALSE)
    }

    ## Activity
    tmp <- activity(object)

    ## ROC
    methods::callGeneric(object = tmp)
  }
)

#' @export
#' @rdname roc
#' @aliases roc,ActivityEvents-method
setMethod(
  f = "roc",
  signature = "ActivityEvents",
  definition = function(object) {
    ## Get data
    a <- object@estimate
    b <- object@year

    x <- b[-1]
    y <- diff(a) / diff(b)

    ## Calendar scale
    if (is_BP(object)) {
      y <- y * -1
    }

    .RateOfChange(
      year = x,
      estimate = y,
      calendar = get_calendar(object),
      hash = get_hash(object)
    )
  }
)

#' @export
#' @method autoplot RateOfChange
autoplot.RateOfChange <- function(object, ..., colour = "black") {
  ## Calendar scale
  gg_x_scale <- scale_calendar(get_calendar(object))

  data <- as.data.frame(object)
  ggplot2::ggplot(data = data) +
    ggplot2::aes(x = .data$year, y = .data$estimate) +
    ggplot2::geom_hline(yintercept = 0, linetype = 2, colour = "grey") +
    ggplot2::geom_path(colour = colour) +
    gg_x_scale +
    ggplot2::scale_y_continuous(name = "Rate of change")
}

#' @export
#' @rdname roc
#' @aliases autoplot,RateOfChange-method
setMethod("autoplot", "RateOfChange", autoplot.RateOfChange)

#' @export
#' @method plot RateOfChange
plot.RateOfChange <- function(x, colour = "black", ...) {
  gg <- autoplot(object = x, ..., colour = colour) + ggplot2::theme_bw()
  print(gg)
  invisible(x)
}

#' @export
#' @rdname roc
#' @aliases plot,RateOfChange,missing-method
setMethod("plot", c(x = "RateOfChange", y = "missing"), plot.RateOfChange)
