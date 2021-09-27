# RATE OF CHANGE
#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname roc
#' @aliases roc,MCMC-method
setMethod(
  f = "roc",
  signature = "MCMC",
  definition = function(object, from = min(object), to = max(object),
                        step = (to - from) / (250 - 1)) {
    ## Activity
    tmp <- activity(object, from = from, to = to, step = step)
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
autoplot.RateOfChange <- function(object, ...) {
  ## Calendar scale
  gg_x_scale <- scale_calendar(get_calendar(object))

  data <- as.data.frame(object)
  ggplot2::ggplot(data = data) +
    ggplot2::aes(x = .data$year, y = .data$estimate) +
    ggplot2::geom_hline(yintercept = 0, linetype = 2, colour = "grey") +
    ggplot2::geom_path() +
    gg_x_scale +
    ggplot2::scale_y_continuous(name = "Rate of change")
}

#' @export
#' @rdname roc
setMethod("autoplot", "RateOfChange", autoplot.RateOfChange)

#' @export
#' @rdname roc
#' @aliases plot,RateOfChange,missing-method
setMethod(
  f = "plot",
  signature = c(x = "RateOfChange", y = "missing"),
  definition = function(x) {
    gg <- autoplot(object = x) + ggplot2::theme_bw()
    print(gg)
    invisible(x)
  }
)
