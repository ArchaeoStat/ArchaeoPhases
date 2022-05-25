# AGE-DEPTH MODELING
#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname bury
#' @aliases bury,EventsMCMC-method
setMethod(
  f = "bury",
  signature = c("EventsMCMC", "numeric"),
  definition = function(object, depth) {
    ## Validation
    arkhe::assert_length(depth, ncol(object))
    arkhe::assert_unique(depth)

    ## Reorder data
    index <- order(depth)
    object <- object[, index]

    curve <- apply(
      X = object,
      MARGIN = 1,
      FUN = function(x, depth) {
        dt <- data.frame(y = x, x = depth)
        stats::loess(y ~ x, data = dt, degree = 1)
      },
      depth = depth,
      simplify = FALSE
    )

    .AgeDepthModel(
      depth = depth,
      model = curve,
      calendar = get_calendar(object),
      hash = get_hash(object)
    )
  }
)

#' @export
#' @rdname bury
#' @aliases bury,AgeDepthModel-method
setMethod(
  f = "predict",
  signature = c("AgeDepthModel"),
  definition = function(object, newdata) {
    if (missing(newdata)) {
      newdata <- object@depth
    }

    age <- object@model
    a <- vapply(
      X = age,
      FUN = stats::predict,
      FUN.VALUE = numeric(length(newdata)),
      newdata = newdata
    )

    ## Event names
    event_names <- names(newdata)
    if (is.null(event_names)) event_names <- paste0("E", seq_along(newdata))

    .EventsMCMC(
      t(a),
      events = event_names,
      depth = newdata,
      calendar = get_calendar(object),
      hash = get_hash(object)
    )
  }
)

#' @export
#' @method autoplot AgeDepthModel
autoplot.AgeDepthModel <- function(object, ..., level = 0.95) {
  ## Calendar scale
  gg_x_scale <- scale_calendar(object)

  ## Get data
  depth <- object@depth
  n <- length(depth)
  data <- predict(object)
  data <- summary(data, level = level)

  ## Build table for ggplot2
  data$Depth <- depth
  data$Estimate <- "median"
  data$`Credible interval` <- paste0(round(level * 100), "%")

  ggplot2::ggplot(data = data) +
    ggplot2::geom_ribbon(
      alpha = 0.5,
      colour = "grey50",
      mapping = ggplot2::aes(
        xmin = .data$lower,
        xmax = .data$upper,
        y = .data$Depth,
        fill = .data$`Credible interval`
      )
    ) +
    ggplot2::geom_path(
      mapping = ggplot2::aes(
        x = .data$median,
        y = .data$Depth,
        colour = .data$Estimate
      )
    ) +
    gg_x_scale
}

#' @export
#' @rdname bury
#' @aliases autoplot,AgeDepthModel-method
setMethod("autoplot", "AgeDepthModel", autoplot.AgeDepthModel)

#' @export
#' @method plot AgeDepthModel
plot.AgeDepthModel <- function(x, level = 0.95, ...) {
  gg <- autoplot(object = x, ..., level = level) +
    ggplot2::scale_colour_manual(values = c("#004488", "#BB5566", "#DDAA33")) +
    ggplot2::scale_fill_manual(values = "grey50") +
    ggplot2::scale_y_reverse() +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom")
  print(gg)
  invisible(x)
}

#' @export
#' @rdname bury
#' @aliases plot,AgeDepthModel,missing-method
setMethod("plot", c(x = "AgeDepthModel", y = "missing"), plot.AgeDepthModel)
