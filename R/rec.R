# RADIOCARBON EVENT COUNT ENSEMBLE
#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname coerce
#' @aliases as_rece,matrix-method
setMethod(
  f = "as_rece",
  signature = "matrix",
  definition = function(from, calendar = c("BP", "CE", "b2k"), age = 1) {
    ## Validation
    calendar <- match.arg(calendar, several.ok = FALSE)

    ## Get ages
    n_age <- length(age)
    n_row <- nrow(from)
    if (n_age == 1) {
      ages <- from[, age]
      from <- from[, -age]
    } else if (n_age != n_row) {
      arkhe::assert_length(age, n_row)
    }

    ## Event names
    event_names <- colnames(from)
    if (is.null(event_names)) event_names <- paste0("E", seq_len(ncol(from)))

    ## Return an RECE object
    .RECE(
      from,
      events = event_names,
      year = ages,
      calendar = calendar
    )
  }
)

#' @export
#' @rdname coerce
#' @aliases as_rece,data.frame-method
setMethod(
  f = "as_rece",
  signature = "data.frame",
  definition = function(from, calendar = c("BP", "CE", "b2k"), age = 1) {
    from <- data.matrix(from)
    methods::callGeneric(from = from, calendar = calendar, age = age)
  }
)

#' @export
#' @rdname rec
#' @aliases rece,EventsMCMC-method
setMethod(
  f = "rece",
  signature = "EventsMCMC",
  definition = function(object, resolution = 1, n = 100) {

    data <- object[sample(nrow(object), size = n, replace = FALSE), ]
    data <- data - data %% resolution

    count <- vector(mode = "list", length = n)
    n_seq <- seq_len(n)
    for (i in n_seq) {
      tmp <- as.data.frame(table(data[i, ]))
      colnames(tmp) <- c("age", paste0("C", i))
      count[[i]] <- tmp
    }

    count <- Reduce(
      f = function(df1, df2) {
        merge(df1, df2, by = "age", all = TRUE)
      },
      x = count
    )

    ## Get ages (merge() coerce joining column to factor)
    age <- as.numeric(as.character(count$age))

    ## Align on a regularly spaced grid
    grid <- seq(from = min(age), to = max(age), by = resolution)
    count <- merge(data.frame(age = grid), count, by = "age", all = TRUE)

    ## Remove age column and coerce to matrix
    count[is.na(count)] <- 0
    count <- as.matrix(count[, -1])

    ## Return an RECE object
    .RECE(
      count,
      events = names(object),
      year = grid,
      calendar = get_calendar(object),
      hash = get_hash(object)
    )
  }
)

#' @export
#' @method autoplot RECE
autoplot.RECE <- function(object, ...) {
  ## Calendar scale
  gg_x_scale <- scale_calendar(object)

  ## Binary array
  bin <- array(FALSE, dim = c(nrow(object), max(object), ncol(object)))
  for (j in seq_len(ncol(object))) {
    x <- object[, j]
    for (i in seq_along(x)) {
      bin[i, x[i], j] <- x[i] > 0
    }
  }
  bin <- apply(X = bin, MARGIN = c(1, 2), FUN = sum)
  bin <- data.frame(
    Age = object@year,
    Count = rep(1:max(object), each = nrow(object)),
    n = as.vector(bin),
    row.names = NULL
  )
  bin <- bin[bin$n != 0, ]

  ## Adjust for geom_tile
  res <- mean(diff(object@year)) / 2

  ggplot2::ggplot(data = bin) +
    ggplot2::aes(xmin = .data$Age - res, xmax = .data$Age + res,
                 ymin = .data$Count - 1, ymax = .data$Count,
                 fill = .data$n) +
    ggplot2::geom_rect() +
    gg_x_scale
}

#' @export
#' @rdname rec
#' @aliases autoplot,RECE-method
setMethod("autoplot", "RECE", autoplot.RECE)

#' @export
#' @method plot RECE
plot.RECE <- function(x, ...) {
  gg <- autoplot(object = x, ...) +
    ggplot2::scale_fill_viridis_c(
      name = "RECE Agreement",
      option = "B",
      begin = 0.15,
      alpha = 0.9,
      trans = "log"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.title = ggplot2::element_text(angle = 270)) +
    ggplot2::guides(
      fill = ggplot2::guide_colorbar(
        title.position = "right",
        label = FALSE
      )
    )
  print(gg)
  invisible(x)
}

#' @export
#' @rdname rec
#' @aliases plot,RECE,missing-method
setMethod("plot", c(x = "RECE", y = "missing"), plot.RECE)
