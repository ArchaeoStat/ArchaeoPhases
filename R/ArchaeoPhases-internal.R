# HELPERS

## https://michaelchirico.github.io/potools/articles/developers.html
tr_ <- function(...) {
  enc2utf8(gettext(paste0(...), domain = "R-aion"))
}

# periode <- Vectorize(
#   function(epsilon, p1, p2, level) {
#     # Computes the 'level'th quantile of the minimum of the events included in the phase
#     q1 <- stats::quantile(p1, probs = epsilon)
#     ind <- p1 > q1
#     q2 <- stats::quantile(p2[ind], probs = level / (1 - epsilon))
#     c(lower = q1, upper = q2)
#   },
#   "epsilon"
# )
periode <- function(epsilon, p1, p2, level) {
  vapply(
    X = epsilon,
    FUN = function(x, p1, p2, level) {
      q1 <- stats::quantile(p1, probs = x)
      ind <- p1 > q1
      q2 <- stats::quantile(p2[ind], probs = level / (1 - x))
      c(q1, q2)
    },
    FUN.VALUE = numeric(2),
    p1 = p1,
    p2 = p2,
    level = level
  )
}

# gap <- Vectorize(
#   function(epsilon, p1, p2, level) {
#     prob <- 1 - epsilon
#     q1 <- stats::quantile(p1, probs = prob)
#     ind <- p1 < q1
#     q2 <- stats::quantile(p2[ind], probs = (prob - level) / prob)
#     c(q1, q2)
#   },
#   "epsilon"
# )
gap <- function(epsilon, p1, p2, level) {
  vapply(
    X = epsilon,
    FUN = function(x, p1, p2, level) {
      prob <- 1 - x
      q1 <- stats::quantile(p1, probs = prob)
      ind <- p1 < q1
      q2 <- stats::quantile(p2[ind], probs = (prob - level) / prob)
      c(q1, q2)
    },
    FUN.VALUE = numeric(2),
    p1 = p1,
    p2 = p2,
    level = level
  )
}

#' Calculate hash
#'
#' @param file A [`character`] string specifying the name of the file.
#' @param download A [`logical`] scalar: should the remote file be downloaded
#'  and hashed locally?
#' @return A [`character`] string of a fixed length.
#' @author T. S. Dye, N. Frerebeau
#' @keywords internal
#' @noRd
make_hash <- function(file, download = TRUE) {
  ## If remote, save temp file
  if (!utils::file_test("-f", file) && download) {
    temp_file <- tempfile()
    utils::download.file(file, temp_file) # Download

    file_hash <- tools::md5sum(temp_file) # Hash
    unlink(temp_file)
  }
  else {
    file_hash <- tools::md5sum(file)
  }
  unname(file_hash)
}

#' Map Alpha to a Variable
#'
#' @param x A [`numeric`] vector.
#' @param level An [`integer`] specifying the number of alpha levels.
#' @return A [`numeric`] vector.
#' @keywords internal
#' @noRd
map_alpha <- function(x, levels = 5) {
  if (arkhe::is_constant(x, na.rm = TRUE)) return(rep(1, length(x)))

  ## Define categories
  brk <- seq(min(x) - 0.1, max(x) + 0.1, length.out = levels)

  ## Define number of alpha groups needed to fill these
  cats <- nlevels(cut(x, breaks = brk))

  ## Map
  map <- cut(x, breaks = brk, labels = seq(0.3, 1, len = cats))
  as.numeric(as.character(map))
}

get_par <- function(params, x, n = 0) {
  p <- params[[x]] %||% graphics::par()[[x]]
  if (n > 0) p <- rep(p, length.out = n)
  p
}

#' Compute x Limits
#'
#' Computes x limits for a time series according to a given calendar.
#' This ensures that the x axis is always in chronological order.
#' @param x A [`TimeSeries-class`] object.
#' @param calendar A [`TimeScale-class`] object.
#' @return A length-two [`numeric`] vector.
#' @keywords internal
#' @noRd
xlim <- function(x, calendar) {
  if (methods::is(x, "TimeSeries"))
    x <- aion::time(x, calendar = NULL)

  x <-  range(x)
  if (is.null(calendar)) return(x)
  aion::as_year(x, calendar = calendar)
}

plot_x_ribbon <- function(xmin, xmax, y, ...) {
  graphics::polygon(x = c(xmin, rev(xmax)), y = c(y, rev(y)), ...)
}
plot_y_ribbon <- function(x, ymin, ymax, ...) {
  graphics::polygon(x = c(x, rev(x)), y = c(ymin, rev(ymax)), ...)
}
