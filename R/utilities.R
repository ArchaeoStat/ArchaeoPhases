# HELPERS

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

#' Reorder an MCMC Object
#'
#' @param x An [`MCMC-class`] object.
#' @param decreasing A [`logical`] scalar: should the sort order be decreasing?
#' @return A [`matrix`].
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
reorder <- function(x, decreasing = TRUE) {
  i <- order(apply(X = x, MARGIN = 2, FUN = stats::median),
             decreasing = decreasing)
  x[, i, drop = FALSE]
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

#' \pkg{ggplot2} Calendar Scale
#'
#' @param A [`character`] string specifying the calendar scale. It must be one
#'  of "`CE`", "`BP`" or "`elapsed`".
#' @seealso [ggplot2::scale_x_continuous()], [ggplot2::scale_x_reverse()]
#' @keywords internal
#' @noRd
scale_calendar <- function(x) {
  switch (
    get_calendar(x),
    CE = ggplot2::scale_x_continuous(name = "Year CE"),
    BP = ggplot2::scale_x_continuous(name = "Year BP", trans = "reverse"),
    b2k = ggplot2::scale_x_continuous(name = "Year b2k", trans = "reverse"),
    elapsed = ggplot2::scale_x_continuous(name = "Elapsed years"),
    stop(sprintf("Unknown calendar scale (%s).", x), call. = FALSE)
  )
}
