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

#' Scales
#'
#' @param x A \code{\link{numeric}} vector.
#' @keywords internal
#' @noRd
scale_01 <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

#' Calculate hash
#'
#' @param file A \code{\link{character}} string specifying the name of the file.
#' @param algo A \code{\link{character}} string specifying the algorithms to be
#' used (see \code{\link[digest]{digest}}).
#' @param ... Extra parameters to be passed to [digest::digest()].
#' @return A \code{\link{character}} string of a fixed length.
#' @author Thomas S. Dye
#' @keywords internal
#' @noRd
make_hash <- function(file, algo = "sha256", ...) {
  ## If connection, save temp file
  if (!utils::file_test("-f", file)) {
    temp_file <- tempfile(pattern = "", fileext = "csv")
    write(file, temp_file)

    file_hash <- digest::digest(file = temp_file, algo = algo, ...)
    unlink(temp_file)
  }
  else {
    file_hash <- digest::digest(file = file, algo = algo, ...)
  }
  file_hash
}
