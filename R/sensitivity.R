# SENSITIVITY
#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname sensitivity
#' @aliases sensitivity,EventsMCMC-method
setMethod(
  f = "sensitivity",
  signature = "EventsMCMC",
  function(..., positions = NULL, level = 0.95) {
    ## Get data
    dots <- list(...)
    n <- length(dots)

    ## Validation
    dims <- vapply(X = dots, FUN = dim, FUN.VALUE = integer(2))
    # TODO: check

    ## Subset
    if (is.null(positions)) {
      positions <- seq_len(ncol(dots[[1L]]))
    }
    data <- lapply(X = dots, FUN = function(x, j) x[, j], j = positions)
    stat <- lapply(
      X = data,
      FUN = function(x, level) data.matrix(summary(x, level = level)),
      level = level
    )

    tmp <- stat[[1L]]
    min_matrix <- matrix(data = NA_real_, nrow = nrow(tmp), ncol = ncol(tmp))
    max_matrix <- min_matrix
    for (i in seq_len(n)) {
      min_matrix <- pmin(min_matrix, stat[[i]], na.rm = TRUE)
      max_matrix <- pmax(max_matrix, stat[[i]], na.rm = TRUE)
    }

    delta <- max_matrix - min_matrix
    rownames(delta) <- rownames(tmp)
    colnames(delta) <- colnames(tmp)
    as.data.frame(delta)
  }
)
