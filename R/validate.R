# CLASSES VALIDATION
#' @include AllClasses.R
NULL

# MCMC =========================================================================
## Events ----------------------------------------------------------------------
setValidity(
  Class = "MCMC",
  method = function(object) {
    # Get data
    labels <- object@labels
    hash <- object@hash
    p <- ncol(object)

    cnd <- list(
      arkhe::validate(arkhe::assert_length(labels, p)),
      arkhe::validate(arkhe::assert_length(hash, 1, empty = TRUE))
    )

    # Return cnd, if any
    arkhe::check_class(object, cnd)
  }
)

## Phases ----------------------------------------------------------------------
setValidity(
  Class = "PhasesMCMC",
  method = function(object) {
    # Get data
    labels <- object@labels
    hash <- object@hash
    n <- nrow(object)
    p <- ncol(object)

    cnd <- list(
      arkhe::validate(arkhe::assert_dimensions(object, c(n, p, 2))),
      arkhe::validate(arkhe::assert_length(labels, p)),
      arkhe::validate(arkhe::assert_length(hash, 1, empty = TRUE))
    )

    # Return cnd, if any
    arkhe::check_class(object, cnd)
  }
)

# Tempo ========================================================================
setValidity(
  Class = "CumulativeEvents",
  method = function(object) {
    # Get data
    credible <- object@credible
    gauss <- object@gauss
    level <- object@level
    counts <- object@counts
    hash <- object@hash
    n <- nrow(object)

    cnd <- list(
      arkhe::validate(arkhe::assert_dimensions(object, c(n, 1, 1))),
      # arkhe::validate(arkhe::assert_dimensions(credible, c(n, 2))),
      # arkhe::validate(arkhe::assert_dimensions(gauss, c(n, 2))),
      arkhe::validate(arkhe::assert_length(level, 1)),
      arkhe::validate(arkhe::assert_length(counts, 1)),
      arkhe::validate(arkhe::assert_length(hash, 1, empty = TRUE))
    )

    # Return cnd, if any
    arkhe::check_class(object, cnd)
  }
)

# Activity =====================================================================
setValidity(
  Class = "ActivityEvents",
  method = function(object) {
    # Get data
    hash <- object@hash

    cnd <- list(
      arkhe::validate(arkhe::assert_length(hash, 1, empty = TRUE))
    )

    # Return cnd, if any
    arkhe::check_class(object, cnd)
  }
)

# Occurrence ===================================================================
setValidity(
  Class = "OccurrenceEvents",
  method = function(object) {
    # Get data
    events <- object@events
    start <- object@start
    end <- object@end
    level <- object@level
    hash <- object@hash
    n <- length(events)

    cnd <- list(
      arkhe::validate(arkhe::assert_length(start, n)),
      arkhe::validate(arkhe::assert_length(end, n)),
      arkhe::validate(arkhe::assert_length(level, 1)),
      arkhe::validate(arkhe::assert_length(hash, 1, empty = TRUE))
    )

    # Return cnd, if any
    arkhe::check_class(object, cnd)
  }
)
