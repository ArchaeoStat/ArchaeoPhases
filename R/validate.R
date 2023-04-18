# CLASSES VALIDATION
#' @include AllClasses.R
NULL

# MCMC =========================================================================
## Events ----------------------------------------------------------------------
setValidity(
  Class = "MCMC",
  method = function(object) {
    # Get data
    events <- object@events
    calendar <- object@calendar
    hash <- object@hash
    p <- ncol(object)

    cnd <- list(
      arkhe::validate(arkhe::assert_length(events, p)),
      arkhe::validate(arkhe::assert_length(calendar, 1)),
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
    phases <- object@phases
    calendar <- object@calendar
    hash <- object@hash
    p <- ncol(object)

    cnd <- list(
      arkhe::validate(arkhe::assert_length(phases, p)),
      arkhe::validate(arkhe::assert_length(calendar, 1)),
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
    years <- object@years
    estimate <- object@estimate
    credible <- object@credible
    gauss <- object@gauss
    level <- object@level
    counts <- object@counts
    events <- object@events
    calendar <- object@calendar
    hash <- object@hash
    n <- length(years)
    g <- getOption("chronos.grid")

    cnd <- list(
      arkhe::validate(arkhe::assert_length(estimate, n)),
      # arkhe::validate(arkhe::assert_dimensions(credible, c(g, 2))),
      # arkhe::validate(arkhe::assert_dimensions(gauss, c(g, 2))),
      arkhe::validate(arkhe::assert_length(level, 1)),
      arkhe::validate(arkhe::assert_length(counts, 1)),
      arkhe::validate(arkhe::assert_length(events, 1)),
      arkhe::validate(arkhe::assert_length(calendar, 1)),
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
    years <- object@years
    estimate <- object@estimate
    calendar <- object@calendar
    hash <- object@hash
    n <- length(years)

    cnd <- list(
      arkhe::validate(arkhe::assert_length(estimate, n)),
      arkhe::validate(arkhe::assert_length(calendar, 1)),
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
    stop <- object@stop
    level <- object@level
    calendar <- object@calendar
    hash <- object@hash
    n <- length(events)

    cnd <- list(
      arkhe::validate(arkhe::assert_length(start, n)),
      arkhe::validate(arkhe::assert_length(stop, n)),
      arkhe::validate(arkhe::assert_length(level, 1)),
      arkhe::validate(arkhe::assert_length(calendar, 1)),
      arkhe::validate(arkhe::assert_length(hash, 1, empty = TRUE))
    )

    # Return cnd, if any
    arkhe::check_class(object, cnd)
  }
)

# Rate of Change ===============================================================
setValidity(
  Class = "RateOfChange",
  method = function(object) {
    # Get data
    years <- object@years
    estimate <- object@estimate
    calendar <- object@calendar
    hash <- object@hash
    n <- length(years)

    cnd <- list(
      arkhe::validate(arkhe::assert_length(estimate, n)),
      arkhe::validate(arkhe::assert_length(calendar, 1)),
      arkhe::validate(arkhe::assert_length(hash, 1, empty = TRUE))
    )

    # Return cnd, if any
    arkhe::check_class(object, cnd)
  }
)
