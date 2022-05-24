# SHOW METHODS
#' @include AllClasses.R
NULL

# MCMC =========================================================================
setMethod(
  f = "show",
  signature = "MCMC",
  definition = function(object) {
    cat(
      sprintf("<%s>", class(object)),
      sprintf("- Number of events: %d", ncol(object)),
      sprintf("- Time scale: %s", get_calendar(object)),
      sep = "\n"
    )
  }
)

# PhasesMCMC ===================================================================
setMethod(
  f = "show",
  signature = "PhasesMCMC",
  definition = function(object) {
    cat(
      sprintf("<%s>", class(object)),
      sprintf("- Number of phases: %d", ncol(object)),
      sprintf("- Time scale: %s", get_calendar(object)),
      sep = "\n"
    )
  }
)
