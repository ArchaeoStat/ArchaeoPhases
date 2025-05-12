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
      sprintf(tr_("- Number of events: %d"), ncol(object)),
      sprintf(tr_("- Number of MCMC samples: %s"), nrow(object)),
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
      sprintf(tr_("- Number of phases: %d"), ncol(object)),
      sprintf(tr_("- Number of MCMC samples: %s"), nrow(object)),
      sep = "\n"
    )
  }
)
