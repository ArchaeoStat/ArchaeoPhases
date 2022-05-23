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
      sprintf("- Calendar: %s", get_calendar(object)),
      sep = "\n"
    )
  }
)

# PhasesMCMC ===================================================================
setMethod(
  f = "show",
  signature = "PhasesMCMC",
  definition = function(object) {
    sep <- ifelse(is_ordered(object), " < ", " ")
    ord <- ifelse(is_ordered(object), "Ordered", "Modelled")
    pha <- get_order(object)
    ntx <- ngettext(nlevels(pha), "phase", "phases")
    cat(
      "<PhasesMCMC>",
      sprintf("- %s %s: %s", ord, ntx, paste0(pha, collapse = sep)),
      sprintf("- Calendar: %s", get_calendar(object)),
      sep = "\n"
    )
  }
)
