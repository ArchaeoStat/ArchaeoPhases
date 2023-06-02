# OCCURRENCE PLOT
#' @include AllGenerics.R
NULL

#' @export
#' @rdname occurrence
#' @aliases occurrence,EventsMCMC-method
setMethod(
  f = "occurrence",
  signature = "EventsMCMC",
  definition = function(object, level = 0.95) {
    ## Sort rows
    sorted <- apply(X = object, MARGIN = 1, FUN = sort, decreasing = FALSE)
    sorted <- as_events(t(sorted), calendar = NULL)
    ord <- seq_len(ncol(object))

    ## Compute interval
    inter <- interval_credible(sorted, level = level, calendar = NULL)
    inter <- do.call(rbind, inter)

    .OccurrenceEvents(
      events = ord,
      start = inter[, "start"],
      end = inter[, "end"],
      level = level,
      hash = get_hash(object)
    )
  }
)
