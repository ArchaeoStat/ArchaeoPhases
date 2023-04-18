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
    ## Calendar scale
    desc <- is_BP(object) || is_b2k(object)

    ## Sort rows
    sorted <- apply(X = object, MARGIN = 1, FUN = sort, decreasing = desc)
    sorted <- as_events(t(sorted))
    ord <- seq_len(ncol(object))

    ## Compute interval
    inter <- credible(sorted, level = level)
    inter <- do.call(rbind, inter)

    .OccurrenceEvents(
      events = ord,
      start = inter[, "start"],
      stop = inter[, "stop"],
      level = level,
      calendar = get_calendar(object),
      hash = get_hash(object)
    )
  }
)
