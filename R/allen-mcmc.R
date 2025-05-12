# ALLEN
#' @include AllGenerics.R
NULL

# Illustrate ===================================================================
#' Illustrate Basic and Composite Allen Relations
#'
#' @param relations A [`character`] string specifying the relation.
#'  It must be one of "`basic`", "`concurrent`", "`distinct`",
#'  "`stratigraphic`", "`branching`", "`transformation`", "`reticulation`",
#'  "`sequence`", "`branch`", "`transform`", or "`reticulate`" (see details).
#' @param ... Further arguments to be passed to internal methods.
#' @details
#'  Illustrate basic and composite Allen relations for several chronological
#'  model domains with a Nokel lattice. Chronological model domains include
#'  stratigraphy and branching, transformative, and reticulate processes of
#'  artifact change.
#'
#'  The illustrative graphics include:
#'  \describe{
#'   \item{`basic`}{the 13 basic Allen relations (default);}
#'   \item{`concurrent`}{concurrent relations;}
#'   \item{`distinct`}{relations with distinct endpoints;}
#'   \item{`stratigraphic`}{basic relations established by an observation of
#'   superposition;}
#'   \item{`branching`}{basic branching relations;}
#'   \item{`transformation`}{basic relations of transformation;}
#'   \item{`reticulation`}{basic relations of reticulation;}
#'   \item{`sequence`}{composite relations in a stratigraphic sequence;}
#'   \item{`branch`}{composite relations of branching;}
#'   \item{`transform`}{composite relations of transformation; or}
#'   \item{`reticulate`}{composite relations of reticulation.}
#'  }
#' @return
#'  `allen_illustrate()` is called it for its side-effects: it results in a
#'  graphic being displayed.
#' @references
#'  Harris, E. C. (1997). *Principles of Archaeological Stratigraphy*.
#'  Second edition. London: Academic Press.
#'
#'  Lyman, R. L. and O'Brien, M. J. (2017). "Sedation and Cladistics: The
#'  Difference between Anagenetic and Cladogenetic Evolution". In *Mapping Our
#'  Ancestors: Phylogenetic Approaches in Anthropology and Prehistory*,
#'  edited by Lipo, C. P., O'Brien, M. J., Couard, M., and Shennan,
#'  S. J. New York: Routledge. \doi{10.4324/9780203786376}.
#'
#'  Viola, T. (2020). *Peirce on the Uses of History*. De Gruyter.
#'  \doi{10.1515/9783110651560}. See chapter 3, "Historicity as Process",
#'  especially p. 83-88.
#' @examples
#' ## Plot the basic Allen relations
#' allen_illustrate()
#' @family Allen's intervals
#' @author T. S. Dye
#' @export
allen_illustrate <- function(relations = "basic", ...) {
  main <- switch(
    relations,
    basic = "Basic Allen relations",
    concurrent = "Concurrent relations",
    distinct = "Relations with distinct endpoints",
    stratigraphic = "Basic stratigraphic relations",
    branching = "Basic branching relations",
    transformation = "Basic transformation relations",
    reticulation = "Basic reticulation relations",
    sequence = "Composite stratigraphic relation",
    branch = "Composite branching relation",
    transform = "Composite transformation relation",
    reticulate = "Composite reticulation relation",
    stop(sprintf("Unknown relation, %s.", sQuote(relations)))
  )
  allen_plot(allen_illustrate_relations(relations), main = main, ...)
}

#' Data for an Illustrative Graphic
#'
#' Create a data frame that can be used as input for an illustrative plot.
#' Useful for describing the Allen operators: illustrate the full
#' set of Allen relations, concurrent Allen relations, and relations with
#' distinct endpoints. Also, useful for describing the chronological domains of
#' stratification, branching, transformation, and reticulation.
#' @param relations A [`character`] string specifying the relation.
#'  It must be one of "`basic`", "`concurrent`", "`distinct`",
#'  "`stratigraphic`", "`branching`", "`transformation`", "`reticulation`",
#'  "`sequence`", "`branch`", "`transform`", or "`reticulate`" (see details).
#' @details
#'  The illustrative graphics include:
#'  \describe{
#'   \item{`basic`}{the 13 basic Allen relations (default);}
#'   \item{`concurrent`}{concurrent relations;}
#'   \item{`distinct`}{relations with distinct endpoints;}
#'   \item{`stratigraphic`}{basic relations established by an observation of superposition;}
#'   \item{`branching`}{basic branching relations;}
#'   \item{`transformation`}{basic relations of transformation;}
#'   \item{`reticulation`}{basic relations of reticulation;}
#'   \item{`sequence`}{composite relations in a stratigraphic sequence;}
#'   \item{`branch`}{composite relations of branching;}
#'   \item{`transform`}{composite relations of transformation; or}
#'   \item{`reticulate`}{composite relations of reticulation.}
#'  }
#' @return
#'  A `data.frame` to be passed to [allen_plot()].
#' @author T. S. Dye
#' @family Allen's intervals
#' @keywords internal
allen_illustrate_relations <- function(relations = "basic") {
  result <- switch(
    relations,
    basic = allen_relation_code(),
    concurrent = allen_relation_concurrent(),
    distinct = allen_relation_distinct(),
    stratigraphic = "mM",
    branching = "OfdoFD",
    transformation = "mM",
    reticulation = "moFDdfOM",
    sequence = allen_composition("m", "m"),
    branch = allen_composition("Ofd", "oFD"),
    transform = allen_composition("m", "m"),
    reticulate = allen_composition("mDFo", "MdfO"),
    stop(sprintf("Unknown relation, %s.", sQuote(relations)))
  )

  merge(
    x = allen_count(result),
    y = allen_table(),
    by = "code",
    all.x = FALSE,
    all.y = TRUE,
    sort = FALSE
  )
}

# Analyse ======================================================================
#' Analyze Composite Allen Relations
#'
#' Visualize composite Allen relations with a Nokel lattice.
#' @param x,y A [`character`] string denoting an Allen relation.
#' @param ... Further arguments to be passed to internal methods.
#' @return
#'  `allen_analyze()` is called it for its side-effects: it results in a
#'  graphic being displayed.
#' @examples
#' allen_analyze("mDFo", "MdfO", main = "Composite reticulation relation")
#' @family Allen's intervals
#' @author T. S. Dye
#' @export
allen_analyze <- function(x, y, ...) {
  allen_plot(allen_analyze_relations(x, y), ...)
}

#' Data for an Analytic Graphic
#'
#' Calculates the Allen composition of two relations.
#' @param x,y A [`character`] string denoting an Allen relation.
#' @return
#'  A `data.frame` to be passed to [allen_plot()].
#' @author T. S. Dye
#' @family Allen's intervals
#' @keywords internal
allen_analyze_relations <- function(x, y) {
  comp <- allen_composition(x, y)
  merge(
    x = allen_count(comp),
    y = allen_table(),
    by = "code",
    all.x = FALSE,
    all.y = TRUE,
    sort = FALSE
  )
}

# Observe ======================================================================
#' @export
#' @rdname allen_observe
#' @aliases allen_observe,PhasesMCMC,missing-method
setMethod(
  f = "allen_observe",
  signature = c(x = "PhasesMCMC", groups = "missing"),
  definition = function(x, converse = TRUE, ...) {
    result <- allen_relate_intervals(mcmc = x, converse = converse)
    n <- length(result)

    if (n > 1) {
      ## Graphical parameters
      n_col <- 2 # if (n > 4) 2 else 1
      n_row <- ceiling(n / n_col)

      ## Save and restore
      old_par <- graphics::par(
        mar = c(1, 1, 3, 1),
        oma = c(0, 0, 0, 0),
        mfcol = c(n_row, n_col)
      )
      on.exit(graphics::par(old_par))
    }

    for (i in seq_len(n)) {
      allen_plot(result[[i]], ...)
      graphics::box()
      graphics::mtext(names(result)[[i]], side = 3)
    }

    invisible(x)
  }
)

#' @export
#' @rdname allen_observe
#' @aliases allen_observe,EventsMCMC,list-method
setMethod(
  f = "allen_observe",
  signature = c(x = "EventsMCMC", groups = "list"),
  definition = function(x, groups, converse = TRUE, ...) {
    ## Build phases
    groups <- unlist(groups, recursive = FALSE)
    groups <- groups[!duplicated(groups)]
    pha <- phases(x, groups = groups)

    methods::callGeneric(x = pha, converse = converse, ...)
  }
)

#' @export
#' @rdname allen_observe_frequency
#' @aliases allen_observe_frequency,PhasesMCMC,missing-method
setMethod(
  f = "allen_observe_frequency",
  signature = c(x = "PhasesMCMC", groups = "missing"),
  definition = function(x, set) {

    interval <- allen_relate_intervals(mcmc = x, converse = TRUE)

    foo <- vapply(
      X = interval,
      FUN = function(x, relations) sum(x$result[x$code %in% relations]),
      FUN.VALUE = numeric(1),
      relations = allen_string_to_set(set)
    )

    ## Build matrix
    n_phases <- NCOL(x)
    if (n_phases == 1) {
      p_phases <- matrix(1, nrow = 2, ncol = 1)
    } else {
      p_phases <- utils::combn(n_phases, m = 2)
    }
    p_phases <- cbind(p_phases, p_phases[c(2, 1), ])
    res <- matrix(NA_real_, nrow = n_phases, ncol = n_phases,
                  dimnames = list(names(x), names(x)))

    ## Fill matrix
    for (i in seq_len(ncol(p_phases))) {
      res[p_phases[1, i], p_phases[2, i]] <- foo[[i]]
    }

    res
  }
)

#' @export
#' @rdname allen_observe_frequency
#' @aliases allen_observe_frequency,EventsMCMC,list-method
setMethod(
  f = "allen_observe_frequency",
  signature = c(x = "EventsMCMC", groups = "list"),
  definition = function(x, groups, ...) {
    ## Build phases
    groups <- unlist(groups, recursive = FALSE)
    groups <- groups[!duplicated(groups)]
    pha <- phases(x, groups = groups)

    methods::callGeneric(x = pha, ...)
  }
)

#' Relate Two or More Observed Intervals
#'
#' Reads MCMC output to create a data frame suitable for plotting the observed
#' Allen relation of two intervals.
#' @param mcmc An [`EventsMCMC-class`] object containing the output of the
#'  MCMC algorithm.
#' @param converse A [`logical`] scalar: should converse relations be observed?
#' @return
#'  A `list` of `data.frame` to be passed to [allen_plot()].
#' @author T. S. Dye, N. Frerebeau
#' @keywords internal
allen_relate_intervals <- function(mcmc, converse = TRUE) {
  n_phases <- NCOL(mcmc)
  n_iter <- NROW(mcmc)

  ## For each pair of phases
  if (n_phases == 1) {
    p_phases <- matrix(1, nrow = 2, ncol = 1)
    converse <- FALSE
  } else {
    p_phases <- utils::combn(n_phases, m = 2)
  }

  if (isTRUE(converse)) {
    p_phases <- cbind(p_phases, p_phases[c(2, 1), ])
  }

  n_pairs <- NCOL(p_phases)
  result_title <- vector(mode = "character", length = n_pairs)
  result_pair <- vector(mode = "list", length = n_pairs)

  for (k in seq_len(n_pairs)) {
    p <- p_phases[, k]
    phase1 <- names(mcmc)[p[1L]]
    phase2 <- names(mcmc)[p[2L]]

    ## For each MCMC iteration
    result_rel <- vector(mode = "character", length = n_iter)
    for (i in seq_len(n_iter)) {
      mcmc1 <- mcmc[i, p[1L], , drop = TRUE]
      mcmc2 <- mcmc[i, p[2L], , drop = TRUE]
      rel <- allen_relation(
        x = c(mcmc1[1], mcmc2[1]),
        y = c(mcmc1[2], mcmc2[2])
      )
      result_rel[[i]] <- rel[1, 2]
    }

    prop <- allen_count(result_rel, count = FALSE)
    code <- paste0(prop$code[prop$result > 0], collapse = "")

    result_title[[k]] <- sprintf("%s(%s)%s", phase1, code, phase2)
    result_pair[[k]] <- merge(x = prop, y = allen_table(), by = "code",
                              all.x = FALSE, all.y = TRUE, sort = FALSE)
  }

  names(result_pair) <- result_title
  result_pair
}

# Concur =======================================================================
#' @export
#' @rdname allen_joint_concurrency
#' @aliases allen_joint_concurrency,EventsMCMC,list-method
setMethod(
  f = "allen_joint_concurrency",
  signature = c(x = "EventsMCMC", groups = "list"),
  definition = function(x, groups, ...) {

    get_joint_start <- function(step, groups) {
      start <- -.Machine$integer.max
      for(x in seq_along(groups))start <- max(start, min(step[groups[[x]]]))
      start
    }

    get_joint_end <- function(step, groups) {
      end <- .Machine$integer.max
      for(x in seq_along(groups)) end <- min(end, max(step[groups[[x]]]))
      end
    }

    start <- apply(X = x, MARGIN = 1, FUN = get_joint_start, groups = groups)
    end <- apply(X = x, MARGIN = 1, FUN = get_joint_end, groups = groups)

    res <- cbind(start = start, end = end)
    as_phases(res, calendar = NULL)
  }
)

# Plot =========================================================================
#' Make a Single Plot of a Nökel Lattice.
#'
#' Plots a Nökel lattice.
#' @param x A [`data.frame`] with plot information, such as the one
#' produced by [allen_illustrate_relations()].
#' @param main A [`character`] string giving a main title for the plot.
#' @param sub A [`character`] string giving a subtitle for the plot.
#' @param ann A [`logical`] scalar: should the default annotation appear on the
#'  plot?
#' @param ... Currently not used.
#' @return
#'   `allen_plot()` is called it for its side-effects: it results in a graphic
#'   being displayed (invisibly returns `x`).
#' @author T. S. Dye, N. Frerebeau
#' @family Allen's intervals
#' @keywords internal
allen_plot <- function(x, main = NULL, sub = NULL,
                       ann = graphics::par("ann"), ...) {
  ## Graphical parameters
  cex <- list(...)$cex %||% graphics::par("cex")
  col <- list(...)$col %||% graphics::par("col")
  col <- grDevices::colorRamp(col)(x$result)
  col <- grDevices::rgb(col[, 1], col[, 2], col[, 3], maxColorValue = 255)
  col <- mapply(FUN = grDevices::adjustcolor, col = col, alpha = map_alpha(x$result))

  ## Save and restore graphical parameters
  # old_par <- graphics::par(no.readonly = TRUE)
  # on.exit(graphics::par(old_par), add = TRUE)

  ## Open new window
  grDevices::dev.hold()
  on.exit(grDevices::dev.flush(), add = TRUE)
  graphics::plot.new()

  ## Set plotting coordinates
  xlim <- range(x$x)
  ylim <- range(x$y)
  xlim <- xlim + c(-0.5, 0.5) * max(graphics::strwidth(x$node), cex = cex)
  graphics::plot.window(xlim = xlim, ylim = ylim)

  ## Evaluate pre-plot expressions
  # Nothing to do

  ## Plot
  graphics::text(x = x$x, y = x$y, labels = x$node,
                 col = col, ...)

  ## Evaluate post-plot and pre-axis expressions
  # Nothing to do

  ## Construct Axis
  # Nothing to do

  ## Plot frame
  # Nothing to do

  ## Add annotation
  if (ann) {
    graphics::title(main = main, sub = sub, xlab = NULL, ylab = NULL)
  }

  invisible(x)
}

# Helpers ======================================================================
#' Count Allen Relations
#'
#' @return A [`data.frame`] with two columns: "`code`" and "`result`".
#' @author N. Frerebeau
#' @keywords internal
allen_count <- function(x, count = TRUE) {
  set <- allen_string_to_set(x, factor = TRUE)
  tbl <- table(set, dnn = list("code"))
  df <- as.data.frame(tbl, responseName = "result")
  if (isFALSE(count)) df$result <- df$result / sum(df$result, na.rm = TRUE)
  df
}

#' Table of Allen Relations
#'
#' @return A [`data.frame`].
#' @author N. Frerebeau
#' @keywords internal
allen_table <- function(...) {
  data.frame(
    code = allen_relation_code(),
    node = allen_relation_string(),
    x = allen_lattice_x(),
    y = allen_lattice_y()
  )
}

#' Convert a String to a Relation Set
#'
#' Converts a string containing Allen relation codes to a relation set.
#' @param x A [`character`] string with Allen relation codes.
#' @param factor A [`logical`] scalar: should character string be coerced to
#'  [`factor`]?
#' @details
#'  Characters in the string that are not Allen relation codes are not
#'  identified and are added to the set.
#' @return A [`character`] vector of single letter Allen relation codes.
#' @author T. S. Dye
#' @keywords internal
#' @noRd
allen_string_to_set <- function(x, factor = FALSE) {
  res <- strsplit(x, "")
  res <- unlist(res)

  if (isTRUE(factor))
    res <- factor(res, levels = allen_relation_code())

  res
}

#' Nokel Lattice x Coordinates
#'
#' A vector of arbitrary coordinates for lattice node placement.
#' @param ... Currently not used.
#' @return An [`integer`] vector.
#' @author T. S. Dye
#' @keywords internal
#' @noRd
allen_lattice_x <- function(...) {
  c(0L, 0L, 0L, -1L, -2L, 1L, 0L, -1L, 2L, 1L, 0L, 0L, 0L)
}

#' Nokel Lattice y Coordinates
#'
#' A vector of arbitrary coordinates for lattice node placement.
#' @param ... Currently not used.
#' @return An [`integer`] vector.
#' @author T. S. Dye
#' @keywords internal
#' @noRd
allen_lattice_y <- function(...) {
  c(8L, 7L, 6L, 5L, 4L, 5L, 4L, 3L, 4L, 3L, 2L, 1L, 0L)
}
