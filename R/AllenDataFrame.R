#' Relate two or more observed intervals
#'
#' Reads MCMC output to create a dataframe suitable
#' for plotting the observed Allen relation of two
#' intervals.
#'
#' @author Thomas S. Dye
#'
#' @param mcmc Dataframe or archaeophases_mcmc object with the MCMC output
#' from a Bayesian calibration.
#' @param chains a list of lists, each with two named elements,
#' each element a vector of names or indexes of columns in \code{mcmc}.
#'
#' @return A dataframe suitable for plotting with \code{allen_plot}.
#'
allen_relate_intervals <- function(mcmc, chains) {

  if(!is.data.frame(mcmc))
    stop("The 'mcmc' parameter must be a dataframe.")

  if(!is.list(chains))
    stop("The 'chains' parameter must be a list.")

  is.named <- function(x) {
    if(is.null(names(x)))
      stop("The 'chains' parameter contains unnamed lists.")
  }

  lapply(chains, is.named)

  get_relation <- function(mcmc, first, second) {
    allen_relation(min(mcmc[first]),
                   max(mcmc[first]),
                   min(mcmc[second]),
                   max(mcmc[second]))
  }

  compile_result <- function(chains, mcmc) {
    result <- allen_create_result_vector()
    relations <- allen_basic_relation_strings()
    x_pos <- allen_lattice_x()
    y_pos <- allen_lattice_y()

    result.vec <- apply(X = mcmc,
                        MARGIN = 1,
                        FUN = get_relation,
                        first = chains[[1]],
                        second = chains[[2]])

    for(res in result.vec)
      result <- allen_update_result(relation = res,
                                    result_vector = result)
    result <- allen_proportion_result(result)

    title <- rep(sprintf("%s(%s)%s",
                         names(chains)[1],
                         paste0(names(result[result > 0]),
                                collapse = ""),
                         names(chains)[2]),
                 times = length(result))

    cbind.data.frame(x = x_pos,
                     y = y_pos,
                     result = result,
                     node = relations,
                     title = title)
  }

  result.list <- lapply(X = chains,
                        FUN = compile_result,
                        mcmc = mcmc)

  result <- NULL
  for (res in result.list)
    result <- rbind(result, res)

  data.frame(result)
}

#' Data for an illustrative graphic
#'
#' Create a dataframe that can be used as input for an illustrative plot.
#' Useful for describing the Allen operators: illustrate the full
#' set of Allen relations, concurrent Allen relations, and relations with
#' distinct endpoints.  Also, useful for describing the
#' chronological domains of stratification, branching, transformation, and
#' reticulation.
#'
#' The illustrative graphics include:
#' \describe{
#' \item{basic}{the 13 basic Allen relations (default);}
#' \item{concurrent}{concurrent relations;}
#' \item{distinct}{relations with distinct endpoints;}
#' \item{stratigraphic}{basic relations established by an observation of superposition;}
#' \item{branching}{basic branching relations;}
#' \item{transformation}{basic relations of transformation;}
#' \item{reticulation}{basic relations of reticulation;}
#' \item{sequence}{composite relations in a stratigraphic sequence;}
#' \item{branch}{composite relations of branching;}
#' \item{transform}{composite relations of transformation; or}
#' \item{reticulate}{composite relations of reticulation.}
#' }
#' @author Thomas S. Dye
#'
#' @param relations One of 'basic', 'concurrent', 'distinct', 'stratigraphic', 'branching', 'transformation', 'reticulation', 'sequence', 'branch', 'transform', or 'reticulate'.
#'
#' @return A dataframe for input to \code{allen_plot}
#'
#' @references
#'
#' Harris, E. \emph{Principles of Archaeological Stratigraphy}. Second edition.
#' London: Academic Press.
#'
#' Lyman, R. Lee and Michael J. O'Brien.  Seriation and cladistics: The
#' difference between anagenetic and cladogenetic evolution.  Chapter 5 in
#' \emph{Mapping Our Ancestors: Phylogenetic Approaches in Anthropology and
#' Prehistory.} New Brunswick: AldineTransaction.
#'
#' Viola, Tullio.  \emph{Peirce on the Uses of History.}  Berlin: de Gruyter.
#' See chapter 3, Historicity as Process, especially p. 83--88.
#'
illustrate_allen_relations <- function(relations = "basic") {
  result <- switch(
    relations,
    basic = allen_create_result_vector(initial_value = 1),
    concurrent = allen_create_concurrent_vector(),
    distinct = allen_create_distinct_endpoint_vector(),
    stratigraphic = allen_string_to_vector("mM"),
    branching = allen_string_to_vector("OfdoFD"),
    transformation = allen_string_to_vector("mM"),
    reticulation = allen_string_to_vector("moFDdfOM"),
    sequence = allen_composition("m", "m"),
    branch = allen_composition("Ofd", "oFD"),
    transform = allen_composition("m", "m"),
    reticulate = allen_composition("mDFo", "MdfO"),
    stop(sprintf("Unknown relation, '%s'", relations)))
  title_str <- switch(
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
    stop(sprintf("Unknown relation, '%s'", relations)))
  node <- allen_basic_relation_strings()
  x <- allen_lattice_x()
  y <- allen_lattice_y()
  title <- rep(title_str, length(node))
  cbind.data.frame(x, y, result, node, title)
}

#' Data for an analytic graphic
#'
#' Calculates the Allen composition of two relations
#'
#' @param relation_1 a string representation of an Allen relation
#' @param relation_2 a string representation of an Allen relation
#' @param title a title for the plot
#'
#' @author Thomas S. Dye
#'
#' @return A dataframe for input to \code{allen_plot}
#' 
analyze_allen_relations <- function(relation_1, relation_2, title) {
 result <- allen_composition(relation_1, relation_2)
 node <- allen_basic_relation_strings()
 x <- allen_lattice_x()
 y <- allen_lattice_y()
 title <- rep(title, length(node))
 cbind.data.frame(x, y, result, node, title)
}

#' Composition of two Allen relations 
#'
#' @param first the first Allen relation 
#' @param second the second Allen relation 
#'
#' @author Thomas S. Dye
#'
#' @return A dataframe for input to \code{allen_plot}
#' 
allen_composition <- function(first, second)
{
  if (!is.vector(first) || !is.vector(second) ||
      !is.character(first) || !is.character(second))
    stop(sprintf("One of more unrecognized parameter: %s, %s",
                 first, second))
  set_1 <- allen_string_to_set(first)
  set_2 <- allen_string_to_set(second)
  if (is.null(set_1) || is.null(set_2))
    ret <- NULL
  else {
    ret <- c()
    lookup.table <- composition_lookup_table()
    relation.pairs <- expand.grid(set_1,
                                  set_2,
                                  KEEP.OUT.ATTRS = FALSE,
                                  stringsAsFactors = FALSE)
    for (pair in seq_len(dim(relation.pairs)[1])) {
      relation <- lookup.table[relation.pairs[pair, 1],
                               relation.pairs[pair, 2]]
      ret <- allen_union(ret,
                         allen_string_to_set(relation))
    }
  }
  allen_set_to_vector(ret)
}

#' Construct an Allen composition lookup table
#'
#' @author Thomas S. Dye
#'
composition_lookup_table <- function() {
    full <- "pmoFDseSdfOMP"
    concur <- "oFDseSdfO"
    names.vector <- c("p", "m", "o", "F", "D", "s", "e", "S", "d", "f", "O", "M",
        "P")
    temp.data <- c("p", "p", "p", "p", "p", "p", "p", "p", "pmosd", "pmosd", "pmosd",
        "pmosd", full, "p", "p", "p", "p", "p", "m", "m", "m", "osd", "osd", "osd",
        "Fef", "DSOMP", "p", "p", "pmo", "pmo", "pmoFD", "o", "o", "oFD", "osd",
        "osd", concur, "DSO", "DSOMP", "p", "m", "o", "F", "D", "o", "F", "D", "osd",
        "Fef", "DSO", "DSO", "DSOMP", "pmoFD", "oFD", "oFD", "D", "D", "oFD", "D",
        "D", concur, "DSO", "DSO", "DSO", "DSOMP", "p", "p", "pmo", "pmo", "pmoFD",
        "s", "s", "seS", "d", "d", "dfO", "M", "P", "p", "m", "o", "F", "D", "s",
        "e", "S", "d", "f", "O", "M", "P", "pmoFD", "oFD", "oFD", "D", "D", "seS",
        "S", "S", "dfO", "O", "O", "M", "P", "p", "p", "pmosd", "pmosd", full, "d",
        "d", "dfOMP", "d", "d", "dfOMP", "P", "P", "p", "m", "osd", "Fef", "DSOMP",
        "d", "f", "OMP", "d", "f", "OMP", "P", "P", "pmoFD", "oFD", concur, "DSO",
        "DSOMP", "dfO", "O", "OMP", "dfO", "O", "OMP", "P", "P", "pmoFD", "seS",
        "dfO", "M", "P", "dfO", "M", "P", "dfO", "M", "P", "P", "P", full, "dfOMP",
        "dfOMP", "P", "P", "dfOMP", "P", "P", "dfOMP", "P", "P", "P", "P")
    lookup.table <- matrix(temp.data, 13, 13, byrow = TRUE)
    colnames(lookup.table) <- names.vector
    rownames(lookup.table) <- names.vector
    lookup.table
}

#' Nokel lattice y coordinates
#'
#' A vector of arbitrary coordinates for lattice node placement
#'
#' @return A vector of integers
#'
#' @author Thomas S. Dye
#'
allen_lattice_y <- function() {
  c(8, 7, 6, 5, 5, 4, 4, 4, 3, 3, 2, 1, 0)
}

#' Nokel lattice x coordinates
#'
#' A vector of arbitrary coordinates for lattice node placement
#'
#' @return A vector of integers
#'
#' @author Thomas S. Dye
#'
allen_lattice_x <- function() {
  c(0, 0, 0, -1, 1, -2, 0, 2, -1, 1, 0, 0, 0)
}

#' Allen relation of two definite intervals
#'
#' Calculates the Allen relation of two definite intervals and
#' reports the one-letter code for the interval proposed by Thomas Alspaugh.
#' Stops with an error if the end of an interval is earlier than its start.
#'
#' @param start_1 The start date of the first interval
#' @param end_1 The end date of the first interval
#' @param start_2 The start date of the second interval
#' @param end_2 The end date of the second interval
#'
#' @return A one-letter code indicating the Allen relation
#'
#' @author Thomas S. Dye
allen_relation <- function(start_1, end_1, start_2, end_2) {
    ## start_1 <- unlist(start_1)
    ## end_1 <- unlist(end_1)
    ## start_2 <- unlist(start_2)
    ## end_2 <- unlist(end_2)
    ## if (!is.numeric(c(start_1, end_1, start_2, end_2)))
    ##     stop("arguments must be numeric")
    if ((end_1 < start_1) || (end_2 < start_2))
      stop("end is older than start")
  result <- if (start_1 < start_2)
            {
              if (end_1 < start_2)
                "p"
              else
                if (end_1 == start_2)
                  "m"
              else
                if (end_1 < end_2)
                  "o"
              else
                if (end_1 == end_2)
                  "F"
              else
                "D"
            }
            else
              if (start_1 == start_2)
              {
                if (end_1 > end_2)
                  "S"
                else
                  if (end_1 == end_2)
                    "e"
                else
                  "s"
              }
            else
              if (start_1 > start_2)
              {
                if (start_1 < end_2)
                {
                  if (end_1 < end_2)
                    "d"
                  else
                    if (end_1 == end_2)
                      "f"
                  else
                    "O"
                }
                else
                  if (start_1 == end_2)
                    "M"
                else
                  "P"
              }
  result
}

#' Calculate the proportion of each relation in a result vector
#'
#' Divides through by the sum of observations in the result vector.
#' Assigns the names of the result vector to the optionally sorted
#' return vector.
#'
#' @param result_vector A result vector
#' @param sort if TRUE sort in decreasing order else return unsorted vector
#'
#' @return A named vector with proportions
#'
#' @author Thomas S. Dye
#'
allen_proportion_result <- function(result_vector, sort = FALSE) {
  if (!allen_is_result_vector(result_vector))
    stop("Unable to proportion results for an object that is not a result vector.")
  res <- result_vector/sum(result_vector)
  names(res) <- names(result_vector)
  if (sort)
    res <- sort(res, decreasing = TRUE)
  res
}

#' The basic Allen relation set
#'
#' A vector of one-letter codes for the thirteen basic Allen relations.
#' The codes were proposed by Thomas Alspaugh.
#'
#' @return A vector of thirteen one-letter codes
#'
#' @author Thomas S. Dye
#'
allen_basic_relation_set <- function() {
  c("p", "m", "o", "F", "s", "D", "e", "d", "S", "f", "O", "M", "P")
}

#' Test whether an object is a result vector
#'
#' Checks for vector, names, and class
#'
#' @param obj An object to test
#'
#' @return Boolean, TRUE if obj is a result vector, FALSE otherwise.
#'
allen_is_result_vector <- function(obj) {
  if (is.vector(obj)
      && !is.null(names(obj))
      && (all(names(obj) == allen_basic_relation_set()))
      && (class(obj) == "numeric"))
    TRUE
  else
    FALSE
}

#' Create a named result vector
#'
#' Create a named result vector initialized to zero by default or to some
#' other value.
#'
#' @param initial_value A value used to initialize the vector. typically 0
#' (default) or 1.
#'
#' @return An initialized result vector.
#'
#' @author Thomas S. Dye
#'
allen_create_result_vector <- function(initial_value = 0) {
  result_vector <- rep(initial_value, times = 13)
  names(result_vector) <- allen_basic_relation_set()
  result_vector
}

#' Update a result vector
#'
#' Increment the element of the result vector corresponding to the given
#' relation.
#'
#' @param result_vector The result vector to update
#' @param relation The relation to increment
#'
#' @return The updated result vector
#'
#' @author Thomas S. Dye
#'
allen_update_result <- function(relation, result_vector) {
  if (!allen_is_result_vector(result_vector))
    stop("Unable to update object that is not a result_vector.")
  result_vector[relation] <- result_vector[relation] + 1
  result_vector
}

#' Allen basic relation set as strings
#'
#' String descriptors of the Allen basic relations.
#'
#' @return A vector of thirteen strings
#'
#' @author Thomas S. Dye
#'
allen_basic_relation_strings <- function() {
  c(p = "precedes", m = "meets", o = "overlaps", F = "finished by", s = "starts",
    D = "contains", e = "equals", d = "during", S = "started by", f = "finishes",
    O = "overlapped by", M = "met by", P = "preceded by")
}

#' Create a result vector identifying concurrent relations
#'
#' Create a result vector where concurrent relations are set to 1
#' and non-concurrent relations are set to 0.
#'
#' @return A result vector
#'
#' @author Thomas S. Dye
#'
allen_create_concurrent_vector <- function() {
  result.vector <- allen_create_result_vector(initial_value = 0)
  concur <- allen_concurrent_relations()
  result.vector[concur] <- 1
  result.vector
}

#' Allen concurrent relation set
#'
#' A vector of nine one-letter codes for the Allen concurrent relations.
#' The codes were proposed by Thomas Alspaugh.
#'
#' @return A vector of nine one-letter codes.
#'
allen_concurrent_relations <- function() {
  c("o", "F", "s", "D", "e", "S", "d", "f", "O")
}

#' Union of two Allen relation sets.
#'
#' Returns the union of two Allen relation sets, taking care to handle
#' empty sets and the sets represented by result vectors.
#'
#' @param set_1 The first Allen relation set or result vector
#' @param set_2 The second Allen relation set or result vector
#'
#' @return An Allen relation set
#'
#' @author Thomas S. Dye
#'
allen_union <- function(set_1, set_2) {
  result.set.1 <- allen_ensure_set_vector(set_1)
  result.set.2 <- allen_ensure_set_vector(set_2)
  union(result.set.1, result.set.2)
}

#' Create a result vector for relations with distinct endpoints
#'
#' The six relations with distinct endpoints are commonly observed when
#' comparing indefinite intervals, such as those returned by a Bayesian
#' calibration
#'
#' @author Thomas S. Dye
#'
#' @return A named vector with distinct endpoint relations set to 1
#' and all others set to 0.
#'
allen_create_distinct_endpoint_vector <- function() {
  result.vector <- allen_create_result_vector(initial_value = 0)
  six <- allen_six_value_set()
  result.vector[six] <- 1
  result.vector
}

#' Allen relation set for intervals with distinct endpoints.
#'
#' Return the six value Allen relation set for intervals with distinct
#' endpoints.
#'
#' @return An Allen relation set
#'
#' @author Thomas S. Dye
#'
allen_six_value_set <- function() {
  c("p", "o", "D", "d", "O", "P")
}

#' Ensure an Allen set is represented as a vector of single character strings
#'
#' Expects a string, set vector, or result vector and will stop with an
#' error if something else is encountered.
#'
#' @param obj An Allen set represented as a string, a set vector, or a
#' result vector.
#'
#' @return An Allen set represented as a set vector.
#'
#' @author Thomas S. Dye
#'
allen_ensure_set_vector <- function(obj) {
  if (allen_is_set_vector(obj))
    result.set <- obj
  else
    if (allen_is_set_string(obj))
      result.set <- allen_string_to_set(obj)
  else
    if (allen_is_result_vector(obj))
      result.set <- names(obj[obj > 0])
  else
    stop("Unrecognized Allen set.")
  result.set
}

#' Test if an object is a set vector
#'
#' Checks for mode 'character', length less than 13.
#' Note: this predicate is a (very) partial implementation.
#'
#' @param obj An object to test
#'
#' @return Boolean, TRUE if obj is a set vector, FALSE otherwise.
#'
#' @author Thomas S. Dye
#'
allen_is_set_vector <- function(obj) {
  if (identical(obj, c()))
    TRUE else if ((length(obj) <= 13 && mode(obj) == "character"))
           TRUE else FALSE
}

#' Test if an object is a set string
#'
#' Checks for mode 'character', length of 1, and nchar <= 13
#'
#' @param obj An object to test
#'
#' @return Boolean, TRUE if obj is a set string, FALSE otherwise.
#'
#' @author Thomas S. Dye
#'
allen_is_set_string <- function(obj) {
  if ((mode(obj) == "character")
      && (length(obj) == 1)
      && (nchar(obj) <= 13))
    TRUE
  else
    FALSE
}

#' Convert a string containing Allen relation codes to a relation set
#'
#' Characters in the string that are not Allen relation codes are
#' not identified and are added to the set.
#'
#' @param s A string with Allen relation codes.
#'
#' @return A vector of single letter Allen relation codes.
#'
#' @author Thomas S. Dye
#'
allen_string_to_set <- function(s) {
  res <- strsplit(s, "")
  unlist(res)
}

#' Convert a string containing Allen relation codes to a result vector
#'
#' A result vector is named with Allen relation codes and contains counts of
#' observed relations.
#'
#' @param s A string with Allen relation codes
#'
#' @return A named result vector
#'
#' @author Thomas S. Dye
#'
allen_string_to_vector <- function(s) {
  s.set <- allen_string_to_set(s)
  allen_set_to_vector(s.set)
}

#' Convert an Allen relation set to a named vector
#'
#' Set elements that are not Allen relation codes are silently ignored.
#'
#' @param s An Allen relation set, a vector of single letter codes.
#'
#' @return A named result vector.
#'
#' @author Thomas S. Dye
#'
allen_set_to_vector <- function(s) {
  res <- allen_create_result_vector()
  for (x in 1:length(s)) {
    res <- allen_update_result(s[x], res)
  }
  res
}
