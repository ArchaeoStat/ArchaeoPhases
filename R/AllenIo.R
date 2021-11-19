#' Read MCMC chains from Bayesian calibration software output
#'
#' Reads chains for two intervals from one or two csv files of MCMC
#' output from a Bayesian calibration application.
#' The function checks for missing or incorrect parameters and whether
#' or not files exist. It exits with an error message if there are problems.
#'
#' @param mcmc.file path to the csv file of MCMC output
#' @param positions a vector of 2 or 4 column indices of \code{mcmc.file}
#' @param mcmc.file.2 optional path to another csv file of MCMC output
#' @param positions.2 a vector of 2 column indices of \code{mcmc.file.2}
#' @param names a vector of two strings naming the intervals specified by
#' the \code{positions} parameters(s)
#' @param app one of 'bcal', 'oxcal', or 'chronomodel'
#' @param decimal optional decimal character for Chronomodel output
#' @param separator optional separator character for Chronomodel output
#' @param quiet One of 'no' to allow messages and warnings,
#' 'partial' (default) to suppress messages and allow warnings, or 'yes'
#' to suppress messages and warnings.
#'
#' @return a list with two components: \code{result} and \code{names}, where \code{result}
#' contains the MCMC chains and \code{names} contains the names parameter.
#'
#' @author Thomas S. Dye
#'
#' @export
allen.read.mcmc.chains <- function(mcmc.file,
                                   positions,
                                   mcmc.file.2 = NULL,
                                   positions.2 = NULL,
                                   names = c("Interval 1", "Interval 2"),
                                   app = "bcal",
                                   decimal = ".",
                                   separator = ",",
                                   quiet = "partial") {
    if (missing(mcmc.file) || missing(positions))
        stop("please set required parameters")
    if (is.null(mcmc.file.2) && !is.null(positions.2))
        stop("missing input file")
    if (is.null(mcmc.file.2) && length(positions) != 4)
        stop("four interval boundaries are required")
    if (!is.null(mcmc.file.2) && is.null(positions.2))
        stop("interval boundaries are required")
    mcmc.1 <- switch(EXPR = app,
                     bcal = read_bcal(mcmc.file, quiet = quiet),
                     oxcal = read_oxcal(mcmc.file, quiet = quiet),
                     chronomodel = read_chronomodel(mcmc.file, decimal, separator, quiet = quiet),
                     stop("unknown calibration application"))
    start.1 <- unlist(mcmc.1[, positions[[1]]])
    end.1 <- unlist(mcmc.1[, positions[[2]]])
    if (is.null(mcmc.file.2)) {
        start.2 <- unlist(mcmc.1[, positions[[3]]])
        end.2 <- unlist(mcmc.1[, positions[[4]]])
    } else {
      mcmc.2 <- switch(EXPR = app,
                       bcal = read_bcal(mcmc.file.2, quiet = quiet),
                       oxcal = read_oxcal(mcmc.file.2, quiet = quiet),
                       chronomodel = read_chronomodel(mcmc.file.2, decimal, separator, quiet = quiet),
                       stop("unknown calibration application"))
        start.2 <- unlist(mcmc.2[, positions.2[[1]]])
        end.2 <- unlist(mcmc.2[, positions.2[[2]]])
    }
    result <- list(start.1, end.1, start.2, end.2)
    names(result) <- c("start.1", "end.1", "start.2", "end.2")
    list(result = result, names = names)
}

#' Express the Allen relation set in mathematical notation
#'
#' Return a string expressing an Allen relation set in mathematical
#' set notation.  Useful for printing to the console.
#'
#' @param allen.set an Allen relation set
#'
#' @author Thomas S. Dye
#'
allen.relations.set <- function(allen.set) {
    result.set <- ensure.allen.set.vector(allen.set)
    temp <- c()
    for (x in result.set) temp <- paste(temp, x, sep = "")
    ret <- paste("(", temp, ")", sep = "")
    ret
}

#' Calculate the concurrence set of an Allen relation sets
#'
#' Return a string that describes the concurrence set of an Allen relation set,
#' or indicates there is no concurrence set.  Useful for output
#' to the console.
#'
#' @param allen.set Allen set
#'
#' @return A string that describes the concurrence relation
#'
#' @author Thomas S. Dye
#'
allen.relations.concur <- function(allen.set) {
    concurrence.set = allen.concurrent.relation.set()
    result.set <- ensure.allen.set.vector(allen.set)
    if (setequal(union(result.set, concurrence.set), concurrence.set))
        ret <- paste("Concurrent:", allen.relations.set(result.set), "is contained in",
            allen.relations.set(concurrence.set)) else ret <- paste("Not concurrent:", allen.relations.set(result.set), "is not contained in",
        allen.relations.set(concurrence.set))
    ret
}

#' Relative strength of Allen relation sets
#'
#' Construct a string that describes the relative strength of two Allen relation
#' sets.  Useful for output to the console.
#'
#' @param allen.set.1 an Allen set
#' @param allen.set.2 an Allen set
#'
#' @return A string that describes the relative strength of two Allen relation sets.
#' 
#' @author Thomas S. Dye
#'
allen.relations.relationships <- function(allen.set.1, allen.set.2) {
    result.set.union <- allen.relations.union(allen.set.1, allen.set.2)
    allen.set.1 <- ensure.allen.set.vector(allen.set.1)
    allen.set.2 <- ensure.allen.set.vector(allen.set.2)
    max.len <- (max(length(allen.set.1), length(allen.set.2)))
    if (length(result.set.union) > max.len)
        result <- "is incomparable to" else if (length(allen.set.1) == length(allen.set.2))
        result <- "is equal to" else if (length(allen.set.1) < length(allen.set.2))
        result <- "is stronger than" else result <- "is weaker than"
    ret <- paste(allen.relations.set(allen.set.1), result, allen.relations.set(allen.set.2))
    ret
}

#' Summarize the relation of two phases
#'
#' @param mcmc path to a csv file with MCMC output
#' @param phases a vector with four column indices representing the start
#' and end chains of two phases
#' @param app one of 'bcal', 'oxcal', or 'chronomodel' to specify which
#' Bayesian calibration application produced the MCMC output
#' @param quiet One of 'no' to allow messages and warnings,
#' 'partial' (default) to suppress messages and allow warnings, or 'yes'
#' to suppress messages and warnings.
#'
#' @return a list with the following components:
#' \describe{
#' \item{result}{The Allen relation as a string.}
#' \item{relation_set}{The Allen relation as a set.}
#' \item{concurrence}{A string describing the concurrency relation.}
#' \item{full_result}{The Allen relation as a vector.}
#' \item{six_value_result}{The Allen relation for relations with distinct endpoints.}
#' \item{full_proportion}{The proportion of each relation in an empirical Allen relation.}
#' \item{six_value_proportion}{The proportion of relations with distinct endpoints in an empirical Allen relation.}
#' \item{concurrence_proportion}{The proportion of concurrent relations in an empirical Allen relation.}
#' \item{mcmc_file}{File name passed to the \code{mcmc} parameter.}
#' \item{application}{String passed to the \code{app} parameter.}
#' }
#' @author Thomas S. Dye
#'
#' @export
#'
allen_relation_summary <- function(mcmc, phases, app = "bcal", quiet = "partial") {
  chains <- switch(app,
                   chronomodel = read_chronomodel(mcmc, quiet = quiet),
                   oxcal = read_oxcal(mcmc, quiet = quiet),
                   bcal = read_bcal(mcmc, quiet = quiet))
    chains <- chains[, phases]
    names <- allen.check.names(colnames(chains))
    zero.vector <- allen.create.result.vector()
    result.full <- allen.calculate.relations.2(zero.vector, chains)
    result.six <- allen.coerce.six(result.full)
    result.six.proportion <- allen_proportion_results(result.six)
    result.full.proportion <- allen_proportion_results(result.full)
    result.non.zero <- result.full.proportion[result.full.proportion != 0]
    concur.set <- allen.concurrent.relation.set()
    relation.set <- allen.relations.set(result.full)
    non.zero.concurs <- allen.relations.intersection(names(result.non.zero), concur.set)
    concurrence_string <- allen.relations.concur(result.full)
    proportion.concurs <- sum(result.full.proportion[non.zero.concurs])
    max_code <- names(result.six[result.six == max(result.six)])
    relation_string <- allen_code_to_string(max_code)
    result_string <- sprintf("%s %s %s", names$first, relation_string, names$second)
    list(result = result_string,
         relation_set = relation.set,
         concurrence = concurrence_string,
         full_result = result.full,
         six_value_result = result.six,
         full_proportion = round(result.full.proportion, digits = 3),
         six_value_proportion = round(result.six.proportion, digits = 3),
         concurrence_proportion = proportion.concurs,
         mcmc_file = mcmc,
         application = app)
}
