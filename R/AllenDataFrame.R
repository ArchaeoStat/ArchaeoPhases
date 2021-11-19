#' Relate two or more phases
#'
#' Reads MCMC output to create a dataframe suitable
#' for plotting the Allen relations between two or more phases.
#'
#' @author Thomas S. Dye
#'
#' @param mcmc path to a csv file with MCMC output
#' @param phases_1 list of three-element vectors, the first element of which is
#' the phase name and the second and third elements are the column indices
#' for the beginning and end of the phase, respectively
#' @param phases_2 list of three-element vectors, the first element of which is
#' the phase name and the second and third elements are the column indices
#' for the beginning and end of the phase, respectively
#' @param app one of 'bcal', 'oxcal', or 'chronomodel' to specify which
#' Bayesian calibration application produced the MCMC output
#' @param quiet One of 'no' to allow messages and warnings,
#' 'partial' (default) to suppress messages and allow warnings, or 'yes'
#' to suppress messages and warnings.
#'
#' @return A dataframe suitable for plotting with allen_plot().
#'
#' @seealso \code{\link{allen_composite_relation}}
#'
allen_relate_phases <- function(mcmc,
                                phases_1,
                                phases_2,
                                app = "bcal",
                                quiet = "partial") {

    relate_phases <- function(first, second, all.chains) {
        chains <- all.chains[, c(first[2:3], second[2:3])]
        zero.vector <- allen.create.result.vector()
        result.vector <- allen.calculate.relations.2(zero.vector, chains)
        result <- allen_proportion_results(result.vector, sort = FALSE)
        node <- allen_basic_relation_strings()
        allen_codes <- names(node)
        allen_set <- allen_codes[result.vector != 0]
        allen_set <- paste(allen_set, collapse = "")
        x <- allen_lattice_x()
        y <- allen_lattice_y()
        title <- rep(sprintf("%s(%s)%s", first[1], allen_set, second[1]),
            length(result.vector))
        this.relation <- cbind.data.frame(x, y, result, node, title)
        this.relation
    }

    chains.df <- switch(
        app,
        chronomodel = read_chronomodel(mcmc, quiet = quiet),
        oxcal = read_oxcal(mcmc, quiet = quiet),
        bcal = read_bcal(mcmc, quiet = quiet),
        stop(sprintf("Unknown application, '%s'", app)))

    relation_list <- mapply(FUN = relate_phases,
                            phases_1,
                            phases_2,
                            MoreArgs = list(all.chains = chains.df),
                            SIMPLIFY = FALSE)
    relations <- NULL
    for (x in relation_list) {
        relations <- rbind.data.frame(relations, x)
    }
    new_allen_empirical(relations)
}

#' Data for an illustrative graphic
#'
#' Create a dataframe that can be used as input for an illustrative plot.
#' Useful for describing the Allen operators: illustrate the full
#' set of Allen relations, concurrent Allen relations, and relations with
#' distinct endpoints (six-value set).  Also, useful for describing the
#' chronological domains of stratification, anagenesis, cladogenesis,
#' reticulation, and incorporation.
#'
#' @author Thomas S. Dye
#'
#' @param relations One of:
#' \describe{
#' \item{basic}{show the 13 basic Allen relations (default);}
#' \item{concurrent}{show concurrent relations;}
#' \item{six}{show relations with distinct endpoints;}
#' \item{inherit}{show chronological information about a transmission
#'     relationship from the receiver's point of view;}
#' \item{contribute}{show chronological information from the giver's point of view during
#'     transmission;}
#' \item{stratigraphic}{show the basic stratigraphic relations established by an
#'     observation of superposition;}
#' \item{anagenetic}{show the basic relations of artifact
#'     change without branching;}
#' \item{cladogenetic}{show the basic relations of artifact change with branching;}
#' \item{contributors}{show the composite relations of two contributors to a reticulation
#'     occurrence;}
#' \item{innovations}{show the composite relations of two entities produced by
#'     innovation from a single source;}
#' \item{sequence}{show the composite relations of superposition
#'     in a stratigraphic sequence; or}
#' \item{anagenesis}{show the composite relations of transmission during
#'     anagenesis.}}
#'
#' @return A dataframe for input to allen_plot()
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

illustrate_allen_relations <- function(relations = "basic") {
    result <- switch(
        relations,
        basic = allen.create.result.vector(initial.value = 1),
        concurrent = allen.create.concurrent.vector(),
        six = allen.create.distinct.endpoint.vector(),
        inherit = allen.string.to.vector("oFD"),
        contribute = allen.string.to.vector("moFD"),
        stratigraphic = allen.string.to.vector("mM"),
        anagenetic = allen.string.to.vector("mM"),
        cladogenetic = allen.string.to.vector("OfdoFD"),
        contributors = allen.set.to.vector(
            allen.composition(allen.string.to.set("moFD"),
                              allen.string.to.set("MOfd"))),
        innovations = allen.set.to.vector(
            allen.composition(allen.string.to.set("Ofd"),
                              allen.string.to.set("oFD"))),
        sequence = allen.set.to.vector(
            allen.composition(allen.string.to.set("m"),
                              allen.string.to.set("m"))),
        anagenesis = allen.set.to.vector(
            allen.composition(allen.string.to.set("m"),
                              allen.string.to.set("m"))),
        incorporation.2 = allen.set.to.vector(
            allen.composition(allen.string.to.set("m"),
                              allen.string.to.set("Ofd"))),
        incorporation.1 = allen.set.to.vector(
            allen.composition(allen.string.to.set("m"),
                              allen.string.to.set("M"))),
        stop(sprintf("Unknown relation, '%s'", relations)))

    title_str <- switch(
        relations,
        basic = "Basic Allen relations",
        concurrent = "Basic concurrent relations",
        six = "Basic Allen relations with distinct endpoints",
        inherit = "Basic inheritance relations",
        contribute = "Basic contribution relations",
        stratigraphic = "Basic stratigraphic relations",
        anagenetic = "Basic anagenetic relations",
        cladogenetic = "Basic cladogenetic relations",
        contributors = "Composite contributor relation",
        innovations = "Composite innovation relation",
        sequence = "Composite stratigraphic relation",
        anagenesis = "Composite anagenetic relation",
        incorporation.2 = "Composite contributor relation with two successors",
        incorporation.1 = "Composite contributor relation with one successor",
        stop(sprintf("Unknown relation, '%s'", relations)))

    node <- allen_basic_relation_strings()
    x <- allen_lattice_x()
    y <- allen_lattice_y()
    title <- rep(title_str, length(node))
    df <- cbind.data.frame(x, y, result, node, title)
    new_allen_illustrative(df)
}

#' Calculate the composite relation of two phases
#'
#' Uses the Allen algebra to infer the relation of two phases from their
#' relation to a third phase.
#'
#' @param mcmc path to a csv file with MCMC output
#' @param phases a vector with six column indices representing the start
#' and end chains of three phases.  The composite relation of the first
#' and third phases will be inferred based on their relation to the
#' second phase.
#' @param title a plot title
#' @param app one of 'bcal', 'oxcal', or 'chronomodel' to specify which
#' Bayesian calibration application produced the MCMC output
#' @param quiet One of 'no' to allow messages and warnings,
#' 'partial' (default) to suppress messages and allow warnings, or 'yes'
#' to suppress messages and warnings.
#'
#' @return A dataframe for input to allen_plot().
#'
#' @seealso \code{\link{allen_relate_phases}}
#'
allen_composite_relation <- function(mcmc,
                                     phases,
                                     title = c("first", "second"),
                                     app = "bcal",
                                     quiet = "partial") {
    if (length(phases) != 6)
        stop("Chains for three phases are required.")
    chains <- switch(app,
                     chronomodel = read_chronomodel(mcmc, quiet = quiet),
                     oxcal = read_oxcal(mcmc, quiet = quiet),
                     bcal = read_bcal(mcmc, quiet = quiet),
                     stop(sprintf("Unknown application, '%s'", app)))
    chains <- chains[, phases]
    names <- colnames(chains)
    zero.vector <- allen.create.result.vector()
    result.vector.1 <- allen.calculate.relations.2(zero.vector, chains[, 1:4])
    result.vector.2 <- allen.calculate.relations.2(zero.vector, chains[, 3:6])
    set.vector.1 <- allen.set.vector.from.result(result.vector.1)
    set.vector.2 <- allen.set.vector.from.result(result.vector.2)
    res <- allen.composition(set.vector.1, set.vector.2)
    title_res <- paste(res, collapse = "")
    title_res <- allen.order.string(title_res)
    graph_title <- sprintf("%s(%s)%s", title[1], title_res, title[2])
    result <- allen.set.to.vector(res)
    node <- allen_basic_relation_strings()
    x <- allen_lattice_x()
    y <- allen_lattice_y()
    title <- rep(graph_title, length(node))
    df <- cbind.data.frame(x, y, result, node, title)
    new_allen_empirical(df)
}


#' Constructor for empirical Allen set object
#'
#' Object to be returned by functions that create empirical Allen sets.
#'
#' @param x A data frame with the Allen set.
#'
#' @return An \code{allen_set_empirical} object that inherits from \code{tbl_df}.
#'
#' @author Thomas S. Dye, \email{tsd@@tsdye.online}
#'
new_allen_empirical <- function(x = data.frame()) {

    stopifnot(is.data.frame(x))
    structure(x,
              class = c("allen_set_empirical", "data.frame"))
}

#' Constructor for illustrative Allen set object
#'
#' Object to be returned by functions that create illustrative Allen sets.
#'
#' @param x A data frame with the Allen set.
#'
#' @return An \code{allen_set_illustrative} object that inherits from \code{tbl_df}.
#'
#' @author Thomas S. Dye, \email{tsd@@tsdye.online}
#'
new_allen_illustrative <- function(x = data.frame()) {

    stopifnot(is.data.frame(x))
    structure(x,
              class = c("allen_set_illustrative", "data.frame"))
}
