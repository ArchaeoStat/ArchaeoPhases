#' Composition of two Allen relation sets
#'
#' @param allen.set.1 the first Allen relation set
#' @param allen.set.2 the second Allen relation set
#'
#' @author Thomas S. Dye
#'
allen.composition <- function(allen.set.1, allen.set.2) {
    if (!is.set.vector(allen.set.1) || !is.set.vector(allen.set.2))
        stop("Unrecognized parameter passed to allen.composition.")
    if (is.null(allen.set.1) || is.null(allen.set.2))
        ret <- NULL else {
        lookup.table <- composition.lookup.table()
        relation.pairs <- expand.grid(allen.set.1, allen.set.2, KEEP.OUT.ATTRS = FALSE,
            stringsAsFactors = FALSE)
        ret <- c()
        for (pair in seq_len(dim(relation.pairs)[1])) {
            relation <- lookup.table[relation.pairs[pair, 1], relation.pairs[pair,
                2]]
            ret <- allen.relations.union(ret, allen.string.to.set(relation))
        }
    }
    ret
}

#' Construct an Allen composition lookup table
#'
#' @author Thomas S. Dye
#'
composition.lookup.table <- function() {
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
