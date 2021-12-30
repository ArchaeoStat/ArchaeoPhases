#' Analyze composite relations
#'
#' Visualize composite Allen relations with a Nokel lattice.
#'
#' @param relation_1 A string denoting an Allen set.
#' @param relation_2 A string denoting an Allen set.
#' @param title A string displayed as the title of the Nokel lattice.
#' @param ... Named arguments to be passed on to \code{allen_plot()}.
#'
#' @author Thomas S. Dye
#'
#' @return A layout_tbl_graph object.
#'
#' @examples
#'
#' # Plot to the R graphics device
#' # allen_analyze("mDFo", "MdfO", "Composite reticulation relation")
#'
#' @export
allen_analyze <- function(relation_1, relation_2, title, ...) {
  allen_plot(analyze_allen_relations(relation_1, relation_2, title), ...)
}
