\dontrun{
## Import ChronoModel Output
path <- "chronomodel/ksarakil"

## Events
path_events <- system.file(path, "Chain_all_Events.csv", package = "fasti")
(chrono_events <- read_chronomodel_events(path_events))

## Phases
path_phases <- system.file(path, "Chain_all_Phases.csv", package = "fasti")
(chrono_phases <- read_chronomodel_phases(path_phases))

path_model <- system.file("chronomodel/ksarakil.chr", package = "fasti")
chrono_model <- jsonlite::fromJSON(path_model, simplifyVector = FALSE)

k <- do.call(rbind.data.frame, chrono_model$events_constraints)
iris_tree <- tidygraph::as_tbl_graph(k, directed = TRUE)

ggraph::ggraph(iris_tree, layout = ggraph::layout_tbl_graph_igraph("sygiyama")) +
  ggraph::geom_edge_link() +
  ggraph::geom_node_point(size = 8, colour = 'steelblue') +
  ggraph::geom_node_text(ggplot2::aes(label = name), colour = 'white', vjust = 0.4) +
  ggraph::theme_graph()
}
