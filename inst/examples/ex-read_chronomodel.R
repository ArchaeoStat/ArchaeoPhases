if (requireNamespace("ArchaeoData", quietly = TRUE)) {
  ## Construct the paths to the data
  path <- file.path("chronomodel", "ksarakil")
  path_events <- system.file(path, "Chain_all_Events.csv", package = "ArchaeoData")
  path_phases <- system.file(path, "Chain_all_Phases.csv", package = "ArchaeoData")

  ## Import ChronoModel events
  (chrono_events <- read_chronomodel_events(path_events))

  ## Import ChronoModel phases
  (chrono_phases <- read_chronomodel_phases(path_phases))
}
