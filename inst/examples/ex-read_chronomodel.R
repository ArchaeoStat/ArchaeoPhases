if (requireNamespace("ArchaeoData", quietly = TRUE)) {
  ## Import ChronoModel Output
  path <- "chronomodel/ksarakil"

  ## Events
  path_events <- system.file(path, "Chain_all_Events.csv", package = "ArchaeoData")
  (chrono_events <- read_chronomodel_events(path_events))

  ## Phases
  path_phases <- system.file(path, "Chain_all_Phases.csv", package = "ArchaeoData")
  (chrono_phases <- read_chronomodel_phases(path_phases))
}
