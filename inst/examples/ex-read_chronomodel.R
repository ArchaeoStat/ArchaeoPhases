\dontrun{
## Import ChronoModel Output
path <- "chronomodel/output/"

## Events
path_events <- system.file(path, "Chain_all_Events.csv", package = "fasti")
(chrono_events <- read_chronomodel(path_events, phases = FALSE))

## Phases
path_phases <- system.file(path, "Chain_all_Phases.csv", package = "fasti")
(chrono_phases <- read_chronomodel(path_phases, phases = TRUE))
}
