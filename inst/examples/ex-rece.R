\dontrun{
## Import ChronoModel Output
path <- "chronomodel/ksarakil"

## Events
path_events <- system.file(path, "Chain_all_Events.csv", package = "fasti")
chrono_events <- read_chronomodel_events(path_events)

## RECE
tmp <- rece(chrono_events, resolution = 10, n = 500)
plot(tmp)
}
