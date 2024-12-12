if (requireNamespace("ArchaeoData", quietly = TRUE)) {
  ## ChronoModel ===============================================================
  chrono_path <- file.path("chronomodel", "ksarakil")

  output_events <- system.file(chrono_path, "Chain_all_Events.csv", package = "ArchaeoData")
  chrono_events <- read_chronomodel_events(output_events)
  expect_inherits(chrono_events, "EventsMCMC")

  output_phases <- system.file(chrono_path, "Chain_all_Phases.csv", package = "ArchaeoData")
  chrono_phases <- read_chronomodel_phases(output_phases)
  expect_inherits(chrono_phases, "PhasesMCMC")

  ## OxCal =====================================================================
  path_oxcal <- file.path("oxcal", "ksarakil", "MCMC_Sample.csv")
  output_oxcal <- system.file(path_oxcal, package = "ArchaeoData")
  oxcal_mcmc <- read_oxcal(output_oxcal)
  expect_inherits(oxcal_mcmc, "EventsMCMC")

  ## BCal ======================================================================
  path_bcal <- file.path("bcal", "fishpond.csv")
  output_bcal <- system.file(path_bcal, package = "ArchaeoData")
  bcal_mcmc <- read_bcal(output_bcal)
  expect_inherits(bcal_mcmc, "EventsMCMC")
}
