Sys.setenv(LANGUAGE = "en") # Force locale

# Occurrence plot ==============================================================
eve <- as_events(mcmc_events, calendar = CE(), iteration = 1)
eve <- eve[1:1000, ]

occ <- occurrence(eve)
expect_equal_to_reference(occ, file = "_snaps/occurrence.rds")

if (at_home()) {
  using("tinysnapshot")
  source("helpers.R")

  plot_occurrence <- function() plot(occ)
  expect_snapshot_plot(plot_occurrence, "plot_occurrence")
}
