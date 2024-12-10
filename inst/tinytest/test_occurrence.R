Sys.setlocale("LC_MESSAGES", 'en_GB.UTF-8') # Force locale
options(ArchaeoPhases.calendar = calendar("CE"))

# Occurrence plot ==============================================================
eve <- as_events(mcmc_events, calendar = CE(), iteration = 1)
eve <- eve[1:1000, ]

occ <- occurrence(eve)
expect_equal_to_reference(occ, file = "_snaps/occurrence.rds")

if (at_home()) {
  using("tinysnapshot")
  options(tinysnapshot_device = "svglite")
  options(tinysnapshot_height = 7) # inches
  options(tinysnapshot_width = 7)
  options(tinysnapshot_tol = 300) # pixels
  options(tinysnapshot_os = "Linux")

  plot_occurrence <- function() plot(occ)
  expect_snapshot_plot(plot_occurrence, "plot_occurrence")
}
