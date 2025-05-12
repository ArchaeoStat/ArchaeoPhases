Sys.setenv(LANGUAGE = "en") # Force locale

# Activity plot ================================================================
eve <- as_events(mcmc_events, calendar = CE(), iteration = 1)
eve <- eve[1:1000, ]

act <- activity(eve)
expect_equal_to_reference(act, file = "_snaps/activity.rds")
expect_error(current = activity(tempo(eve, count = TRUE)),
             "Tempo must be computed as probabilities.")

if (at_home()) {
  using("tinysnapshot")
  options(tinysnapshot_device = "svglite")
  options(tinysnapshot_height = 7) # inches
  options(tinysnapshot_width = 7)
  options(tinysnapshot_tol = 300) # pixels
  options(tinysnapshot_os = "Linux")

  plot_activity <- function() plot(act)
  expect_snapshot_plot(plot_activity, "plot_activity")
}
