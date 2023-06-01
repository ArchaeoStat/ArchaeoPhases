if (at_home()) {
  using("tinysnapshot")
  options(tinysnapshot_device = "svglite")
  options(tinysnapshot_height = 7) # inches
  options(tinysnapshot_width = 7)
  options(tinysnapshot_tol = 200) # pixels
  options(tinysnapshot_os = "Linux")

  eve <- as_events(mcmc_events, calendar = CE(), iteration = 1)
  eve <- eve[1:5000, ]

  # Tempo plot =================================================================
  tmp <- tempo(eve)

  plot_tempo_cred <- function() plot(tmp, interval = "credible")
  expect_snapshot_plot(plot_tempo_cred, "plot_tempo_cred")

  plot_tempo_gauss <- function() plot(tmp, interval = "gauss")
  expect_snapshot_plot(plot_tempo_gauss, "plot_tempo_gauss")

  # Activity plot ==============================================================
  tmp <- activity(eve)
  plot_activity <- function() plot(tmp)
  expect_snapshot_plot(plot_activity, "plot_activity")

  # Occurrence plot ============================================================
  tmp <- occurrence(eve)
  plot_occurrence <- function() plot(tmp)
  expect_snapshot_plot(plot_occurrence, "plot_occ_CE")
}
