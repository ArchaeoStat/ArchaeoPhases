Sys.setenv(LANGUAGE = "en") # Force locale

# Tempo plot ===================================================================
eve <- as_events(mcmc_events, calendar = CE(), iteration = 1)
eve <- eve[1:1000, ]

tmp <- tempo(eve)
expect_equal_to_reference(tmp, file = "_snaps/tempo.rds")

if (at_home()) {
  using("tinysnapshot")
  options(tinysnapshot_device = "svglite")
  options(tinysnapshot_height = 7) # inches
  options(tinysnapshot_width = 7)
  options(tinysnapshot_tol = 300) # pixels
  options(tinysnapshot_os = "Linux")

  plot_tempo_cred <- function() plot(tmp, interval = "credible")
  expect_snapshot_plot(plot_tempo_cred, "plot_tempo_cred")

  plot_tempo_gauss <- function() plot(tmp, interval = "gauss")
  expect_snapshot_plot(plot_tempo_gauss, "plot_tempo_gauss")
}
