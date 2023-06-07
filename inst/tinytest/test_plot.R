if (at_home()) {
  using("tinysnapshot")
  options(tinysnapshot_device = "svglite")
  options(tinysnapshot_height = 7) # inches
  options(tinysnapshot_width = 7)
  options(tinysnapshot_tol = 1000) # pixels
  options(tinysnapshot_os = "Linux")

  # Events =====================================================================
  eve <- as_events(mcmc_events, calendar = CE(), iteration = 1)

  plot_event <- function() plot(eve, interval = NULL, sort = FALSE)
  expect_snapshot_plot(plot_event, "plot_event")

  plot_event_incr <- function() plot(eve, interval = NULL, decreasing = TRUE)
  expect_snapshot_plot(plot_event_incr, "plot_event_incr")

  plot_event_decr <- function() plot(eve, interval = NULL, decreasing = FALSE)
  expect_snapshot_plot(plot_event_decr, "plot_event_decr")

  plot_event_cred <- function() plot(eve, interval = "credible", level = 0.68)
  expect_snapshot_plot(plot_event_cred, "plot_event_cred")

  plot_event_hdr <- function() plot(eve, interval = "hdr", level = 0.68)
  expect_snapshot_plot(plot_event_hdr, "plot_event_hdr")

  plot_event_inter <- function() plot(eve, density = FALSE)
  expect_snapshot_plot(plot_event_inter, "plot_event_inter")

  # Phases =====================================================================
  pha <- as_phases(mcmc_phases, start = c(1, 3), calendar = CE(), iteration = 1)

  plot_phase_decr <- function() plot(pha, decreasing = TRUE)
  expect_snapshot_plot(plot_phase_decr, "plot_phase_decr")

  plot_phase_inc <- function() plot(pha, decreasing = FALSE)
  expect_snapshot_plot(plot_phase_inc, "plot_phase_inc")

  # Succession =================================================================
  plot_phase_hiatus <- function() plot(pha, succession = "hiatus")
  expect_snapshot_plot(plot_phase_hiatus, "plot_phase_hiatus")

  plot_phase_transition <- function() plot(pha, succession = "transition")
  expect_snapshot_plot(plot_phase_transition, "plot_phase_transition")

  plot_phase_succession <- function() plot(pha, succession = c("transition", "hiatus"))
  expect_snapshot_plot(plot_phase_succession, "plot_phase_succession")
}
