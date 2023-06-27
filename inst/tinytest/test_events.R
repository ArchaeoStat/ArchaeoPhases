eve <- as_events(mcmc_events, calendar = CE(), iteration = 1)

names(eve) <- c("A", "B", "C", "D")
expect_identical(names(eve), colnames(eve))

if (at_home()) {
  using("tinysnapshot")
  options(tinysnapshot_device = "svglite")
  options(tinysnapshot_height = 7) # inches
  options(tinysnapshot_width = 7)
  options(tinysnapshot_tol = 1000) # pixels
  options(tinysnapshot_os = "Linux")

  ## Events --------------------------------------------------------------------
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
}
