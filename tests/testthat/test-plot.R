test_that("Events", {
  skip_if_not_installed("vdiffr")

  eve <- as_events(events, calendar = "CE", iteration = 1)

  plot_event_CE_desc <- function() plot(eve, interval = "credible", decreasing = TRUE)
  vdiffr::expect_doppelganger("plot_event_CE_desc", plot_event_CE_desc)

  plot_event_CE_incr <- function() plot(eve, interval = "credible", decreasing = FALSE)
  vdiffr::expect_doppelganger("plot_event_CE_incr", plot_event_CE_incr)

  eve <- CE_to_BP(eve)
  plot_event_hpdi_BP <- function() plot(eve, interval = "hpdi", level = 0.68)
  vdiffr::expect_doppelganger("plot_event_hpdi_BP", plot_event_hpdi_BP)
})
test_that("Phases", {
  skip_if_not_installed("vdiffr")

  pha <- as_phases(phases, start = c(1, 3), calendar = "CE", iteration = 1)

  plot_phase_CE_desc <- function() plot(pha, range = NULL, decreasing = TRUE)
  vdiffr::expect_doppelganger("plot_phase_CE_desc", plot_phase_CE_desc)

  plot_phase_CE_inc <- function() plot(pha, range = NULL, decreasing = FALSE)
  vdiffr::expect_doppelganger("plot_phase_CE_inc", plot_phase_CE_inc)

  pha <- CE_to_BP(pha)
  plot_phase_BP <- function() plot(pha, level = 0.68)
  vdiffr::expect_doppelganger("plot_phase_BP", plot_phase_BP)
})
test_that("Succession", {
  skip_if_not_installed("vdiffr")

  pha <- as_phases(phases, start = c(1, 3), calendar = "CE", iteration = 1)

  plot_phase_hiatus_CE <- function() plot(pha, range = "hiatus", decreasing = FALSE)
  vdiffr::expect_doppelganger("phase_hiatus_CE", plot_phase_hiatus_CE)

  pha <- CE_to_BP(pha)
  plot_phase_transition_BP <- function() plot(pha, range = "transition")
  vdiffr::expect_doppelganger("phase_transition_BP", plot_phase_transition_BP)
})
