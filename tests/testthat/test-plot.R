test_that("Events", {
  skip_if_not_installed("vdiffr")

  eve <- as_events(events, calendar = "CE", iteration = 1)

  gg_event_CE <- autoplot(eve, interval = "credible", select = 4)
  vdiffr::expect_doppelganger("event_CE", gg_event_CE)

  gg_event_cred_CE <- autoplot(eve, interval = "credible", level = 0.68,
                               decreasing = TRUE)
  vdiffr::expect_doppelganger("event_cred_CE", gg_event_cred_CE)

  eve <- CE_to_BP(eve)
  gg_event_hpdi_BP <- autoplot(eve, interval = "hpdi", level = 0.68,
                               decreasing = FALSE)
  vdiffr::expect_doppelganger("event_hpdi_BP", gg_event_hpdi_BP)
})
test_that("Phases", {
  skip_if_not_installed("vdiffr")

  pha <- as_phases(phases, start = c(1, 3), calendar = "CE", iteration = 1)

  gg_phase_nofacet_CE <- autoplot(pha, range = NULL, facet = FALSE)
  vdiffr::expect_doppelganger("phase_nofacet_CE", gg_phase_nofacet_CE)

  pha <- CE_to_BP(pha)
  gg_phase_facet_BP <- autoplot(pha, range = NULL, facet = TRUE,
                                decreasing = FALSE)
  vdiffr::expect_doppelganger("phase_facet_BP", gg_phase_facet_BP)
})
test_that("Succession", {
  skip_if_not_installed("vdiffr")

  pha <- as_phases(phases, start = c(1, 3), calendar = "CE", iteration = 1)

  gg_phase_hiatus_CE <- autoplot(pha, range = "hiatus", facet = TRUE,
                                 decreasing = FALSE)
  vdiffr::expect_doppelganger("phase_hiatus_CE", gg_phase_hiatus_CE)

  pha <- CE_to_BP(pha)
  gg_phase_transition_BP <- autoplot(pha, range = "transition")
  vdiffr::expect_doppelganger("phase_transition_BP", gg_phase_transition_BP)
})
