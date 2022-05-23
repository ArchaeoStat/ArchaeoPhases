test_that("Tempo plot", {
  skip_if_not_installed("vdiffr")

  eve <- as_events(events, calendar = "CE", iteration = 1)
  eve <- eve[1:5000, ]

  tmp <- tempo(eve)
  gg_tempo_cred_CE <- autoplot(tmp, credible = TRUE, gauss = FALSE)
  vdiffr::expect_doppelganger("tempo_cred_CE", gg_tempo_cred_CE)

  gg_tempo_gauss_CE <- autoplot(tmp, credible = FALSE, gauss = TRUE)
  vdiffr::expect_doppelganger("tempo_gauss_CE", gg_tempo_gauss_CE)

  eve <- CE_to_BP(eve)
  tmp <- tempo(eve, count = TRUE)
  gg_tempo_BP <- autoplot(tmp, credible = TRUE, gauss = TRUE)
  vdiffr::expect_doppelganger("tempo_BPE", gg_tempo_BP)

  tmp1 <- tempo(eve[, 1:2], resolution = 5)
  tmp2 <- tempo(eve[, 3:4], resolution = 5)
  gg_tempo_multi <- multiplot(tmp1, tmp2)
  vdiffr::expect_doppelganger("tempo_multi", gg_tempo_multi)
})
test_that("Activity plot", {
  skip_if_not_installed("vdiffr")

  eve <- as_events(events, calendar = "CE", iteration = 1)
  eve <- eve[1:5000, ]

  tmp <- activity(eve)
  gg_activity_CE <- autoplot(tmp)
  vdiffr::expect_doppelganger("activity_CE", gg_activity_CE)

  eve <- CE_to_BP(eve)
  tmp <- activity(eve)
  gg_activity_BP <- autoplot(tmp)
  vdiffr::expect_doppelganger("activity_BPE", gg_activity_BP)

  tmp1 <- activity(eve[, 1:2], resolution = 5)
  tmp2 <- activity(eve[, 3:4], resolution = 5)
  gg_activity_multi <- multiplot(tmp1, tmp2)
  vdiffr::expect_doppelganger("activity_multi", gg_activity_multi)
})
test_that("Occurrence plot", {
  skip_if_not_installed("vdiffr")

  eve <- as_events(events, calendar = "CE", iteration = 1)
  eve <- eve[1:10000, ]

  tmp <- occurrence(eve, interval = "ci")
  gg_occ_cred_CE <- autoplot(tmp)
  vdiffr::expect_doppelganger("occurrence_cred_CE", gg_occ_cred_CE)

  tmp <- occurrence(eve, interval = "hpdi")
  gg_occ_hpdi_CE <- autoplot(tmp)
  vdiffr::expect_doppelganger("occurrence_hpdi_CE", gg_occ_hpdi_CE)

  eve <- CE_to_BP(eve)
  tmp <- occurrence(eve, interval = "ci")
  gg_occ_BP <- autoplot(tmp)
  vdiffr::expect_doppelganger("occurrence_cred_BP", gg_occ_cred_CE)
})
