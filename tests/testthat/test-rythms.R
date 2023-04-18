test_that("Tempo plot", {
  skip_if_not_installed("vdiffr")

  eve <- as_events(events, calendar = "CE", iteration = 1)
  eve <- eve[1:5000, ]

  tmp <- tempo(eve)
  plot_tempo_cred_CE <- function() plot(tmp, credible = TRUE, gauss = FALSE)
  vdiffr::expect_doppelganger("plot_tempo_cred_CE", plot_tempo_cred_CE)

  plot_tempo_gauss_CE <- function() plot(tmp, credible = FALSE, gauss = TRUE)
  vdiffr::expect_doppelganger("plot_tempo_gauss_CE", plot_tempo_gauss_CE)

  eve <- CE_to_BP(eve)
  tmp <- tempo(eve, count = TRUE)
  plot_tempo_BP <- function() plot(tmp, credible = TRUE, gauss = TRUE)
  vdiffr::expect_doppelganger("plot_tempo_BP", plot_tempo_BP)

  tmp1 <- tempo(eve[, 1:2], resolution = 5)
  tmp2 <- tempo(eve[, 3:4], resolution = 5)
  plot_tempo_multi <- function() multiplot(tmp1, tmp2)
  vdiffr::expect_doppelganger("plot_tempo_multi", plot_tempo_multi)
})
test_that("Activity plot", {
  skip_if_not_installed("vdiffr")

  eve <- as_events(events, calendar = "CE", iteration = 1)
  eve <- eve[1:5000, ]

  tmp <- activity(eve)
  plot_activity_CE <- function() plot(tmp)
  vdiffr::expect_doppelganger("plot_activity_CE", plot_activity_CE)

  eve <- CE_to_BP(eve)
  tmp <- activity(eve)
  plot_activity_BP <- function() plot(tmp)
  vdiffr::expect_doppelganger("plot_activity_BP", plot_activity_BP)

  tmp1 <- activity(eve[, 1:2], resolution = 5)
  tmp2 <- activity(eve[, 3:4], resolution = 5)
  plot_activity_multi <- function() multiplot(tmp1, tmp2)
  vdiffr::expect_doppelganger("plot_activity_multi", plot_activity_multi)
})
test_that("Occurrence plot", {
  skip_if_not_installed("vdiffr")

  eve <- as_events(events, calendar = "CE", iteration = 1)
  eve <- eve[1:10000, ]

  tmp <- occurrence(eve)
  plot_occ_CE <- function() plot(tmp)
  vdiffr::expect_doppelganger("plot_occ_CE", plot_occ_CE)

  eve <- CE_to_BP(eve)
  tmp <- occurrence(eve)
  plot_occ_BP <- function() plot(tmp)
  vdiffr::expect_doppelganger("plot_occ_BP", plot_occ_BP)
})
