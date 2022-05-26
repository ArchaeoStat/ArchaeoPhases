test_that("Credible interval", {
  eve <- as_events(events, calendar = "CE", iteration = 1)
  eve <- eve[1:10000, ]

  cred_CE <- credible(eve, level = 0.68, simplify = FALSE)
  expect_snapshot(cred_CE)

  eve <- CE_to_BP(eve)
  cred_BP <- credible(eve, level = 0.68)
  expect_snapshot(cred_BP)
})
test_that("HPD interval", {
  eve <- as_events(events, calendar = "CE", iteration = 1)
  eve <- eve[1:10000, ]

  hpdi_CE <- hpdi(eve, level = 0.68, simplify = FALSE)
  expect_snapshot(hpdi_CE)

  eve <- CE_to_BP(eve)
  hpdi_BP <- hpdi(eve, level = 0.68)
  expect_snapshot(hpdi_BP)
})
