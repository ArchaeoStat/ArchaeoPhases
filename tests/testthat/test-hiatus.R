test_that("hiatus bewteen events exists", {
  eve <- as_events(events, calendar = "CE", iteration = 1)
  eve <- eve[1:10000, ]

  hia_CE <- hiatus(eve)
  expect_snapshot(as.data.frame(hia_CE))

  eve <- CE_to_BP(eve)
  hia_BP <- hiatus(eve)
  expect_snapshot(as.data.frame(hia_BP))
})
