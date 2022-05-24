test_that("Summary", {
  pha <- as_phases(phases, start = c(1, 3), calendar = "CE", iteration = 1)
  pha <- pha[1:10000, , ]

  expect_snapshot(summary(pha))
})
test_that("Duration", {
  pha <- as_phases(phases, start = c(1, 3), calendar = "CE", iteration = 1)
  pha <- pha[1:10000, , ]

  pha_CE <- duration(pha)
  expect_snapshot(summary(pha_CE))

  pha <- CE_to_BP(pha)
  pha_BP <- duration(pha)
  expect_identical(summary(pha_CE), summary(pha_BP))
})
test_that("Boundaries", {
  pha <- as_phases(phases, start = c(1, 3), calendar = "CE", iteration = 1)
  pha <- pha[1:10000, , ]

  pha_CE <- boundaries(pha)
  expect_snapshot(pha_CE)

  pha <- CE_to_BP(pha)
  pha_BP <- boundaries(pha)
  expect_snapshot(pha_BP)
})
test_that("Transitions", {
  pha <- as_phases(phases, start = c(1, 3), calendar = "CE", iteration = 1)
  pha <- pha[1:10000, , ]

  pha_CE <- transition(pha)
  expect_snapshot(as.data.frame(pha_CE))

  pha <- CE_to_BP(pha)
  pha_BP <- transition(pha)
  expect_snapshot(as.data.frame(pha_BP))
})
test_that("Hiatus", {
  pha <- as_phases(phases, start = c(1, 3), calendar = "CE", iteration = 1)
  pha <- pha[1:10000, , ]

  pha_CE <- hiatus(pha)
  expect_snapshot(as.data.frame(pha_CE))

  pha <- CE_to_BP(pha)
  pha_BP <- hiatus(pha)
  expect_snapshot(as.data.frame(pha_BP))
})
