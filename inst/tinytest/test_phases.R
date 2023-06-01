# Summary ======================================================================
eve <- as_events(mcmc_events, calendar = CE(), iteration = 1)
pha1 <- phases(eve, groups = list(phase_1 = c(1, 3), phase_2 = c(2, 4)))
pha2 <- as_phases(mcmc_phases, start = c(1, 3), calendar = CE(), iteration = 1)

expect_true(all(pha1 == pha2))

pha2 <- pha2[1:10000, , ]
expect_equal_to_reference(summary(pha2), file = "_snaps/phases_summary.rds")
