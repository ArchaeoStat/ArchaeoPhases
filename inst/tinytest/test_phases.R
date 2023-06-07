# Build ========================================================================
eve <- as_events(mcmc_events, calendar = CE(), iteration = 1)
pha1 <- phases(eve, groups = list(phase_1 = c(1, 3), phase_2 = c(2, 4)))
pha2 <- as_phases(mcmc_phases, start = c(1, 3), calendar = CE(), iteration = 1)

expect_true(all(pha1 == pha2))
