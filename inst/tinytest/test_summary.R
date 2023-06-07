# Events =======================================================================
eve <- as_events(mcmc_events, calendar = CE(), iteration = 1)
eve <- eve[1:10000, ]
expect_equal_to_reference(summary(eve), file = "_snaps/events_summary.rds", tolerance = 0.005)

# Phases =======================================================================
pha <- phases(eve, groups = list(P1 = c(1, 3), P2 = c(2, 4)))
expect_equal_to_reference(summary(pha), file = "_snaps/phases_summary.rds", tolerance = 0.005)
