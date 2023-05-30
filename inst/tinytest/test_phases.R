# Summary ======================================================================
pha <- as_phases(mcmc_phases, start = c(1, 3), calendar = CE(), iteration = 1)
pha <- pha[1:10000, , ]

expect_equal_to_reference(summary(pha), file = "_snaps/phases_summary.rds")
