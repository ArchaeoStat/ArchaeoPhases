# Hiatus bewteen events ========================================================
eve <- as_events(mcmc_events, calendar = CE(), iteration = 1)
eve <- eve[1:10000, ]

hiatus_events <- hiatus(eve)
expect_equal_to_reference(hiatus_events, file = "_snaps/events_hiatus.rds")

# Hiatus bewteen phases ========================================================
pha <- as_phases(mcmc_phases, start = c(1, 3), calendar = CE(), iteration = 1)
pha <- pha[1:10000, , ]

hiatus_phases <- hiatus(pha)
expect_equal_to_reference(hiatus_phases, file = "_snaps/phases_hiatus.rds")
