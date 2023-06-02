# Transition bewteen phases ====================================================
pha <- as_phases(mcmc_phases, start = c(1, 3), calendar = CE(), iteration = 1)
pha <- pha[1:10000, , ]

transition_phases <- transition(pha)
expect_equal_to_reference(transition_phases, file = "_snaps/phases_transition.rds")
