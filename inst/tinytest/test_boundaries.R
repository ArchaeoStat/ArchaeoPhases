Sys.setlocale("LC_MESSAGES", 'en_GB.UTF-8') # Force locale
options(ArchaeoPhases.calendar = calendar("CE"))

# Transition bewteen phases ====================================================
pha <- as_phases(mcmc_phases, start = c(1, 3), calendar = CE(), iteration = 1)
pha <- pha[1:10000, , ]

boundaries_phases <- boundaries(pha)
expect_equal_to_reference(boundaries_phases, file = "_snaps/phases_boundaries.rds")
