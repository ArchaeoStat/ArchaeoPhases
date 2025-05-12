Sys.setenv(LANGUAGE = "en") # Force locale

# Test for anteriority =========================================================
eve <- as_events(mcmc_events, calendar = CE(), iteration = 1)
eve <- eve[1:10000, ]

old_21 <- older(eve[, 2, drop = TRUE], eve[, 1, drop = TRUE])
expect_identical(old_21, 1)

old <- older(eve[, 1:2])
expect_equivalent(old, matrix(c(0, 1, 0, 0), nrow = 2))
