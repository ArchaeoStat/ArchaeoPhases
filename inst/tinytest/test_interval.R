Sys.setenv(LANGUAGE = "en") # Force locale

# Credible interval ============================================================
eve <- as_events(mcmc_events, calendar = CE(), iteration = 1)
eve <- eve[1:10000, ]

credible_CE <- interval_credible(eve, level = 0.68)
expect_equal_to_reference(credible_CE, file = "_snaps/credible_CE.rds")

credible_BP <- interval_credible(eve, level = 0.68, calendar = BP())
expect_equal_to_reference(credible_BP, file = "_snaps/credible_BP.rds")

# HDR ==========================================================================
eve <- as_events(mcmc_events, calendar = CE(), iteration = 1)
eve <- eve[1:10000, ]

hdr_CE <- interval_hdr(eve, level = 0.68)
expect_equal_to_reference(hdr_CE, file = "_snaps/hdr_CE.rds")

hdr_BP <- interval_hdr(eve, level = 0.68, calendar = BP())
expect_equal_to_reference(hdr_BP, file = "_snaps/hdr_BP.rds")
