context("Sensitivity")
library(ArchaeoPhases)

test_that("Identity range is zero", {
    res <- estimate_range(mcmc = c("test-data/oxcal.csv", "test-data/oxcal.csv"),
                          position = c(1, 2),
                          app = "oxcal",
                          estimates = c("mean", "q1", "median", "q3", "ci.inf", "ci.sup"),
                          quiet = "yes")
    expect_equal(sum(res$range_table), 0)
})
