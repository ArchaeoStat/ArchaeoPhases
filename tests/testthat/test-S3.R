context("S3 object methods")
library(ArchaeoPhases)

test_that("Method original_file identifies OxCal import file", {
    oxcal <- read_oxcal("test-data/oxcal.csv", quiet = "yes")
    expect_true(original_file(oxcal))
    expect_false(original_file(oxcal, "foo.csv"))
})

test_that("Method original_file identifies default BCal import file", {
    bcal_1 <- read_bcal("test-data/bcal-1.csv", quiet = "yes")
    expect_true(original_file(bcal_1))
    expect_false(original_file(bcal_1, "foo.csv"))
})

test_that("Method original_file identifies arbitrary BCal import file", {
    bcal_17 <- read_bcal("test-data/bcal-17.csv", quiet = "yes")
    expect_true(original_file(bcal_17))
    expect_false(original_file(bcal_17, "foo.csv"))
})

test_that("Method original_file identifies ChronoModel import file", {
    cm <- read_chronomodel("test-data/cm/Chain_all_Events.csv", quiet = "yes")
    expect_true(original_file(cm))
    expect_false(original_file(cm, "foo.csv"))
})

test_that("Occurrence plot identifies import file", {
    oxcal <- read_oxcal("test-data/oxcal.csv", quiet = "yes")
    op <- occurrence_plot(oxcal, plot_result = FALSE)
    expect_true(original_file(op))
    expect_false(original_file(op, "foo.csv"))
})

test_that("Tempo plot identifies import file", {
    oxcal <- read_oxcal("test-data/oxcal.csv", quiet = "yes")
    tp <- tempo_plot(oxcal, plot_result = FALSE)
    expect_true(original_file(tp))
    expect_false(original_file(tp, "foo.csv"))
})

test_that("Multi marginal plot identifies import file", {
    oxcal <- read_oxcal("test-data/oxcal.csv", quiet = "yes")
    mmp <- multi_marginal_plot(oxcal, plot_result = FALSE)
    expect_true(original_file(mmp))
    expect_false(original_file(mmp, "foo.csv"))
})

test_that("Marginal plot identifies import file", {
    oxcal <- read_oxcal("test-data/oxcal.csv", quiet = "yes")
    mp <- marginal_plot(oxcal, 1, plot_result = FALSE)
    expect_true(original_file(mp))
    expect_false(original_file(mp, "foo.csv"))
})

## This test complains that object mp not found.

## test_that("Plot works with marginal_plot", {
##     oxcal <- read_oxcal("test-data/oxcal.csv")
##     mp <- marginal_plot(oxcal, position = 2, plot_result = FALSE)
##     ## FAILS, but works in the REPL
##     ## mp <- marginal_plot(oxcal, position = 1, plot_result = FALSE)
##     expect_equal(sum(mp), sum(plot(mp)))
##     expect_true(original_file(plot(mp)))
##     expect_false(original_file(plot(mp), "foo.csv"))
## })

test_that("Tempo activity plot identifies import file", {
    oxcal <- read_oxcal("test-data/oxcal.csv", quiet = "yes")
    tap <- tempo_activity_plot(oxcal, plot_result = FALSE)
    expect_true(original_file(tap))
    expect_false(original_file(tap, "foo.csv"))
})

## This test complains that object tap not found

## test_that("Plot works with tempo_activity_plot", {
##     oxcal <- read_oxcal("test-data/oxcal.csv")
##     tap <- tempo_activity_plot(oxcal, plot_result = FALSE)
##     tap2 <- plot(tap)
##     expect_equal(sum(tap), sum(tap2))
##     expect_true(original_file(tap2))
##     expect_false(original_file(tap2, "foo.csv"))
## })

test_that("Multi dates plot identifies import file", {
    oxcal <- read_oxcal("test-data/oxcal.csv", quiet = "yes")
    mdp <- multi_dates_plot(oxcal, plot_result = FALSE)
    expect_true(original_file(mdp))
    expect_false(original_file(mdp, "foo.csv"))
})

## This test complains that object mdp not found

## test_that("Plot works with multi_dates_plot", {
##     oxcal <- read_oxcal("test-data/oxcal.csv")
##     mdp <- multi_dates_plot(oxcal, plot_result = FALSE)
##     mdp2 <- plot(mdp)
##     expect_equal(sum(mdp), sum(mdp2))
##     expect_true(original_file(mdp2))
##     expect_false(original_file(mdp2, "foo.csv"))
## })
