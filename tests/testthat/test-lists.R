context("Non-graphical functions return lists")
library(ArchaeoPhases)

test_that("DatesHiatus and dates_hiatus return equivalent results", {
    oxcal <- read_oxcal("test-data/oxcal.csv", quiet = "yes")
    original <- DatesHiatus(unlist(oxcal[, 1]), unlist(oxcal[, 2]))
    new <- dates_hiatus(oxcal[, 1], oxcal[, 2])
    expect_equivalent(original[c(2, 3)], new$hiatus)
})

test_that("PhasesGap and phases_gap return equivalent results", {
    data(Phases)
    original <- PhasesGap(Phases$Phase.1.beta, Phases$Phase.2.alpha, 0.95)
    new <- phases_gap(Phases$Phase.1.beta, Phases$Phase.2.alpha, 0.95)
    expect_equivalent(original[c(2, 3)], new$hiatus)
})

test_that("MarginalStatistics and marginal_statistics are equivalent", {
    oxcal <- read_oxcal("test-data/oxcal.csv", quiet = "yes")
    original <- MarginalStatistics(oxcal[, 1])
    new <- marginal_statistics(oxcal[, 1])
    new <- unlist(new)
    expect_equivalent(original["mean", 1], new["mean"])
    expect_equivalent(original["MAP", 1], new["map"], tolerance = 1)
    expect_equivalent(original["sd", 1], new["sd"])
    expect_equivalent(original["Q1", 1], new["quantiles.q1"])
    expect_equivalent(original["median", 1], new["quantiles.median"])
    expect_equivalent(original["Q2", 1], new["quantiles.q3"])
    expect_equivalent(original["level", 1], new["level"])
    expect_equivalent(original["Credible Interval Inf", 1], new["ci.inf"])
    expect_equivalent(original["Credible Interval Sup", 1], new["ci.sup"])
    expect_equivalent(original["HPDR Inf 1", 1], new["hpdr.inf_1"], tolerance = 1)
    expect_equivalent(original["HPDR Sup 1", 1], new["hpdr.sup_1"], tolerance = 1)
    expect_equivalent(original["HPDR Inf 2", 1], new["hpdr.inf_2"], tolerance = 1)
    expect_equivalent(original["HPDR Sup 2", 1], new["hpdr.sup_2"], tolerance = 1)
})

test_that("MultiCredibleInterval and multi_credible_interval are equivalent", {
    oxcal <- read_oxcal("test-data/oxcal.csv", quiet = "yes")
    original <- MultiCredibleInterval(as.data.frame(oxcal), c(1,2))
    new <- multi_credible_interval(oxcal, c(1, 2))
    expect_equivalent(original["foo-early", ][c(2, 3)], unlist(new$ci["foo-early",]))
    expect_equivalent(original["foo-late", ][c(2, 3)], unlist(new$ci["foo-late",]))
})

test_that("CredibleInterval and credible_interval are equivalent", {
    oxcal <- read_oxcal("test-data/oxcal.csv", quiet = "yes")
    original <- CredibleInterval(as.data.frame(oxcal)[, 1])
    new <- credible_interval(oxcal[, 1])
    expect_equivalent(original[c(2, 3)], new$ci)
})

test_that("MultiHPD and multi_hpd are equivalent", {
    oxcal <- read_oxcal("test-data/oxcal.csv", quiet = "yes")
    original <- MultiHPD(as.data.frame(oxcal), c(1, 2), roundingOfValue = -1)
    new <- multi_hpd(oxcal, c(1, 2), round_to = -1)
    expect_equivalent(original[, c(2, 3)], new$results[, c(1, 2)], tolerance = 10)
})

test_that("PhaseStatistics and phase_statistics are equivalent", {
    data(Phases)
    original <- PhaseStatistics(Phases$Phase.2.alpha,
                                Phases$Phase.2.beta,
                                0.68)
    new <- phase_statistics(Phases$Phase.2.alpha,
                            Phases$Phase.2.beta,
                            0.68)
    expect_equivalent(as.data.frame(original[-7, ]),
                      new$statistics[-c(4, 8), ], tolerance = 0.1)
})
