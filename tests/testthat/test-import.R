
test_that("ImportCSV works with OxCal", {
    oxcal <- ImportCSV("test-data/oxcal.csv")
    expect_equal(dim(oxcal), c(1000, 4))
    expect_equivalent(names(oxcal),
                      c("Pass", "foo.early", "foo.late", "X"))
    expect_equivalent(oxcal[1, 1:3], c(0, 1026.41, 1048.33),
                      tolerance = 0.01)
    expect_equivalent(oxcal[1000, 1:3], c(19980, 1135.31, 1167.27),
                      tolerance = 0.01)
})

test_that("read_oxcal works", {
    oxcal <- read_oxcal("test-data/oxcal.csv")
    expect_equal(dim(oxcal), c(1000, 2))
    expect_equivalent(names(oxcal), c("foo-early", "foo-late"))
    expect_equivalent(as.data.frame(oxcal)[1, ], c(1026.41, 1048.33),
                      tolerance = 0.01)
    expect_equivalent(as.data.frame(oxcal)[1000, ], c(1135.31, 1167.27),
                      tolerance = 0.01)
})

## BCal mcmc output has changed, need to find an older file to use as a test
## test_that("ImportCSV works with BCal default bin width", {
##     bcal <- ImportCSV.BCal("test-data/bcal-1.csv")
##     expect_equal(dim(bcal), c(293704, 5))
##     expect_equivalent(names(bcal), c("beta.1..test.", "theta.2..test.",
##                                      "theta.1..test.", "alpha.1..test.", "X"))
##     expect_equivalent(bcal[1, 1:4], c(1949, 1160, 1112, -2349))
##     expect_equivalent(bcal[293704, 1:4], c(1950, 1126, 997, 877))
## })

test_that("read_bcal works with default bin width", {
    bcal <- read_bcal("test-data/bcal-1.csv")
    expect_equal(dim(bcal), c(293705, 4))
    expect_equivalent(colnames(bcal), c("beta 1 (test)", "theta 2 (test)",
                                        "theta 1 (test)", "alpha 1 (test)"))
    expect_equivalent(bcal[1, ], c(1949, 1160, 1112, -2349))
    expect_equivalent(bcal[293705, ], c(1950, 1126, 997, 877))
})

## BCal mcmc output has changed, need to find an older file to use as a test
## test_that("ImportCSV works with BCal custom bin width", {
##     bcal <- ImportCSV.BCal("test-data/bcal-17.csv",
##                            bin.width = 17)
##     expect_equal(dim(bcal), c(295941, 5))
##     expect_equivalent(names(bcal), c("beta.1..test.", "theta.2..test.",
##                                      "theta.1..test.", "alpha.1..test.", "X"))
##     expect_equivalent(bcal[1, 1:4], c(1491, 1168, 1100, 216))
##     expect_equivalent(bcal[295941, 1:4], c(1219, 1083, 1015, 879))
## })

test_that("read_bcal works with custom bin width", {
    bcal <- read_bcal("test-data/bcal-17.csv", bin_width = 17)
    expect_equal(dim(bcal), c(294770, 4))
    expect_equivalent(colnames(bcal), c("beta 1 (test)", "theta 2 (test)",
                                        "theta 1 (test)", "alpha 1 (test)"))
    expect_equivalent(bcal[1, ], c(1389, 1032, 1032, 318))
    expect_equivalent(bcal[294770, ], c(1406, 1168, 1100, 1032))
})

test_that("ImportCSV works with ChronoModel", {
    chronomodel <- ImportCSV("test-data/cm/Chain_all_Events.csv")
    expect_equal(dim(chronomodel), c(30000, 3))
    expect_equivalent(names(chronomodel), c("iter", "foo.late", "foo.early"))
    expect_equivalent(as.vector(chronomodel[1, ]),
                 c(7001, 1073.91, 969.5635), tolerance = 0.001)
    expect_equivalent(as.vector(chronomodel[30000, ]), c(50002, 1107.239, 984.2023),
                 tolerance = 0.001)
})

test_that("read_chronomodel works", {
    cm <- read_chronomodel("test-data/cm/Chain_all_Events.csv")
    expect_equal(dim(cm), c(30000, 2))
    expect_equivalent(names(cm), c("foo-late", "foo-early"))
    expect_equivalent(as.data.frame(cm)[1, ],
                      c(1073.91, 969.5635), tolerance = 0.001)
    expect_equivalent(as.data.frame(cm)[30000, ], c(1107.239, 984.2023),
                      tolerance = 0.001)
})
