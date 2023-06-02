# Basic relations ==============================================================
xmin <- c(1, 3, 3, 3, 3, 4, 4, 4, 2, 4, 4, 4, 4)
xmax <- c(2, 4, 5, 6, 7, 5, 6, 7, 4, 6, 6, 6, 6)
ymin <- c(4, 4, 4, 4, 4, 4, 4, 4, 1, 2, 3, 3, 1)
ymax <- c(6, 6, 6, 6, 6, 6, 6, 6, 5, 6, 5, 4, 2)

expect_equal(ArchaeoPhases:::.allen_relation(xmin, xmax, ymin, ymax), allen_relation_code())

# Relations ====================================================================
z <- matrix(c(NA, "f", "F", NA), nrow = 2)
expect_equal(allen_relation(c(1, 3), c(4, 4)), z)

expect_error(allen_relation(c(4, 3), c(1, 4, 3)))
expect_error(allen_relation(c(4, 3), c(1, 4)))

# Complement ====================================================================
x <- c("p", "pmoFD", "")
z <- c("moFDseSdfOMP", "seSdfOMP", "pmoFDseSdfOMP")
expect_equal(allen_complement(x), z)

i <- matrix(c(NA, "f", "F", NA), nrow = 2)
k <- matrix(c(NA, "pmoFDseSdOMP", "pmoDseSdfOMP", NA), nrow = 2)
expect_equal(allen_complement(i), k)

# Converse =====================================================================
x <- c("p", "pmoFD", "mM", "")
z <- c("P", "dfOMP", "mM", "")
expect_equal(allen_converse(x), z)

i <- matrix(c(NA, "f", "F", NA), nrow = 2)
expect_equal(allen_converse(i), t(i))

# Composition ==================================================================
x <- c("m", "pm", "oFD")
y <- c("m", "pm", "oFDseS")
z <- c("p", "p", "pmoFD")

expect_equal(allen_composition(x, y), z)

# Intersection =================================================================
x <- c("pmo", "pFsSf", "pmo")
y <- c("FDseS", "pmoFD", "pmo")
z <- c("", "pF", "pmo")

expect_equal(allen_intersect(x, y), z)

# Union ========================================================================
x <- c("pmo", "pFsSf", "pmo")
y <- c("FDseS", "pmoFD", "pmo")
z <- c("pmoFDseS", "pmoFDsSf", "pmo")

expect_equal(allen_union(x, y), z)
