## Data from Husi 2022
loire <- data.frame(
  lower = c(625, 700, 1200, 1225, 1250, 500, 1000, 1200,
            1325, 1375, 1200, 1300, 1375, 1275, 1325),
  upper = c(750, 825, 1250, 1275, 1325, 700, 1300, 1325,
            1400, 1500, 1300, 1375, 1500, 1325, 1425)
)

## Basic relations
allen_relation(loire$lower, loire$upper)

## Complement
(comp <- allen_complement("F")) # "pmoDseSdfOMP"

## Converse
(conv <- allen_converse(comp)) # "pmoFDseSdOMP"

## Composition
allen_composition("oFD", "oFDseS") # "pmoFD"

## Intersection
allen_intersect("pFsSf", "pmoFD") # "pF"

# Union
allen_union("pFsSf", "pmoFD") # "pmoFDsSf"
