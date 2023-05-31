## Data from Husi 2022
data("loire", package = "folio")
loire <- subset(loire, area == "Anjou")

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
