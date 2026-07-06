# Composition of Allen Relations

Composition of Allen Relations

## Usage

``` r
allen_composition(x, y, ...)

# S4 method for class 'character,character'
allen_composition(x, y)
```

## Arguments

- x, y:

  A [`character`](https://rdrr.io/r/base/character.html) vector of Allen
  relations.

- ...:

  Currently not used.

## Value

A [`character`](https://rdrr.io/r/base/character.html) vector.

## References

Allen, J. F. (1983). Maintaining Knowledge about Temporal Intervals.
*Communications of the ACM*, 26(11): 832-843.
[doi:10.1145/182.358434](https://doi.org/10.1145/182.358434) .

## See also

Other Allen's intervals:
[`allen_analyze()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/allen_analyze.md),
[`allen_analyze_relations()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/allen_analyze_relations.md),
[`allen_complement()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/allen_complement.md),
[`allen_converse()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/allen_converse.md),
[`allen_illustrate()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/allen_illustrate.md),
[`allen_illustrate_relations()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/allen_illustrate_relations.md),
[`allen_intersect()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/allen_intersect.md),
[`allen_joint_concurrency()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/allen_joint_concurrency.md),
[`allen_observe()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/allen_observe.md),
[`allen_observe_frequency()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/allen_observe_frequency.md),
[`allen_plot()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/allen_plot.md),
[`allen_relation()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/allen_relation.md),
[`allen_relation_code()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/allen_relation_code.md),
[`allen_union()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/allen_union.md)

## Author

T. S. Dye, N. Frerebeau

## Examples

``` r
## Data from Husi 2022
loire <- data.frame(
  lower = c(625, 700, 1200, 1225, 1250, 500, 1000, 1200,
            1325, 1375, 1200, 1300, 1375, 1275, 1325),
  upper = c(750, 825, 1250, 1275, 1325, 700, 1300, 1325,
            1400, 1500, 1300, 1375, 1500, 1325, 1425)
)

## Basic relations
allen_relation(loire$lower, loire$upper)
#>       [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12] [,13]
#>  [1,] NA   "o"  "p"  "p"  "p"  "O"  "p"  "p"  "p"  "p"   "p"   "p"   "p"  
#>  [2,] "O"  NA   "p"  "p"  "p"  "M"  "p"  "p"  "p"  "p"   "p"   "p"   "p"  
#>  [3,] "P"  "P"  NA   "o"  "m"  "P"  "d"  "s"  "p"  "p"   "s"   "p"   "p"  
#>  [4,] "P"  "P"  "O"  NA   "o"  "P"  "d"  "d"  "p"  "p"   "d"   "p"   "p"  
#>  [5,] "P"  "P"  "M"  "O"  NA   "P"  "O"  "f"  "m"  "p"   "O"   "o"   "p"  
#>  [6,] "o"  "m"  "p"  "p"  "p"  NA   "p"  "p"  "p"  "p"   "p"   "p"   "p"  
#>  [7,] "P"  "P"  "D"  "D"  "o"  "P"  NA   "o"  "p"  "p"   "F"   "m"   "p"  
#>  [8,] "P"  "P"  "S"  "D"  "F"  "P"  "O"  NA   "m"  "p"   "S"   "o"   "p"  
#>  [9,] "P"  "P"  "P"  "P"  "M"  "P"  "P"  "M"  NA   "o"   "P"   "O"   "o"  
#> [10,] "P"  "P"  "P"  "P"  "P"  "P"  "P"  "P"  "O"  NA    "P"   "M"   "e"  
#> [11,] "P"  "P"  "S"  "D"  "o"  "P"  "f"  "s"  "p"  "p"   NA    "m"   "p"  
#> [12,] "P"  "P"  "P"  "P"  "O"  "P"  "M"  "O"  "o"  "m"   "M"   NA    "m"  
#> [13,] "P"  "P"  "P"  "P"  "P"  "P"  "P"  "P"  "O"  "e"   "P"   "M"   NA   
#> [14,] "P"  "P"  "P"  "M"  "f"  "P"  "O"  "f"  "m"  "p"   "O"   "o"   "p"  
#> [15,] "P"  "P"  "P"  "P"  "M"  "P"  "P"  "M"  "S"  "o"   "P"   "O"   "o"  
#>       [,14] [,15]
#>  [1,] "p"   "p"  
#>  [2,] "p"   "p"  
#>  [3,] "p"   "p"  
#>  [4,] "m"   "p"  
#>  [5,] "F"   "m"  
#>  [6,] "p"   "p"  
#>  [7,] "o"   "p"  
#>  [8,] "F"   "m"  
#>  [9,] "M"   "s"  
#> [10,] "P"   "O"  
#> [11,] "o"   "p"  
#> [12,] "O"   "o"  
#> [13,] "P"   "O"  
#> [14,] NA    "m"  
#> [15,] "M"   NA   

## Complement
(comp <- allen_complement("F")) # "pmoDseSdfOMP"
#> [1] "pmoDseSdfOMP"

## Converse
(conv <- allen_converse(comp)) # "pmoFDseSdOMP"
#> [1] "pmoFDseSdOMP"

## Composition
allen_composition("oFD", "oFDseS") # "pmoFD"
#> [1] "pmoFD"

## Intersection
allen_intersect("pFsSf", "pmoFD") # "pF"
#> [1] "pF"

# Union
allen_union("pFsSf", "pmoFD") # "pmoFDsSf"
#> [1] "pmoFDsSf"
```
