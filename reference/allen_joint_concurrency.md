# Joint Concurrence of Two or More Observed Intervals

Estimates the age of an undated context based on the known depositional
history of associated artifacts.

## Usage

``` r
allen_joint_concurrency(x, groups, ...)

# S4 method for class 'EventsMCMC,list'
allen_joint_concurrency(x, groups, ...)
```

## Arguments

- x:

  An
  [`EventsMCMC`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/EventsMCMC-class.md)
  object containing the output of the MCMC algorithm.

- groups:

  A [`list`](https://rdrr.io/r/base/list.html) of (named) vector of
  names or indexes of columns in `x` (see
  [`phases()`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/phases.md)).

- ...:

  Currently not used.

## Value

A
[`PhasesMCMC`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/PhasesMCMC-class.md)
object.

## See also

Other Allen's intervals:
[`allen_analyze()`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/allen_analyze.md),
[`allen_analyze_relations()`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/allen_analyze_relations.md),
[`allen_complement()`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/allen_complement.md),
[`allen_composition()`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/allen_composition.md),
[`allen_converse()`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/allen_converse.md),
[`allen_illustrate()`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/allen_illustrate.md),
[`allen_illustrate_relations()`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/allen_illustrate_relations.md),
[`allen_intersect()`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/allen_intersect.md),
[`allen_observe()`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/allen_observe.md),
[`allen_observe_frequency()`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/allen_observe_frequency.md),
[`allen_plot()`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/allen_plot.md),
[`allen_relation()`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/allen_relation.md),
[`allen_relation_code()`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/allen_relation_code.md),
[`allen_union()`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/allen_union.md)

## Author

T. S. Dye
