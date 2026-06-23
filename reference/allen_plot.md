# Make a Single Plot of a Nökel Lattice.

Plots a Nökel lattice.

## Usage

``` r
allen_plot(x, main = NULL, sub = NULL, ann = graphics::par("ann"), ...)
```

## Arguments

- x:

  A
  [`data.frame`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/data.frame.md)
  with plot information, such as the one produced by
  [`allen_illustrate_relations()`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/allen_illustrate_relations.md).

- main:

  A [`character`](https://rdrr.io/r/base/character.html) string giving a
  main title for the plot.

- sub:

  A [`character`](https://rdrr.io/r/base/character.html) string giving a
  subtitle for the plot.

- ann:

  A [`logical`](https://rdrr.io/r/base/logical.html) scalar: should the
  default annotation appear on the plot?

- ...:

  Currently not used.

## Value

`allen_plot()` is called it for its side-effects: it results in a
graphic being displayed (invisibly returns `x`).

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
[`allen_joint_concurrency()`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/allen_joint_concurrency.md),
[`allen_observe()`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/allen_observe.md),
[`allen_observe_frequency()`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/allen_observe_frequency.md),
[`allen_relation()`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/allen_relation.md),
[`allen_relation_code()`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/allen_relation_code.md),
[`allen_union()`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/allen_union.md)

## Author

T. S. Dye, N. Frerebeau
