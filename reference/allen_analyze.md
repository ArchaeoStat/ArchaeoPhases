# Analyze Composite Allen Relations

Visualize composite Allen relations with a Nokel lattice.

## Usage

``` r
allen_analyze(x, y, ...)
```

## Arguments

- x, y:

  A [`character`](https://rdrr.io/r/base/character.html) string denoting
  an Allen relation.

- ...:

  Further arguments to be passed to internal methods.

## Value

`allen_analyze()` is called it for its side-effects: it results in a
graphic being displayed.

## See also

Other Allen's intervals:
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
[`allen_plot()`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/allen_plot.md),
[`allen_relation()`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/allen_relation.md),
[`allen_relation_code()`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/allen_relation_code.md),
[`allen_union()`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/allen_union.md)

## Author

T. S. Dye

## Examples

``` r
allen_analyze("mDFo", "MdfO", main = "Composite reticulation relation")
```
