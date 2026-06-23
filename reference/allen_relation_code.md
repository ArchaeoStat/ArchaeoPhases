# The Basic Allen Relation Set

The Basic Allen Relation Set

## Usage

``` r
allen_relation_code(...)

allen_relation_string(...)

allen_relation_concurrent(...)

allen_relation_distinct(...)
```

## Arguments

- ...:

  Currently not used.

## Value

- `allen_relation_code()` returns a
  [`character`](https://rdrr.io/r/base/character.html) vector of
  one-letter codes for the thirteen basic Allen relations.

- `allen_relation_string()` returns a
  [`character`](https://rdrr.io/r/base/character.html) vector of string
  descriptors of the Allen basic relations.

- `allen_relation_concurrent()` returns a
  [`character`](https://rdrr.io/r/base/character.html) vector of nine
  one-letter codes for the Allen concurrent relations.

- `allen_relation_distinct()` returns the six value Allen relation set
  for intervals with distinct endpoints.

## Note

The codes were proposed by Thomas Alspaugh.

## References

Allen, J. F. (1983). Maintaining Knowledge about Temporal Intervals.
*Communications of the ACM*, 26(11): 832-843.
[doi:10.1145/182.358434](https://doi.org/10.1145/182.358434) .

Alspaugh, T. (2019). Allen's Interval Algebra. URL:
<https://thomasalspaugh.org/pub/fnd/allen.html>.

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
[`allen_plot()`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/allen_plot.md),
[`allen_relation()`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/allen_relation.md),
[`allen_union()`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/allen_union.md)

## Author

T. S. Dye
