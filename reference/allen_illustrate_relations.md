# Data for an Illustrative Graphic

Create a data frame that can be used as input for an illustrative plot.
Useful for describing the Allen operators: illustrate the full set of
Allen relations, concurrent Allen relations, and relations with distinct
endpoints. Also, useful for describing the chronological domains of
stratification, branching, transformation, and reticulation.

## Usage

``` r
allen_illustrate_relations(relations = "basic")
```

## Arguments

- relations:

  A [`character`](https://rdrr.io/r/base/character.html) string
  specifying the relation. It must be one of "`basic`", "`concurrent`",
  "`distinct`", "`stratigraphic`", "`branching`", "`transformation`",
  "`reticulation`", "`sequence`", "`branch`", "`transform`", or
  "`reticulate`" (see details).

## Value

A `data.frame` to be passed to
[`allen_plot()`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/allen_plot.md).

## Details

The illustrative graphics include:

- `basic`:

  the 13 basic Allen relations (default);

- `concurrent`:

  concurrent relations;

- `distinct`:

  relations with distinct endpoints;

- `stratigraphic`:

  basic relations established by an observation of superposition;

- `branching`:

  basic branching relations;

- `transformation`:

  basic relations of transformation;

- `reticulation`:

  basic relations of reticulation;

- `sequence`:

  composite relations in a stratigraphic sequence;

- `branch`:

  composite relations of branching;

- `transform`:

  composite relations of transformation; or

- `reticulate`:

  composite relations of reticulation.

## See also

Other Allen's intervals:
[`allen_analyze()`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/allen_analyze.md),
[`allen_analyze_relations()`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/allen_analyze_relations.md),
[`allen_complement()`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/allen_complement.md),
[`allen_composition()`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/allen_composition.md),
[`allen_converse()`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/allen_converse.md),
[`allen_illustrate()`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/allen_illustrate.md),
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
