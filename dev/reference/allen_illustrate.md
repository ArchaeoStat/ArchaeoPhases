# Illustrate Basic and Composite Allen Relations

Illustrate Basic and Composite Allen Relations

## Usage

``` r
allen_illustrate(relations = "basic", ...)
```

## Arguments

- relations:

  A [`character`](https://rdrr.io/r/base/character.html) string
  specifying the relation. It must be one of "`basic`", "`concurrent`",
  "`distinct`", "`stratigraphic`", "`branching`", "`transformation`",
  "`reticulation`", "`sequence`", "`branch`", "`transform`", or
  "`reticulate`" (see details).

- ...:

  Further arguments to be passed to internal methods.

## Value

`allen_illustrate()` is called it for its side-effects: it results in a
graphic being displayed.

## Details

Illustrate basic and composite Allen relations for several chronological
model domains with a Nokel lattice. Chronological model domains include
stratigraphy and branching, transformative, and reticulate processes of
artifact change.

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

## References

Harris, E. C. (1997). *Principles of Archaeological Stratigraphy*.
Second edition. London: Academic Press.

Lyman, R. L. and O'Brien, M. J. (2017). "Sedation and Cladistics: The
Difference between Anagenetic and Cladogenetic Evolution". In *Mapping
Our Ancestors: Phylogenetic Approaches in Anthropology and Prehistory*,
edited by Lipo, C. P., O'Brien, M. J., Couard, M., and Shennan, S. J.
New York: Routledge.
[doi:10.4324/9780203786376](https://doi.org/10.4324/9780203786376) .

Viola, T. (2020). *Peirce on the Uses of History*. De Gruyter.
[doi:10.1515/9783110651560](https://doi.org/10.1515/9783110651560) . See
chapter 3, "Historicity as Process", especially p. 83-88.

## See also

Other Allen's intervals:
[`allen_analyze()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/allen_analyze.md),
[`allen_analyze_relations()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/allen_analyze_relations.md),
[`allen_complement()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/allen_complement.md),
[`allen_composition()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/allen_composition.md),
[`allen_converse()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/allen_converse.md),
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

T. S. Dye

## Examples

``` r
## Plot the basic Allen relations
allen_illustrate()
```
