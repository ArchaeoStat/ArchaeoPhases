# Package index

## Data input/output

- [`as_coda()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/as_coda.md)
  : Coerce to Coda
- [`as_events()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/as_events.md)
  : Coerce to Events
- [`as_phases()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/as_phases.md)
  : Coerce to Phases
- [`is_original()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/check.md)
  : Check for an Original MCMC File
- [`read_bcal()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/read_bcal.md)
  : Read BCal Output
- [`read_chronomodel_events()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/read_chronomodel.md)
  [`read_chronomodel_phases()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/read_chronomodel.md)
  : Read ChronoModel Output
- [`read_oxcal()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/read_oxcal.md)
  : Read OxCal Output

## Rhythms

- [`activity()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/activity.md)
  [`plot(`*`<ActivityEvents>`*`,`*`<missing>`*`)`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/activity.md)
  : Activity Plot
- [`elapse()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/elapse.md)
  : Elapsed Time Scale
- [`occurrence()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/occurrence.md)
  : Occurrence Plot
- [`tempo()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/tempo.md)
  [`plot(`*`<CumulativeEvents>`*`,`*`<missing>`*`)`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/tempo.md)
  : Tempo Plot

## Phases

- [`duration()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/duration.md)
  : Phase Duration
- [`phases()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/phases.md)
  : Compute Phases

## Time Ranges

- [`boundaries()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/boundaries.md)
  : Phase Time Range
- [`hiatus()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/hiatus.md)
  : Hiatus Between Two Dates
- [`transition()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/transition.md)
  : Transition Range Between Successive Phases

## Age-Depth Modeling

- [`bury()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/bury.md)
  [`predict(`*`<AgeDepthModel>`*`)`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/bury.md)
  [`plot(`*`<AgeDepthModel>`*`,`*`<missing>`*`)`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/bury.md)
  : Age-Depth Modeling
- [`interpolate()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/interpolate.md)
  : Interpolate Between Two Dates

## Visualization

- [`plot(`*`<MCMC>`*`,`*`<missing>`*`)`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/plot_events.md)
  : Plot Events
- [`plot(`*`<PhasesMCMC>`*`,`*`<missing>`*`)`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/plot_phases.md)
  : Plot Phases

## Statistics

- [`interval_credible()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/interval_credible.md)
  : Bayesian Credible Interval
- [`interval_hdr()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/interval_hdr.md)
  : Bayesian HPD Regions
- [`sensitivity()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/sensitivity.md)
  : Sensitivity
- [`summary(`*`<MCMC>`*`)`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/summary.md)
  [`summary(`*`<PhasesMCMC>`*`)`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/summary.md)
  : Marginal Summary Statistics for Multiple MCMC Chains

## Tests

- [`older()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/older.md)
  : Bayesian Test for Anteriority/Posteriority

## Allen’s Interval Algebra

- [`allen_analyze()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/allen_analyze.md)
  : Analyze Composite Allen Relations
- [`allen_complement()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/allen_complement.md)
  : Complement of an Allen Relation
- [`allen_composition()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/allen_composition.md)
  : Composition of Allen Relations
- [`allen_converse()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/allen_converse.md)
  : Converse of an Allen Relation
- [`allen_illustrate()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/allen_illustrate.md)
  : Illustrate Basic and Composite Allen Relations
- [`allen_intersect()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/allen_intersect.md)
  : Intersection of Allen Relations
- [`allen_joint_concurrency()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/allen_joint_concurrency.md)
  : Joint Concurrence of Two or More Observed Intervals
- [`allen_observe()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/allen_observe.md)
  : Observe the Relation Between two Phases
- [`allen_observe_frequency()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/allen_observe_frequency.md)
  : Observed Frequency of an Allen Set
- [`allen_relation()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/allen_relation.md)
  : Allen Relation Between Definite Intervals
- [`allen_relation_code()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/allen_relation_code.md)
  [`allen_relation_string()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/allen_relation_code.md)
  [`allen_relation_concurrent()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/allen_relation_code.md)
  [`allen_relation_distinct()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/allen_relation_code.md)
  : The Basic Allen Relation Set
- [`allen_union()`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/allen_union.md)
  : Union of Allen Relations

## Coerce

- [`as.data.frame(`*`<CumulativeEvents>`*`)`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/data.frame.md)
  [`as.data.frame(`*`<ActivityEvents>`*`)`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/data.frame.md)
  [`as.data.frame(`*`<OccurrenceEvents>`*`)`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/data.frame.md)
  [`as.data.frame(`*`<TimeRange>`*`)`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/data.frame.md)
  : Coerce to a Data Frame

## Subset

- [`cbind2(`*`<MCMC>`*`,`*`<MCMC>`*`)`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/bind.md)
  : Combine two MCMC Objects
- [`` `[`( ``*`<MCMC>`*`)`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/subset.md)
  [`` `[`( ``*`<PhasesMCMC>`*`)`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/subset.md)
  : Extract or Replace Parts of an Object

## Tools

- [`names(`*`<MCMC>`*`)`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/names.md)
  [`` `names<-`( ``*`<MCMC>`*`)`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/names.md)
  [`names(`*`<PhasesMCMC>`*`)`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/names.md)
  [`` `names<-`( ``*`<PhasesMCMC>`*`)`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/names.md)
  : The Names of an Object
- [`sort(`*`<MCMC>`*`)`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/sort.md)
  [`sort(`*`<PhasesMCMC>`*`)`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/sort.md)
  : Sort an MCMC Object
- [`sort.list(`*`<MCMC>`*`)`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/sort.list.md)
  [`sort.list(`*`<PhasesMCMC>`*`)`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/sort.list.md)
  : Ordering Permutation of an MCMC Object

## Datasets

- [`mcmc_events`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/mcmc_events.md)
  : Events
- [`mcmc_phases`](https://ArchaeoStat.github.io/ArchaeoPhases/dev/reference/mcmc_phases.md)
  : Phases
