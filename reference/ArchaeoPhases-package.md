# ArchaeoPhases: Post-Processing of Markov Chain Monte Carlo Simulations for Chronological Modelling

Statistical analysis of archaeological dates and groups of dates. This
package allows to post-process Markov Chain Monte Carlo (MCMC)
simulations from 'ChronoModel' <https://chronomodel.com/>, 'Oxcal'
<https://c14.arch.ox.ac.uk/oxcal.html> or 'BCal'
<https://bcal.shef.ac.uk/>. It provides functions for the study of
rhythms of the long term from the posterior distribution of a series of
dates (tempo and activity plot). It also allows the estimation and
visualization of time ranges from the posterior distribution of groups
of dates (e.g. duration, transition and hiatus between successive
phases) as described in Philippe and Vibet (2020)
[doi:10.18637/jss.v093.c01](https://doi.org/10.18637/jss.v093.c01) .

## Details

|  |  |
|----|----|
| **Version** | 2.1.1 |
| **License** | GPL-3 |
| **CRAN DOI** | [doi:10.32614/CRAN.package.ArchaeoPhases](https://doi.org/10.32614/CRAN.package.ArchaeoPhases) |
| **Zenodo DOI** | [doi:10.5281/zenodo.8087121](https://doi.org/10.5281/zenodo.8087121) |
| **JSS DOI** | [doi:10.18637/jss.v093.c01](https://doi.org/10.18637/jss.v093.c01) |

Laboratoire de Mathématiques Jean Leray (UMR 6629)  
2, rue de la Houssinière  
BP 92208  
F-44322 Nantes Cedex 3  
France

## Package options

ArchaeoPhases uses the following
[`options()`](https://rdrr.io/r/base/options.html) to configure
behaviour:

- `ArchaeoPhases.grid`: a
  [`numeric`](https://rdrr.io/r/base/numeric.html) value specifying the
  number of equally spaced points at which densities are to be estimated
  (defaults to \\512\\). Should be a power of \\2\\.

- `ArchaeoPhases.precision`: an
  [`integer`](https://rdrr.io/r/base/integer.html) indicating the number
  of decimal places (defaults to \\0\\).

- `ArchaeoPhases.progress`: a
  [`logical`](https://rdrr.io/r/base/logical.html) scalar specifying if
  progress bars should be displayed (defaults to
  [`interactive()`](https://rdrr.io/r/base/interactive.html)).

## See also

Useful links:

- <https://ArchaeoStat.github.io/ArchaeoPhases/>

- <https://github.com/ArchaeoStat/ArchaeoPhases>

- Report bugs at <https://github.com/ArchaeoStat/ArchaeoPhases/issues>

## Author

**Maintainer**: Anne Philippe <anne.philippe@univ-nantes.fr>
([ORCID](https://orcid.org/0000-0002-5331-5087))

Authors:

- Anne Philippe <anne.philippe@univ-nantes.fr>
  ([ORCID](https://orcid.org/0000-0002-5331-5087))

- Marie-Anne Vibet ([ORCID](https://orcid.org/0000-0003-4003-3141))

- Nicolas Frerebeau <nicolas.frerebeau@u-bordeaux-montaigne.fr>
  ([ORCID](https://orcid.org/0000-0001-5759-4944))

Other contributors:

- Thomas S. Dye ([ORCID](https://orcid.org/0000-0001-8116-782X))
  \[contributor\]

- Nantes Université ([ROR](https://ror.org/03gnr7b55)) \[funder\]

- Université Bordeaux Montaigne ([ROR](https://ror.org/03pbgwk21))
  \[funder\]

- CNRS ([ROR](https://ror.org/02feahw73)) \[funder\]
