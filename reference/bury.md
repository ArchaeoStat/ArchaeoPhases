# Age-Depth Modeling

Computes the age-depth curve from the output of the MCMC algorithm and
the known depth of each dated samples.

## Usage

``` r
bury(object, depth, ...)

# S4 method for class 'EventsMCMC,numeric'
bury(object, depth, span = 0.75, degree = 2)

# S4 method for class 'AgeDepthModel'
predict(object, newdata)

# S4 method for class 'AgeDepthModel,missing'
plot(
  x,
  level = 0.95,
  calendar = get_calendar(),
  main = NULL,
  sub = NULL,
  ann = graphics::par("ann"),
  axes = TRUE,
  frame.plot = axes,
  panel.first = NULL,
  panel.last = NULL,
  ...
)
```

## Arguments

- object:

  An
  [`EventsMCMC`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/EventsMCMC-class.md)
  object.

- depth:

  A [`numeric`](https://rdrr.io/r/base/numeric.html) vector giving of
  the depths of the dated samples.

- ...:

  Other [graphical parameters](https://rdrr.io/r/graphics/par.html) may
  also be passed as arguments to this function, particularly, `border`,
  `col`, `lwd`, `lty` or `pch`.

- span:

  A length-one [`numeric`](https://rdrr.io/r/base/numeric.html) vector
  giving the regression parameter (see
  [`stats::loess()`](https://rdrr.io/r/stats/loess.html)).

- degree:

  A length-one [`numeric`](https://rdrr.io/r/base/numeric.html) vector
  giving the degree of the polynomials to be used (see
  [`stats::loess()`](https://rdrr.io/r/stats/loess.html)).

- newdata:

  A [`numeric`](https://rdrr.io/r/base/numeric.html) vector giving the
  depths at which ages will be predicted. If `missing`, the original
  data points are used.

- x:

  An
  [`AgeDepthModel`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/AgeDepthModel-class.md)
  object.

- level:

  A length-one [`numeric`](https://rdrr.io/r/base/numeric.html) vector
  giving the confidence level.

- calendar:

  A
  [`aion::TimeScale`](https://packages.tesselle.org/aion//reference/TimeScale-class.html)
  object specifying the target calendar (see
  [`calendar()`](https://packages.tesselle.org/aion//reference/calendar.html)).

- main:

  A [`character`](https://rdrr.io/r/base/character.html) string giving a
  main title for the plot.

- sub:

  A [`character`](https://rdrr.io/r/base/character.html) string giving a
  subtitle for the plot.

- ann:

  A [`logical`](https://rdrr.io/r/base/logical.html) scalar: should the
  default annotation (title and x and y axis labels) appear on the plot?

- axes:

  A [`logical`](https://rdrr.io/r/base/logical.html) scalar: should axes
  be drawn on the plot?

- frame.plot:

  A [`logical`](https://rdrr.io/r/base/logical.html) scalar: should a
  box be drawn around the plot?

- panel.first:

  An an `expression` to be evaluated after the plot axes are set up but
  before any plotting takes place. This can be useful for drawing
  background grids.

- panel.last:

  An `expression` to be evaluated after plotting has taken place but
  before the axes, title and box are added.

## Value

- `bury()` returns an
  [`AgeDepthModel`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/AgeDepthModel-class.md)
  object.

- [`predict()`](https://rdrr.io/r/stats/predict.html) returns an
  [`EventsMCMC`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/EventsMCMC-class.md)
  object.

- [`plot()`](https://packages.tesselle.org/aion//reference/plot.html) is
  called it for its side-effects: it results in a graphic being
  displayed (invisibly returns `x`).

## Details

We assume it exists a function \\f\\ relating the age and the depth
\\age = f(depth)\\. We estimate the function using local regression
(also called local polynomial regression): \\f = loess(age ~ depth)\\.
This estimated function \\f\\ depends on the unknown dates. However,
from the posterior distribution of the age/date sequence, we can
evaluate the posterior distribution of the age function for each desired
depth.

## References

Jha, D. K., Sanyal, P. & Philippe, A. (2020). Multi-Proxy Evidence of
Late Quaternary Climate and Vegetational History of North-Central India:
Implication for the Paleolithic to Neolithic Phases. *Quaternary Science
Reviews*, 229: 106121.
[doi:10.1016/j.quascirev.2019.106121](https://doi.org/10.1016/j.quascirev.2019.106121)
.

Ghosh, S., Sanyal, P., Roy, S., Bhushan, R., Sati, S. P., Philippe, A. &
Juyal, N. (2020). Early Holocene Indian Summer Monsoon and Its Impact on
Vegetation in the Central Himalaya: Insight from dD and d13C Values of
Leaf Wax Lipid. *The Holocene*, 30(7): 1063-1074.
[doi:10.1177/0959683620908639](https://doi.org/10.1177/0959683620908639)
.

## See also

Other age-depth modeling tools:
[`interpolate()`](https://ArchaeoStat.github.io/ArchaeoPhases/reference/interpolate.md)

## Author

A. Philippe

## Examples

``` r
## Coerce to MCMC
eve <- matrix(rnorm(6000, (1:6)^2), ncol = 6, byrow = TRUE)
eve <- as_events(eve, calendar = CE())

## Compute an age-depth curve
age <- bury(eve, depth = 1:6)
plot(age)


## Predict new values
new <- predict(age, newdata = 1.5:5.5)
summary(new)
#>   mad mean sd min q1 median q3 max start end
#> 1   2    2  2   0  2      2  3   5     1   4
#> 2   6    6  2   3  6      6  7   9     5   8
#> 3  12   12  2  10 12     12 13  15    11  14
#> 4  20   20  2  18 20     20 21  23    19  22
#> 5  30   30  2  27 30     30 31  33    29  32

plot(eve)

plot(new)
```
