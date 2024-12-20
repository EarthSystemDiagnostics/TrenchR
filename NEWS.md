# Development of major version update 1.x.x

# TrenchR 1.0.0.9100

Adds new user function `summarizeBin()` to calculate summary values of a trench
mean profile partitioned into differently sized bins, e.g. an annual mean time
series from an depth-age relationship.

# TrenchR 1.0.0.9099

Adds a new vignette on statistical trench data analysis, which also reproduces
Münch et al. (2017) Table 2. With this new vignette, the statistical analyses in
the package introductory vignette are now better suited here, and accordingly
have been moved from there to the new vignette.

# TrenchR 1.0.0.9098

Adds new user function `getMeanProfileCorrelation()` to calculate the mean
profiles for two trench datasets and obtain the correlation between them,
optionally allowing for relative shifts between the profiles and/or for
interpolation onto a higher resolution.

# TrenchR 1.0.0.9097

Adds new user function `summarizeTrench()` to retrieve summary information on a
trench dataset.

# TrenchR 1.0.0.9096

Adds utility function `is.identical()` to check whether two vectors are (nearly)
equal.

# TrenchR 1.0.0.9095

Bug fix in `estimateSNR()`: required input is now correctly `lambda` instead of
`a1` parameter previously.

# TrenchR 1.0.0.9094

In decorrelation length estimation, the decorrelation length is no longer
returned automatically for both trench directions, but only for that direction
requested by the new function parameter `direction` (#15).

# TrenchR 1.0.0.9093

Bug fix in decorrelation length estimation when estimated `a1` coefficient is <=
0; in such a case, the returned decorrelation length is now set to `NA` with a
warning issued (#12).

# TrenchR 1.0.0.9092

This version update implements the estimation of trench decorrelation lengths in
horizontal and vertical direction via the new function
`estimateTrenchDecorrelation()`.

Further, the new function `removeSurfaceRegion()` is introduced, which is needed
in `estimateTrenchDecorrelation()` and removes a possibly incomplete surface
region of a trench dataset (due to the sampling on an absolute vertical scale).
While this function uses `getFirstCompleteDepthBin()` under the hood, the latter
function - although still exported - will probably no longer be needed much in
daily work, in favour of the new function.

Further updates:

- new unexported utility function `is.equidistant` for determining whether
  sampling positions are equidistant;
- for calculating the effective number of degrees of freedom with the function
`getEffectiveDOF`, it is now also possible to input the decorrelation length
instead of the autocorrelation at lag-1. The latter is still possible, but using
decorrelation length is preferred, since it automatically has the right units,
while the AR1 coefficient depends on the sampling resolution of the data it was
estimated on, so it might need rescaling, which the user would have to take care
of;
- based on the latter change, the function `estimateInterProfileCorrelation` now
  requires the decorrelation length as input, no longer the AR1 coefficient.

# TrenchR 1.0.0.9091

- Internal function `makeHiResKohnenTrenches()` now includes the option to
  return the full T15 trench dataset, instead of having the only default of
  returning the subset used for the Münch et al. (2017) paper analysis.

# TrenchR 1.0.0.9090

This version update provides changes that render **TrenchR** a more
self-contained and generic package and which yield a clearer separation between
the **TrenchR** and **FirnR** packages by removing data and functions from
**TrenchR** which are better suited for and (now) also available from **FirnR**.

#### Deletions of non-generic functions:

The functions `CompressRecord()`, `Compression()`, `DensificationRate()`,
`DifferentialDiffusion()`, `DiffuseRecord()`, `LoopParamSpace()`,
`ModifyRecord()`, and `RecordCompression()` were removed; their functionality is
now available from functions of the same or a similar name in **FirnR**, and
instances, where one of these functions is still needed within **TrenchR**, they
are replaced by the respective function from **FirnR**.

#### Update to package data:

- Automatic Weather Station 9 (AWS9) data (variable `aws9`) is no longer an
  exported dataset, but now internal package data. Source code to reproduce the
  dataset was added under `data-raw/`.
- the data to create/reproduce Fig. 5 of Münch et al. (2017) (variable
  `ParamSpace`) is no longer an exported dataset, but now internal package
  data. Addtionally, the data was re-created using **FirnR** (no actual change
  in data, of course) and the source code to reproduce the dataset was added
  under `data-raw/`.
- the internal firn density dataset from the Kohnen firn cores B41/B42 was
  deleted; it is now part of the **FirnR** package.
- the analysis parameters and the temporal change parameters used for the Münch
  et al. (2017) paper analyses, which previously could be obtained from the
  internal functions `loadKohnenTrenchPar()` and `SetModificationPar()`, are now
  stored as internal package data in the variables `tc17.paper.param` and
  `tc17.modif.param`, respectively. The same applies to the T13 annual mean
  data, which are now available from the internal data variable
  `t13.annual.means` instead of from the internal function
  `T13AnnualMeans()`. For all the three new data variables the source code to
  recreate the data was added under `data-raw/`.

#### New package vignette:

The new vignette `Introduction to TrenchR` was added, which gives an
introduction to **TrenchR**, explaining the expected generic dataset structure
and giving examples of applying specific **TrenchR** functions.

#### **FirnR** availability:

**FirnR** is, as of now (June 2024), still only available from a private
repository; because of this, it is no official package dependency in order to
not break **TrenchR**  installation. However, **FirnR** functions are only
needed for reproducing Münch et al. (2017) figures 6 and 7, for re-creating the
vignette `Plot Münch et al. (2017) figures`, and for re-creating some of the
internal datasets, not for any of the exported **TrenchR** functions. For every
of the **FirnR** usages the package availability is being checked. **FirnR**
is available on request from the **TrenchR** package authors.

#### Further minor changes:

- in `getZ()`: former function parameter `.var` is now called `vscale` to be
  consistent with respective instances in other functions.
- bug fixes: `estimateSNR()` and `plot2D()` now handle missing values.
- `makeHiResKohnenTrenches()` now returns the isotope data in the column with
  the neutral name "y" in order to facilitate the use of the data with **FirnR**
  functions.
- the internal functions of the name `TC17.Fig<xx>()` where `<xx>` stands for
  the figure number from 01 to 07, are now bundled together in one single
  (internal) function called `produceTC17Figures()`.
- The README.md file is from now on being built from a README.rmd (via
  `devtools::build_readme()`) to allow literate programming parts in it. The
  sections about the data structure and the examples are taken from the
  new **TrenchR** introduction vignette.

# TrenchR 1.0.0.9000

#### Update of T13 and T15 trench package datasets, accompanied by new data handling functions:

- the trench package data is now formatted in a modern way using a long `tibble`;
- the original trench data files were added under `data-raw/` along with
  processing code;
- new data handling functions were added that work nicely together with the
  updated T13/T15 trench dataset structure, but also with any dataset arranged
  in that way: `getSurfaceProfile()`, `getX()` `getZ()`, `make2D()`,
  `makeMean()`;
- internal trench data processing for paper analyses was updated accordingly.

#### New data analysis and plotting functions:

- `plot2D()`: plot a 2D trench image;
- `estimateInterProfileCorrelation)()`: estimate the pairwise correlation
  between trench profiles as a function of the distance between the profiles;
- `getEffectiveTrenchDOF()`: calculate the effective degrees of freedom for a
  set of trench profiles;
- `estimateSNR()`: estimate the signal-to-noise ratio (SNR) for a trench
  variable;
- `estimateTrenchVariance()`: calculate the average variance in horizontal and
  vertical direction of a trench dataset;
- `estimateInterTrenchCorrelation()`: calculate the pairwise correlations of all
  records in a trench with all records in a second trench;
- `getFirstCompleteDepthBin()`: extract the value of the bin in vertical
  dimension ("depth") for which a complete horizontal data set across all trench
  profile positions is available.

#### Further minor changes:

- internal trench data processing and plotting functions (Münch et al. (2017)
  paper analyses) updated; note especially the name change:
  `prepareTrenchData()` -> `makeHiResKohnenTrenches()`;
- update of package dependencies.

---

# Previous major version

# TrenchR 0.2.0

This update narrows down the scope of **TrenchR** by removing functions which
have a more general application than just for use with trench-like data. Since
this is applied to both internal as well as NAMESPACE functions, this update may
break code that calls any of these functions from **TrenchR**, which is, however,
alleviated by the fact that all these functions are available from dependent
packages installed along with this new version.

In detail, the following functions are no longer part of **TrenchR** but instead
available from the specified external packages:

* `Polyplot` is replaced by `grfxtools::Polyplot`;
* `rmsd` is replaced by `stattools::rmsd`;
* `SetPlotPar` is replaced by `grfxtools::Par`;
* `AverageIndexBins` is replaced by `prxytools::AverageByIndex`;
* `MinorTick` is replaced by `grfxtools::MinorTick`;
* `which.peaks` is replaced by `prxytools::LocatePeaks`;
* `my.legend` is replaced by `grfxtools::Legend`.

Note the function name change in some cases. Usage and version of the new
functions are identical to the previous ones, except for `grfxtools::Par` and
`grfxtools::MinorTick` which are enhanced versions of the respective **TrenchR**
versions.

In order to additionally optimise dependencies, the dependencies on the packages
**prettymapr** and **Hmisc** have been removed since only one function is used
from either package: `prettymapr::addnortharrow` and
`Hmisc::Lag`. `prettymapr::addnortharrow` is replaced by the slightly enhanced
version `grfxtools::AddNorthArrow` and `Hmisc::Lag` is replaced by the identical
`prxytools::Lag`. The former case also circumvents the problem of a [potential
future deprecation of **prettymapr**](https://github.com/paleolimbot/prettymapr)
and consequent removal from CRAN.

#### Further minor changes:

- update of license information;
- unexported functions are no longer documented;
- minor roxygen documentation updates;
- GitHub README update.

# TrenchR 0.1.3

* A package vignette showing how to reproduce the figures of the Münch et
  al. (2017) paper is now included.

# TrenchR 0.1.2

* Replaced GISTools dependency since the package is no longer on CRAN. This only
  affects the layout of the north arrow plotted on the map in Figure 2 of Münch
  et al. (2017).

# TrenchR 0.1.1

* Improved checking for reasonable advection values in `LoopParamSpace()`.

# TrenchR 0.1.0

* Full functionality and documentation.
* Generalised usage of some functions.

# TrenchR 0.0.0.9000

* Development version of package released along with the publication Münch, T.,
et al.: _Constraints on post-depositional isotope modifications in East
Antarctic firn from analysing temporal changes of isotope profiles_, The
Cryosphere, 11(5), 2175-2188, doi:
[10.5194/tc-11-2175-2017](https://doi.org/10.5194/tc-11-2175-2017), 2017.

