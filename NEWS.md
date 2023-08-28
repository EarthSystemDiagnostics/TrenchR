# TrenchR 0.2.0

This update narrows down the scope of **TrenchR** by removing functions which
have a more general application than just for use with trench-like data. Since
this is applied to both internal as well as NAMESPACE functions, this update may
break code that calls any of these functions from **TrenchR** which is, however,
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

# TrenchR 0.0.9

* Development version of package released along with the publication Münch, T.,
et al.: _Constraints on post-depositional isotope modifications in East
Antarctic firn from analysing temporal changes of isotope profiles_, The
Cryosphere, 11(5), 2175-2188, doi:
[10.5194/tc-11-2175-2017](https://doi.org/10.5194/tc-11-2175-2017), 2017.

