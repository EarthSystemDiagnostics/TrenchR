# TrenchR: Analyse Trench-like Proxy Records

------------------------------

## Introduction

A _trench_ denotes a spatial array of profiles that tabulate climate proxy data
over depth or time, with the profiles being sampled longitudinally at regular
or irregular intervals.  The R package **TrenchR** implements routines that are
targeted at the analysis and the plotting of the spatial (longitudinal) and
vertical (temporal) variations of the climate proxy data derived from such
trenches. The range of functions in **TrenchR** were the basis for the
publications of Münch et al. [2016](https://doi.org/10.5194/cp-12-1565-2016) and
Münch et al. [2017](https://doi.org/10.5194/tc-11-2175-2017), which analysed
Antarctic snow trench stable isotope data.

The R code in this package has been implemented by Dr. Thomas Münch. For further
information, code enhancements or potential bugs, please write to
<<thomas.muench@awi.de>> at the Alfred Wegener Institute, Helmholtz Centre for
Polar and Marine Research, Germany, or open an issue on the repository page.

This work was supported by Helmholtz funding through the Polar Regions and
Coasts in the Changing Earth System (PACES) programme of the Alfred Wegener
Institute and by the Initiative and Networking Fund of the Helmholtz Association
Grant VG-NH900.
 
## Installation

The latest version of **TrenchR** is currently available from the development
branch of its [GitHub
repository](https://github.com/EarthSystemDiagnostics/TrenchR), and is installed
via:

```r
# install.packages("remotes")
remotes::install_github("EarthSystemDiagnostics/TrenchR", ref = "dev")
```

## Vignettes

* The vignette `Introduction to TrenchR` gives an introduction to **TrenchR**,
  explaining the expected generic dataset structure and giving examples of
  applying specific **TrenchR** functions.
* The package vignette `Plot Münch et al. (2017) figures` reproduces the plots
  for the publication of Münch et
  al. ([2017](https://doi.org/10.5194/tc-11-2175-2017)).

## Package data

Several trench datasets are included in **TrenchR** and can be accessed from the
following variables after loading the package:

* `t13.trench1`: Antarctic snow trench T13-1 stable water isotopologue data and
  meta information.
* `t13.trench2`: Antarctic snow trench T13-2 stable water isotopologue data and
  meta information.
* `t15.trench1`: Antarctic snow trench T15-1 stable water isotopologue data and
  meta information.
* `t15.trench2`: Antarctic snow trench T15-2 stable water isotopologue data and
  meta information.

## Literature cited

Münch, T., Kipfstuhl, S., Freitag, J., Meyer, H., and Laepple, T.: Regional
climate signal vs. local noise: a two-dimensional view of water isotopes in
Antarctic firn at Kohnen Station, Dronning Maud Land, Clim. Past, 12(7),
1565-1581, doi:
[10.5194/cp-12-1565-2016](https://doi.org/10.5194/cp-12-1565-2016), 2016.

Münch, T., Kipfstuhl, S., Freitag, J., Meyer, H., and Laepple, T.: Constraints
on post-depositional isotope modifications in East Antarctic firn from analysing
temporal changes of isotope profiles, The Cryosphere, 11(5), 2175-2188, doi:
[10.5194/tc-11-2175-2017](https://doi.org/10.5194/tc-11-2175-2017), 2017.
