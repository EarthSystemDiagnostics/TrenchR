# TrenchR: Analyse Trench-like Proxy Records.

------------------------------

## Introduction

**TrenchR** implements routines that are targeted at the analysis, together with
accompanying plotting, of spatial and temporal variations of proxy data
derived from spatial arrays of records, specifically from trenches, i.e.,
proxy depth profiles sampled longitudinally at regular intervals. This part is
still under active development, but the package incorporates the analyses of
Antarctic snow trench stable isotope data performed for the publication of Münch
et al. (2017).

The R code in this package has been implemented by Dr. Thomas Münch. For further
information, code enhancements or potential bugs, please write to
<<thomas.muench@awi.de>> at the Alfred Wegener Institute, Helmholtz Centre for
Polar and Marine Research, Germany, or open an issue on the repository page.

This work was supported by Helmholtz funding through the Polar Regions and
Coasts in the Changing Earth System (PACES) programme of the Alfred Wegener
Institute and by the Initiative and Networking Fund of the Helmholtz Association
Grant VG-NH900.
 
## Installation

**TrenchR** is currently available from its [GitHub
repository](https://github.com/EarthSystemDiagnostics/TrenchR) and is installed
via:

```r
# install.packages("remotes")
remotes::install_github("EarthSystemDiagnostics/TrenchR", build_vignettes = TRUE)
```

## Examples

* The package vignette `Plot Münch et al. (2017) figures` shows how to
  reproduce the plots for the publication Münch et al. (2017) and is available after
  installing the package as shown above. The respective vignette of the package
  development version is linked
  [here](http://htmlpreview.github.io/?https://github.com/EarthSystemDiagnostics/TrenchR/blob/master/vignettes/tc17-figures.html).

## Package data

Several datasets are included in **TrenchR** and can be accessed from the
following variables after loading the package:

* `t13.trench1`: Antarctic snow trench T13-1 stable water isotopologue data and
  meta information.
* `t13.trench2`: Antarctic snow trench T13-2 stable water isotopologue data and
  meta information.
* `t15.trench1`: Antarctic snow trench T15-1 stable water isotopologue data and
  meta information.
* `t15.trench2`: Antarctic snow trench T15-2 stable water isotopologue data and
  meta information.
* `aws9`: 2 m air temperature from the automatic weather station AWS9 at Kohnen
  Station, Dronning Maud Land, Antarctica, at monthly and annual resolution for
  the period December 1998 (start of AWS9 operation) until April 2014.
* `ParamSpace`: Data to reproduce Figure 5 in Münch et al. (2017).

## Literature cited

Münch, T., Kipfstuhl, S., Freitag, J., Meyer, H., and Laepple, T.: Constraints
on post-depositional isotope modifications in East Antarctic firn from analysing
temporal changes of isotope profiles, The Cryosphere, 11(5), 2175-2188, doi:
[10.5194/tc-11-2175-2017](https://doi.org/10.5194/tc-11-2175-2017), 2017.
