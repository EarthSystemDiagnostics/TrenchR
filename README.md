# TrenchR: Analyse Trench-like Proxy Records.

------------------------------

## Introduction

**TrenchR** implements routines that are targeted at the analysis, together with
accompanying plotting, of spatial and temporal variations of proxy data
derived from spatial arrays of records, specifically from trenches, i.e.,
proxy depth profiles sampled longitudinally at regular intervals. It
incorporates the analyses of snow trench stable isotope data for the
publication of Münch et al. (2017).

The R code in this package has been implemented by Dr. Thomas Münch. For further
information, code enhancements or potential bugs, please write to
<<thomas.muench@awi.de>> at the Alfred Wegener Institute, Helmholtz Centre for
Polar and Marine Research, Germany, or open an issue on the repository page.

This work was supported by Helmholtz funding through the Polar Regions and
Coasts in the Changing Earth System (PACES) programme of the Alfred Wegener
Institute and by the Initiative and Networking Fund of the Helmholtz Association
Grant VG-NH900.
 
## Installation

**TrenchR** can be installed directly from GitHub:

```r
if (!require("devtools")) {
  install.packages("devtools")
}

devtools::install_github("EarthSystemDiagnostics/TrenchR")
```


## Literature cited

Münch, T., Kipfstuhl, S., Freitag, J., Meyer, H., and Laepple, T.: Constraints
on post-depositional isotope modifications in East Antarctic firn from analysing
temporal changes of isotope profiles, The Cryosphere, 11(5), 2175-2188, doi:
[10.5194/tc-11-2175-2017](https://doi.org/10.5194/tc-11-2175-2017), 2017.
