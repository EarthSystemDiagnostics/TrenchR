# TrenchR: Analyse and Plot Trench Isotope Records.

------------------------------

## Introduction

**TrenchR** implements all main analyses and the plotting of results of the publication Münch et al. (2017). Please note that currently the package implements all plotting but only the major analyses performed in the paper and only minimum desriptions of the data and functionalities are available. Corresponding updates will be made in due course. Please contact Thomas Münch <<thomas.muench@awi.de>> at the Alfred-Wegener-Institute, Helmholtz Centre for Polar and Marine Research, Germany, for more information.

 
## Installation

**TrenchR** can be installed directly from bitbucket:

```r
if (!require("devtools")) {
  install.packages("devtools")
}

devtools::install_bitbucket("ecus/trenchr")
```

After installation, load the package by running

```r
library("TrenchR")
```

to get full functionality.


## Dependencies

The following packages are needed for **TrenchR** to function properly:

* `geosphere`
* `GISTools`
* `Hmisc`
* `RColorBrewer`

These packages are available on **CRAN** and are, if not yet present, automatically installed on your system when installing **TrenchR**.

## Literature cited

Münch, T., Kipfstuhl, S., Freitag, J., Meyer, H., and Laepple, T.: Constraints on post-depositional isotope modifications in East Antarctic firn from analysing temporal changes of isotope profiles, The Cryosphere Discuss., 2017, 1–21, doi:10.5194/tc-2017-35, 2017.