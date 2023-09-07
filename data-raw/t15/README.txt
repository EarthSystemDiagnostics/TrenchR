This folder contains:
---------------------

Stable Water Isotopologue Data of Trenches T15-1 and T15-2 from Kohnen Station,
Dronning Maud Land, Antarctica from Field Season KS1415 (ANT-Land-2014 campaign
of COFI project).

PI:   Thomas Münch (tmuench@awi.de)
Co-PI: Sepp Kipfstuhl (sepp.kipfstuhl@awi.de), Thomas Laepple (tlaepple@awi.de)

Rough data structure:
* two trenches separated by approx. 550 m (from starting points)
* each trench contains 11 profiles to 3.4 m depth
* horizontal profile spacing: 5 m
* trench T15-1 contains one additional profile ("DUNE1") at horiz. pos. 26.5 m
  (was sampled since it lies on a prominent dune feature)
* samples are given with respect to an absolute height reference given by
  the max. surface height at each trench (see meta files)
* depth range from 0 - 2.4 m was sampled in bags directly in the field
* depth range from 2.4 - 3.4 m was sampled with CFK liners
* depth resolution: 0 - 2.4 m at 3 cm, 2.4 - 3.4 m at ~2.22 cm resolution

Location:
T15-1-start: LAT -75.00719 LON 0.0796
T15-1-end:   LAT -75.00760 LON 0.0805
T15-2-start: LAT -75.00944 LON 0.0967
T15-2-end:   LAT -75.00986 LON 0.0972

Date of Sampling:
T15-1/bags:  2015/01/24 - 2015/01/26
T15-1/liner: 2015/01/26
T15-2/bags:  2015/01/26 - 2015/01/28
T15-2/liner: 2015/01/28

Date of Sample Processing/Measurements:
T15/bags:  measured between April and September 2015 at AWI Potsdam
T15/liner: cut and put in bags from 2015/11/04 - 2015/11/06 at AWI Bremerhaven
T15/liner: corresponding bag samples measured between November 2015 and January
           2016 at AWI Potsdam

Measurements:
Isotope Laboratory, AWI Potsdam, Picarro inc. L2130i
Uncertainty: d18O: 0.08 permil; d2H: 0.8 permil; V-SMOW/SLAP

File Structure:
- "*_isotopes_T15-*.txt":  isotope data of individual profiles
- "meta_trench1/2.txt":    profile names, horizontal profile position in m,
  			   surface height in cm (positive downwards)

Notes:
d18O = oxygen isotopes (18O/16O)
d2H  = hydrogen isotopes (2H/1H)
dxs  = d-excess = d2H - 8*d18O

