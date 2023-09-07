This folder contains:
---------------------

Stable Water Isotopologue Data of Trenches T13-1 and T13-2 from Kohnen Station,
Dronning Maud Land, Antarctica from Field Season KS1213 (ANT-Land-2012 campaign
of COFI project).

PI:   Thomas Laepple (tlaepple@awi.de)
Co-PI: Thomas Münch (tmuench@awi.de), Sepp Kipfstuhl (sepp.kipfstuhl@awi.de),
           Johannes Freitag (jfreitag@awi.de)

Rough data structure:
* two 1.2 m deep and 45 m long trenches separated by approx. 415 m (from
  starting points)
* trench 1 contains 38 profiles taken at variable horizontal profile spacings
  between 0.1 and ~2.5 m (see meta file)
* trench 2 contains 4 profiles taken at 10 and 20 m profile spacing (see meta
  file) 
* samples are given with respect to an absolute height reference given by
  the max. surface height at each trench (see meta files)
* entire depth range was sampled in bags directly in the field

Location:
T13-1-start: LAT -75.00641 LON 0.07498
T13-1-end:   LAT -75.00673 LON 0.076024
T13-2-start: LAT -75.00849 LON 0.08693
T13-2-end:   LAT -75.00877 LON 0.08819

Date of Sampling:
T13-1:  2013/01/02 - 2013/01/03
T13-2:  2013/01/07

Date of Sample Measurements:
T13-1:  measured between November and December 2013 at AWI Bremerhaven and
             between April and August 2014 at AWI Potsdam
T13-2:  measured in September 2014 at AWI Bremerhaven

Measurements:
Isotope Laboratories, AWI Bremerhaven, AWI Potsdam; Picarro inc. L2120i and L2130i
Uncertainty: d18O: 0.09 permil; d2H: 0.8 permil; V-SMOW/SLAP

File Structure:
- "*_isotopes_T13-*.txt":  isotope data of individual profiles
- "meta_trench1/2.txt":    profile names, horizontal profile position in m,
  			                 surface height in cm (positive downwards)

Notes:
d18O = oxygen isotopes (18O/16O)
d2H  = hydrogen isotopes (2H/1H)
dxs  = d-excess = d2H - 8*d18O

