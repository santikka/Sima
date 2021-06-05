
# Simulation Framework

A simulation framework for large-scale data generation with applications
in the health care domain. The work is a part of the DEMO project
<http://www.jyu.fi/demo>.

## Installation

Install the released version of the framework from CRAN (not released
yet):

``` r
install.packages("Sima")
```

Alternatively, install the development version from GitHub:

``` r
# install.packages("devtools")
devtools::install_github("santikka/Sima")
```

## Usage

A simulator consists of events and an initial population. Events come in
two types of R6 class objects, known as manipulation events and
accumulation events. Manipulation events modify the status of initial
population whereas accumulation events introduce new individuals into
the population. The population itself is contained in a `data.table` and
can be either entirely synthetic or based on real data. To construct a
simulator, first define the events governing the system of interest and
the initial population.

Read `vignette("Sima")` for more details on how to construct a simulator
for your purposes.

## Recent changes

### Changes from version 0.5.1 to 0.5.2

-   The package no longer depends on ‘doMPI’, ‘foreach’ and ‘iterators’
    packages. Now suggests instead.
-   Added explicit support for interventions via the method ‘intervene’.
-   All event parameters are now named.
-   Now supports derived parameters for events, that change their values
    automatically if the parameter values change.
-   Events can now have their own persistent data.
-   Removed `Time` class as unnecessary.
