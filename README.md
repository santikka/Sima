
# Simulator Framework

A simulation framework for large-scale data generation in the health
care domain. The work is a part of the DEMO project
<http://www.jyu.fi/demo>.

## Installation

Install the released version of the framework from CRAN:

``` r
install.packages("Sima")
```

Alternatively, install the development version from GitHub:

``` r
# install.packages("devtools")
devtools::install_github("santikk/Sima")
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
