# PNECR

The repository holds R code and data to estimate predicted no-effect
concentrations of antibiotics in terms of selection for resistance
(PNECR). The latter are meant to be used for the purpose of
environmental regulation.

Consider the [web interface](https::/enviresist.github.io/pnecr) to explore
the outcomes. For the standard user, this is way more convenient
compared to cloning the repository and re-analyzing the data locally.

In case of questions, please contact the maintainer of the repository:
David Kneis (firstname.lastname@tu-dresden.de).

# Major directories

## 1_databases

Holds data bases in delimited text format. This includes snapshots
of [MIC data provided by EUCAST](https://mic.eucast.org/search/),
a compilation of resistance cost published in scientific articles,
and Table 1 of the paper of
[Bengtsson-Palme & Larsson (2016)](https://doi.org/10.1016/j.envint.2015.10.015).

## 2_analysis_MIC

R code to find a reasonable estimate of the lowest MIC based on the
approach originally proposed by
[Bengtsson-Palme & Larsson (2016)](https://doi.org/10.1016/j.envint.2015.10.015).

## 3_analysis_cost

R code to estimate quantiles of the cost of plasmid-borne resistance.

## 4_webinterface

R code to derive PNECR values from the estimated
lowest MIC and a quantile of the resistance costs. The results are
compiled into a (pseudo)-dynamic web page. See the top of this README
file for an instantly accessible instance of that web page.
