
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dabestr

<!-- badges: start -->

[![Travis CI build
status](https://img.shields.io/travis/com/ACCLAB/dabestr/master.svg)](https://travis-ci.com/ACCLAB/dabestr/)
[![minimal R
version](https://img.shields.io/badge/R%3E%3D-2.10-6666ff.svg)](https://cran.r-project.org/)  
[![CRAN Download
Count](https://cranlogs.r-pkg.org/badges/grand-total/dabestr?color=brightgreen)](https://cran.r-project.org/package=dabestr)
[![Free-to-view
citation](https://zenodo.org/badge/DOI/10.1038/s41592-019-0470-3.svg)](https://rdcu.be/bHhJ4)
[![License](https://img.shields.io/badge/License-Apache_2.0-orange.svg)](https://spdx.org/licenses/BSD-3-Clause-Clear.html)
<!-- badges: end -->

<!-- ## Overview -->

dabestr is a package for **D**ata **A**nalysis using
**B**ootstrap-Coupled **EST**imation.

[Estimation
statistics](https://en.wikipedia.org/wiki/Estimation_statistics "Estimation Stats on Wikipedia")
is a [simple
framework](https://thenewstatistics.com/itns/ "Introduction to the New Statistics")
that avoids the
[pitfalls](https://www.nature.com/articles/nmeth.3288 "The fickle P value generates irreproducible results, Halsey et al 2015")
of significance testing. It uses familiar statistical concepts: means,
mean differences, and error bars. More importantly, it focuses on the
effect size of one’s experiment/intervention, as opposed to a false
dichotomy engendered by *P* values.

An estimation plot has two key features.

1.  It **presents all datapoints** as a swarmplot, which orders each
    point to display the underlying distribution.

2.  It presents the **effect size** as a **bootstrap 95% confidence
    interval** on a **separate but aligned axes**.

## Installation

    git clone https://github.com/ACCLAB/dabestr

## Usage

``` r
devtools::load_all(".")
#> ℹ Loading dabestr
#> Warning: package 'testthat' was built under R version 4.1.2

## Loading in data
data(twogroup_data)

dabest_obj.mean_diff <- dabestr::load(data = twogroup_data, 
                                      x = Group, 
                                      y = Measurement, 
                                      idx = c("Control1", "Group1")) %>%
  dabestr::mean_diff()
#> DABESTR v0.0.0.9000
#> ===================
#> 
#> Good evening!
#> The current time is 23:39 PM on Monday August 14, 2023.
#> 
#> Effect size(s) with 95% confidence intervals will be computed for:
#> 1. Group1 minus Control1
#> 
#> resamples will be used to generate the effect size bootstraps.
#> 
#> DABESTR v0.0.0.9000
#> ===================
#> 
#> Good evening!
#> The current time is 23:39 PM on Monday August 14, 2023.
#> 
#> The unpaired mean difference between Group1 and Control1 is 19.734 [95%CI 8.099, 31.045].
#> The p-value of the two-sided permutation t-test is , calculated for legacy purposes only.
#> 
#> bootstrap samples were taken; the confidence interval is bias-corrected and accelerated.
#> Any p-value reported is the probability of observing the effect size (or greater),
#> assuming the null hypothesis of zero difference is true.
#> For each p-value, reshuffles of the control and test labels were performed.
#> 
#> To get the results of all valid statistical tests, use .mean_diff.statistical_tests

dabest_plot(dabest_obj.mean_diff, TRUE)
```

![](man/figures/README-unnamed-chunk-2-1.png)<!-- -->

## Citation

**Moving beyond P values: Everyday data analysis with estimation plots**

*Joses Ho, Tayfun Tumkaya, Sameer Aryal, Hyungwon Choi, Adam
Claridge-Chang*

Nature Methods 2019, 1548-7105.
[10.1038/s41592-019-0470-3](http://dx.doi.org/10.1038/s41592-019-0470-3)

[Paywalled publisher
site](https://www.nature.com/articles/s41592-019-0470-3); [Free-to-view
PDF](https://rdcu.be/bHhJ4)

## Contributing

All contributions are welcome; please read the [Guidelines for
contributing](https://github.com/ACCLAB/dabestr/blob/master/CONTRIBUTING.md)
first.

We also have a [Code of
Conduct](https://github.com/ACCLAB/dabestr/blob/master/CODE_OF_CONDUCT.md)
to foster an inclusive and productive space.

### A wish list for new features

Currently, DABEST offers functions to handle data traditionally analyzed
with Student’s paired and unpaired t-tests. It also offers plots for
multiplexed versions of these, and the estimation counterpart to a 1-way
analysis of variance (ANOVA), the shared-control design. While these
five functions execute a large fraction of common biomedical data
analyses, there remain three others: 2-way data, time-series group data,
and proportional data. We aim to add these new functions to both the R
and Python libraries.

● In many experiments, four groups are investigate to isolate an
interaction, for example: a genotype × drug effect. Here, wild-type and
mutant animals are each subjected to drug or sham treatments; the data
are traditionally analysed with a 2×2 ANOVA. We have received requests
by email, Twitter, and GitHub to implement an estimation counterpart to
the 2-way ANOVA. To do this, we will implement ∆∆ plots, in which the
difference of means (∆) of two groups is subtracted from a second
two-group ∆.

● Currently, DABEST can analyse multiple paired data in a single plot,
and multiple groups with a common, shared control. However, a common
design in biomedical science is to follow the same group of subjects
over multiple, successive time points. An estimation plot for this would
combine elements of the two other designs, and could be used in place of
a repeated-measures ANOVA.

● We have observed that proportional data are often analyzed in
neuroscience and other areas of biomedical research. However, compared
to other data types, the charts are frequently impoverished: often, they
omit error bars, sample sizes, and even P values—let alone effect sizes.
We would like DABEST to feature proportion charts, with error bars and a
curve for the distribution of the proportional differences.

We encourage contributions for the above features.

## Dabestr in other languages

dabestr is also available in
[Python](https://github.com/ACCLAB/DABEST-python "DABEST-Python on Github")
and
[Matlab](https://github.com/ACCLAB/DABEST-Matlab "DABEST-Matlab on Github").
