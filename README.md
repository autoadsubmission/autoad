[![Launch Rstudio Binder](http://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/autoadsubmission/autoad/HEAD?urlpath=rstudio)

# Meta-Learning for Fast Time Series Anomaly Detection Model Recommendation

This document contains instructions on installing the `autoad` library and reproducing
the results in the paper "Meta-Learning for Fast Model Recommendation in Unsupervised
Multivariate Time Series Anomaly Detection". We supply intermediary results so
that lengthy computations are not needed (but we supply too the scripts to generate
these intermediary files in case regenerating them is needed). We don't supply
the datasets we used, as we consider the start of our reproducibility chain
the precomputed performance and meta-feature matrices we supply in the library.
Nevertheless, links to the data and algorithms used are provided below.

## Steps to reproduce the paper's results

The environment is managed through a Binder instance that opens automatically
an Rstudio server with all the packages available. Click on the badge on top
to launch it.

### Restore the environment and install the `autoad` library

1. Create the environment by doing `renv::restore()`.
2. Install the `autoad` package by doing `devtools::install()`.

### Get paper outputs

There are five main results in the paper and a computation time calculation, we detail how
to reproduce them in detail below. The code for all the experiments and analyses run
in the paper is found in the `paper_scripts/` folder. 

#### Figure 1: Dataset UMAP projection

1. Open and run `paper_scripts/figures/datasets.R`.
2. The figure is found on `paper_scripts/figures/datasets.pdf`.

#### Figure 2: Table of meta-feature sets comparison

1. Open and run `paper_scripts/figures/metafeatures.R`.
2. The table is output in the terminal.

#### Figure 3: Recommender comparison

1. Open and run `paper_scripts/figures/recommender_comparison.R`.
2. The figure is found on `paper_scripts/figures/recommender_comparison.pdf`.

#### Figure 4: Ablation study (URegression vector analysis)

1. Open and run `paper_scripts/figures/uregression_vectors.R`.
2. The figure is found on `paper_scripts/figures/uregression_comparison.pdf`.

#### Figure 5: Ablation study (Hydra vs individual building blocks)

1. Open and run `paper_scripts/figures/ablation.R`.
2. The figures are found on `paper_scripts/figures/ablation_scenario1.pdf` and `paper_scripts/figures/ablation_scenario2.pdf`.


## Regenerating the experimental results

If the experimental results were to be recomputed, both files in `paper_scripts/experiments/`
should be rerun. Fair warning: these take a lot of time. The code runs in parallel for
most recommenders, but it will still take several hours, at least, depending on your machine.
Once they've finished, the files should appear in the `paper_scripts/cache/` and
`paper_scripts/cache_uregression/` folders and the instructions above can be
immediately followed.


## Usage of the `autoad` library

Note: the only information needed to reproduce the paper's results is the one
displayed above. We include this additional information in case a deeper understanding
of the code / library is needed.

### Fitting a recommender

This package follows a `recommender %>% fit %>% predict` convention. A matrix with dataset's meta-features and performance of applied algorithms is needed for the fitting.
You can obtain both calling `get_data()`:

```
library(autoad)
dat <- get_data(mfs_metaod = FALSE, mfs_catch22 = TRUE)
performance <- dat$performance
metafeatures <- dat$metafeatures
rec <- recommender('cfact') %>% fit(performance, metafeatures)
```

The following recommenders are available: 'global_best', 'hydra', 'metaod', 'nearest_neighbor', 'regression', 'u_regression'.

### Recommending a configuration

Once you have a trained recommender, you can issue algorithmic predictions for new data by feeding it their metafeatures. The `get_metafeatures()` function creates the appropriate input matrix:

```
new_data <- readr::read_csv('https://archive.ics.uci.edu/ml/machine-learning-databases/00374/energydata_complete.csv')
new_metafeatures <- get_metafeatures(new_data, include_metaod = FALSE, include_catch22 = TRUE, time_var = 'date')
rec %>% predict(new_metafeatures)
```

### Using MetaOD

[MetaOD](https://github.com/yzhao062/metaod) is a state of the art recommender that we ship as part of the library. In order to use it (as a recommender, `recommender('metaod')`, or when calculating metafeatures, `get_metafeatures_metaod()` or `get_metafeatures(include_metaod = TRUE)`) you need a Python installation and the following modules available:

- `numpy`.
- `scipy`.
- `pandas`.
- `scikit-learn`.
- `pyod`.

We recommend managing this through the `install_metaod_dependencies()` function.

## Pre-computed meta-features and performance matrix

The `get_data____()` and `get_precomputed_____()` function families work with internal data, previously computed by us. They use the following data sources:

- [BGP](https://github.com/cisco-ie/telemetry).
- [SMD](https://github.com/NetManAIOps/OmniAnomaly).
- [Benchmark](https://github.com/datamllab/tods/tree/benchmark).
- [Water](https://github.com/icsdataset/hai).

The performance matrix was computed using the following algorithms and hyper-parameter configurations:

- [Half Space Trees](https://github.com/yli96/HSTree):
  - Trees = {5, 10, 50, 100, 200}.
  - Tree depth = {5, 10 ,20}.
  - Window size = {1%, 5%, 10%, 20%}.
- [Isolation Forest](https://cran.r-project.org/web/packages/isotree/index.html):
  - % data per tree = {1%, 5%, 10%, 20%}.
  - Tree depth = {5, 10 , 20}.
  - Columns per split = {5, 10, 50, 100, 200}.
- [LODA](https://pyod.readthedocs.io/en/latest/_modules/pyod/models/loda.html):
  - Bins = {5, 10, 50, 100, 200}.
  - Random cuts = {5, 20, 50, 100, 200}.
- [Local Outlier Factor](https://scikit-learn.org/stable/modules/generated/sklearn.neighbors.LocalOutlierFactor.html):
  - Minkowksi exponent = {1, 2}.
  - Leaf size = {5, 10, 20}.
  - Neighbors = {2, 5, 10, 50, 100, 500}.
- [Random Histogram Forest](https://github.com/anrputina/rhf):
  - Trees = {5, 10, 50, 100, 200}.
  - Tree height = {5, 10, 20}.
  - Check duplicates = {yes, no}.
- [xStream](https://cmuxstream.github.io/):
  - Projection size = {5, 20, 50, 100, 200}.
  - Chains = {5, 10, 50, 100, 200}.
  - Depth = {5, 10, 20}.
  - Window size = {1%, 5%, 10%, 20%}.

The meta-feature sets available are MetaOD and Catch22 (summarized as the minimum, 1st quartile, median, 3rd quartile and maximum).


