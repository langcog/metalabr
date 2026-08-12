# metalabr

<!-- badges: start -->
[![R-CMD-check](https://github.com/langcog/metalabr/actions/workflows/check.yaml/badge.svg)](https://github.com/langcog/metalabr/actions/workflows/check.yaml)
<!-- badges: end -->

An R package for accessing data from [MetaLab](https://metalab.stanford.edu),
a database of community-augmented meta-analyses of language acquisition and
cognitive development. MetaLab data are released as versioned, citable
snapshots on [Redivis](https://redivis.com/datasets/81tq-9ewzpdvz0); metalabr
reads those releases.

## Installation

```r
# install.packages("remotes")
remotes::install_github("langcog/metalabr")

# for versioned Redivis access (recommended):
install.packages("redivis",
  repos = c("https://langcog.r-universe.dev", getOption("repos")))
```

## Usage

The one-call path (no account needed) loads the current release:

```r
library(metalabr)
get_current_metalab_data()
#> Loaded MetaLab data release 2026.1 into the global environment
#> (objects: metalab_data, dataset_info).
```

For versioned, reproducible access (uses your free
[Redivis](https://redivis.com) account via the redivis package):

```r
metalab_data <- get_metalab_data()                  # current release
metalab_data <- get_metalab_data(version = "2023.1")  # pinned
mutex <- get_metalab_data(short_names = "mutex")
metadata <- get_metalab_metadata()
get_metalab_versions()
```

Standard MetaLab visualizations (multilevel random-effects models via
metafor):

```r
metalab_scatter_plot(metalab_data, "idspref", "g_calc", "g_var_calc")
metalab_violin_plot(metalab_data, "idspref")
metalab_forest_plot(metalab_data, "idspref", moderators = "response_mode")
metalab_funnel_plot(metalab_data, "idspref")
metalab_funnel_test(metalab_data, "idspref")
```

Curators can still fetch live coding spreadsheets (the path used to build
data releases):

```r
metadata <- get_metalab_metadata("path/to/datasets.yaml")
metalab_data <- get_metalab_data(metadata)
```

## Citation

Please cite the individual dataset(s) you use (see each dataset's page on
[metalab.stanford.edu](https://metalab.stanford.edu)) as well as:

Bergmann, C., Tsuji, S., Piccinini, P.E., Lewis, M.L., Braginsky, M., Frank,
M.C., & Cristia, A. (2018). Promoting replicability in developmental research
through meta-analyses: Insights from language acquisition research. *Child
Development, 89*, 1996-2009.
