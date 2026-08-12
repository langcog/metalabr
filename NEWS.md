# metalabr 1.0.0

MetaLab data now lives in versioned, citable releases on Redivis
(<https://stanford.redivis.com/datasets/81tq-8dp5ge6b9>), and metalabr reads them by
default. Every data-access call announces which release it used.

## Data access

* `get_metalab_data()` now reads released data from Redivis by default, with
  a `version` argument (`"current"` by default, or a release name like
  `"2023.1"` to pin). The legacy live-Google-Sheets path is still available
  by passing dataset metadata (`get_metalab_data(metadata)`); it is intended
  for curators building releases.
* `get_metalab_metadata()` likewise reads the released dataset registry by
  default (with `sheet_status`/`data_as_of` provenance columns); passing a
  `datasets.yaml` path/URL retains the legacy behavior.
* `get_current_metalab_data()` now downloads the current release bundle
  served by the MetaLab site instead of a snapshot committed to GitHub in
  2023, and reports the release it loaded. It still loads `metalab_data` and
  `dataset_info` into the global environment for backward compatibility.
* New `get_metalab_versions()` lists releases and their Redivis versions.
* All network access fails gracefully with a message (returning `NULL`)
  rather than an error.

## Visualizations

* `metalab_scatter_plot()` was unusable outside the retired Shiny app (it
  referenced reactive objects); it now has a working data-in/ggplot-out
  interface consistent with the other plot functions.
* `metalab_violin_plot()` supports every effect-size type (previously it
  errored for anything but Hedges' g) and handles multiple moderators.
* `metalab_forest_plot()` fits its model once instead of four times, and its
  moderator handling works outside the app.
* `metalab_funnel_plot()` works with moderators outside the app (it
  previously referenced free variables that only existed in the app
  environment).
* New `metalab_funnel_test()`: an Egger-style asymmetry test adapted to the
  multilevel model (√vᵢ as a fixed-effect moderator). The legacy app's
  `regtest()` call always errored (invisibly) on multilevel models.
* Plot colors no longer require the GitHub-only `langcog` package.

## Removed / deprecated

* `viz_app()` is deprecated: the interactive applications are superseded by
  the in-browser visualizations at <https://metalab.stanford.edu>. The
  internal Shiny app code (visualization, power analysis, validation) and
  the abandoned Google-Drive revision machinery were removed.

## Fixed

* The package now declares and imports its dependencies correctly —
  `library(metalabr)` works without attaching dplyr first
  (fixes #6).
* `dplyr::add_rownames()` (defunct) and other retired tidyverse idioms were
  replaced; the pipeline runs on current tidyverse releases.
* Invalid `License` field replaced with MIT + file LICENSE.

## Compatibility notes

* Effect-size computation is characterization-tested against the released
  database: the 1.0.0 pipeline reproduces the legacy pipeline's output
  exactly (all effect-size branches, the hardcoded special-case studies,
  seed-111 correlation imputation, and derived columns).
* In released data on Redivis, the Abstract rule learning moderator column
  `rule.type` is named `rule_type` (Redivis does not allow dots in column
  names). Data frames loaded via `get_current_metalab_data()` keep the
  legacy name.
