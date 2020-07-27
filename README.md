# MetaLab R Package

The `metalabr` package provides functions for reading, validating, and
cleaning datasets that support the [MetaLab](https://github.com/langcog/metalab2) site.

## Installation

Before you can use `metalabr`, you must install it. If you need to
install the [devtools](https://github.com/r-lib/devtools) package, first do that. After the devtools
package is available, you can install the latest version of metalabr
directly from Github with the following command:

```r 
devtools::install_github("metalab/metalabr")
```
## Usage

Once the `metalabr` package has been installed, you can can load it with
the following command:

```r
library(metalabr)
```

Now that the `metalabr` package has been loaded, you can read in the
current datasets incorporated into Metalab with the following
commands. First first line of code will read in the medadata
associated with each MetaLab dataset, including the URL of the Google
sheet that contains the raw data. At that point, you can optionally
filter `ml_dataset_info` to the values you want to read.

The second line of code will read the raw data from Google Sheets for
each of the datasets in `ml_dataset_info`.

```r
ml_dataset_info <- metalabr::get_metalab_dataset_info()
ml_data <- metalabr::get_metalab_data(ml_dataset_info)
```
