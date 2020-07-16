## load metalab dataset metadata
mds <- metalab::get_dataset_metadata(here("../metalab2/metadata/datasets.yaml"))

## load metalab field specification metadata
fields <- metalab::get_fields_metadata(here("../metalab2/metadata/spec.yaml"))

## extract Google Sheet keys
keys <- sapply(mds, function(x) x$key)

## fetch metalab data from Google Sheets
ds <- metalab::fetch_dataset(keys[1])

## validate dataset
metalab::validate_dataset(ds, fields)
