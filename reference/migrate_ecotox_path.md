# Temporary function to switch to new ECOTOX path

Since ECOTOXr v1.2.5, the path is located with the base package `tools`
instead of the previously used `rappdirs`. Use this function to migrate
your old files to this new location.

## Usage

``` r
migrate_ecotox_path(overwrite = FALSE)
```

## Arguments

- overwrite:

  `logical` value. Should files that already exist in the new location
  be overwritten?

## Value

Returns `NULL` invisibly.
