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

## See also

Other database-access-functions:
[`check_ecotox_availability()`](https://pepijn-devries.github.io/ECOTOXr/reference/check_ecotox_availability.md),
[`check_ecotox_build()`](https://pepijn-devries.github.io/ECOTOXr/reference/check_ecotox_build.md),
[`check_ecotox_version()`](https://pepijn-devries.github.io/ECOTOXr/reference/check_ecotox_version.md),
[`cite_ecotox()`](https://pepijn-devries.github.io/ECOTOXr/reference/cite_ecotox.md),
[`dbConnectEcotox()`](https://pepijn-devries.github.io/ECOTOXr/reference/dbConnectEcotox.md),
[`get_ecotox_info()`](https://pepijn-devries.github.io/ECOTOXr/reference/get_ecotox_info.md),
[`get_ecotox_sqlite_file()`](https://pepijn-devries.github.io/ECOTOXr/reference/get_path.md),
[`list_ecotox_fields()`](https://pepijn-devries.github.io/ECOTOXr/reference/list_ecotox_fields.md),
[`search_ecotox()`](https://pepijn-devries.github.io/ECOTOXr/reference/search_ecotox.md)
