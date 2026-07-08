# Check if the locally build database is up to date

**\[stable\]** Checks the version of the database available online from
the EPA against the specified version (latest by default) of the
database build locally. Returns `TRUE` when they are the same.

## Usage

``` r
check_ecotox_version(path = get_ecotox_path(), version, verbose = TRUE, ...)
```

## Arguments

- path:

  When you have a copy of the database somewhere other than the default
  directory
  ([`get_ecotox_path()`](https://pepijn-devries.github.io/ECOTOXr/reference/get_path.md)),
  you can provide the path here.

- version:

  A `character` string referring to the release version of the database
  you wish to locate. It should have the same format as the date in the
  EPA download link, which is month, day, year, separated by underscores
  ("%m\_%d\_%Y"). When missing, the most recent available copy is
  selected automatically.

- verbose:

  A `logical` value. If true messages are shown on the console reporting
  on the check.

- ...:

  Arguments passed to
  [`get_ecotox_url()`](https://pepijn-devries.github.io/ECOTOXr/reference/get_ecotox_url.md)

## Value

Returns a `logical` value invisibly indicating whether the locally build
is up to date with the latest release by the EPA.

## See also

Other database-access-functions:
[`check_ecotox_availability()`](https://pepijn-devries.github.io/ECOTOXr/reference/check_ecotox_availability.md),
[`check_ecotox_build()`](https://pepijn-devries.github.io/ECOTOXr/reference/check_ecotox_build.md),
[`cite_ecotox()`](https://pepijn-devries.github.io/ECOTOXr/reference/cite_ecotox.md),
[`dbConnectEcotox()`](https://pepijn-devries.github.io/ECOTOXr/reference/dbConnectEcotox.md),
[`get_ecotox_info()`](https://pepijn-devries.github.io/ECOTOXr/reference/get_ecotox_info.md),
[`get_ecotox_sqlite_file()`](https://pepijn-devries.github.io/ECOTOXr/reference/get_path.md),
[`list_ecotox_fields()`](https://pepijn-devries.github.io/ECOTOXr/reference/list_ecotox_fields.md),
[`migrate_ecotox_path()`](https://pepijn-devries.github.io/ECOTOXr/reference/migrate_ecotox_path.md)

Other database-build-functions:
[`build_ecotox_sqlite()`](https://pepijn-devries.github.io/ECOTOXr/reference/build_ecotox_sqlite.md),
[`check_ecotox_build()`](https://pepijn-devries.github.io/ECOTOXr/reference/check_ecotox_build.md),
[`download_ecotox_data()`](https://pepijn-devries.github.io/ECOTOXr/reference/download_ecotox_data.md),
[`get_ecotox_url()`](https://pepijn-devries.github.io/ECOTOXr/reference/get_ecotox_url.md)

## Author

Pepijn de Vries

## Examples

``` r
if (interactive() && check_ecotox_availability()) {
  check_ecotox_version()
}
```
