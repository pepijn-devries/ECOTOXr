# Get information on the local ECOTOX database when available

**\[stable\]** Get information on how and when the local ECOTOX database
was build.

## Usage

``` r
get_ecotox_info(path = get_ecotox_path(), version)
```

## Arguments

- path:

  A `character` string with the path to the location of the local
  database (default is
  [`get_ecotox_path()`](https://pepijn-devries.github.io/ECOTOXr/reference/get_path.md)).

- version:

  A `character` string referring to the release version of the database
  you wish to locate. It should have the same format as the date in the
  EPA download link, which is month, day, year, separated by underscores
  ("%m\_%d\_%Y"). When missing, the most recent available copy is
  selected automatically.

## Value

Returns a `vector` of `character`s, containing a information on the
selected local ECOTOX database.

## Details

Get information on how and when the local ECOTOX database was build.
This information is retrieved from the log-file that is (optionally)
stored with the local database when calling
[`download_ecotox_data()`](https://pepijn-devries.github.io/ECOTOXr/reference/download_ecotox_data.md)
or
[`build_ecotox_sqlite()`](https://pepijn-devries.github.io/ECOTOXr/reference/build_ecotox_sqlite.md).

## See also

Other database-access-functions:
[`check_ecotox_availability()`](https://pepijn-devries.github.io/ECOTOXr/reference/check_ecotox_availability.md),
[`check_ecotox_build()`](https://pepijn-devries.github.io/ECOTOXr/reference/check_ecotox_build.md),
[`check_ecotox_version()`](https://pepijn-devries.github.io/ECOTOXr/reference/check_ecotox_version.md),
[`cite_ecotox()`](https://pepijn-devries.github.io/ECOTOXr/reference/cite_ecotox.md),
[`dbConnectEcotox()`](https://pepijn-devries.github.io/ECOTOXr/reference/dbConnectEcotox.md),
[`get_ecotox_sqlite_file()`](https://pepijn-devries.github.io/ECOTOXr/reference/get_path.md),
[`list_ecotox_fields()`](https://pepijn-devries.github.io/ECOTOXr/reference/list_ecotox_fields.md),
[`migrate_ecotox_path()`](https://pepijn-devries.github.io/ECOTOXr/reference/migrate_ecotox_path.md),
[`search_ecotox()`](https://pepijn-devries.github.io/ECOTOXr/reference/search_ecotox.md)

## Author

Pepijn de Vries

## Examples

``` r
if (check_ecotox_availability()) {
  ## Show info on the current database (only works when one is downloaded and build):
  get_ecotox_info()
}
```
