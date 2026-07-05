# Check the locally build database for validity

**\[stable\]** Performs some simple tests to check whether the locally
built database is not corrupted.

## Usage

``` r
check_ecotox_build(path = get_ecotox_path(), version, ...)
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

- ...:

  Arguments that are passed to
  [`dbConnect()`](https://rsqlite.r-dbi.org/reference/SQLite.html)
  method or
  [`dbDisconnect()`](https://rsqlite.r-dbi.org/reference/SQLite.html)
  method.

## Value

Returns an indicative logical value whether the database is not
corrupted. `TRUE` indicates the database is most likely OK. `FALSE`
indicates that something might be wrong. Additional messages (when
`FALSE`) are included as attributes containing hints on the outcome of
the tests. See also the 'details' section.

## Details

For now this function tests if all expected tables are present in the
locally built database. Note that in later release of the database some
tables were added. Therefore for older builds this function might return
`FALSE` whereas it is actually just fine (just out-dated).

Furthermore, this function tests if all tables contain one or more
records. Obviously, this is no guarantee that the database is valid, but
it is a start.

More tests may be added in future releases.

## See also

Other database-access-functions:
[`check_ecotox_availability()`](https://pepijn-devries.github.io/ECOTOXr/reference/check_ecotox_availability.md),
[`check_ecotox_version()`](https://pepijn-devries.github.io/ECOTOXr/reference/check_ecotox_version.md),
[`cite_ecotox()`](https://pepijn-devries.github.io/ECOTOXr/reference/cite_ecotox.md),
[`dbConnectEcotox()`](https://pepijn-devries.github.io/ECOTOXr/reference/dbConnectEcotox.md),
[`get_ecotox_info()`](https://pepijn-devries.github.io/ECOTOXr/reference/get_ecotox_info.md),
[`get_ecotox_sqlite_file()`](https://pepijn-devries.github.io/ECOTOXr/reference/get_path.md),
[`list_ecotox_fields()`](https://pepijn-devries.github.io/ECOTOXr/reference/list_ecotox_fields.md)

Other database-build-functions:
[`build_ecotox_sqlite()`](https://pepijn-devries.github.io/ECOTOXr/reference/build_ecotox_sqlite.md),
[`check_ecotox_version()`](https://pepijn-devries.github.io/ECOTOXr/reference/check_ecotox_version.md),
[`download_ecotox_data()`](https://pepijn-devries.github.io/ECOTOXr/reference/download_ecotox_data.md),
[`get_ecotox_url()`](https://pepijn-devries.github.io/ECOTOXr/reference/get_ecotox_url.md)

## Author

Pepijn de Vries

## Examples

``` r
if (check_ecotox_availability()) {
  check_ecotox_build()
}
```
