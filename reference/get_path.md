# The local path to the ECOTOX database (directory or sqlite file)

**\[stable\]** Obtain the local path to where the ECOTOX database is (or
will be) placed.

## Usage

``` r
get_ecotox_sqlite_file(path = get_ecotox_path(), version)

get_ecotox_path()
```

## Arguments

- path:

  When you have a copy of the database somewhere other than the default
  directory (`get_ecotox_path()`), you can provide the path here.

- version:

  A `character` string referring to the release version of the database
  you wish to locate. It should have the same format as the date in the
  EPA download link, which is month, day, year, separated by underscores
  ("%m\_%d\_%Y"). When missing, the most recent available copy is
  selected automatically.

## Value

Returns a `character` string of the path. `get_ecotox_path` will return
the default directory of the database. `get_ecotox_sqlite_file` will
return the path to the sqlite file when it exists.

## Details

It can be useful to know where the database is located on your disk.
This function returns the location as provided by
[`tools::R_user_dir()`](https://rdrr.io/r/tools/userdir.html), or as
specified by you using `options(ECOTOXr_path = "mypath")`.

## See also

Other database-access-functions:
[`check_ecotox_availability()`](https://pepijn-devries.github.io/ECOTOXr/reference/check_ecotox_availability.md),
[`check_ecotox_build()`](https://pepijn-devries.github.io/ECOTOXr/reference/check_ecotox_build.md),
[`check_ecotox_version()`](https://pepijn-devries.github.io/ECOTOXr/reference/check_ecotox_version.md),
[`cite_ecotox()`](https://pepijn-devries.github.io/ECOTOXr/reference/cite_ecotox.md),
[`dbConnectEcotox()`](https://pepijn-devries.github.io/ECOTOXr/reference/dbConnectEcotox.md),
[`get_ecotox_info()`](https://pepijn-devries.github.io/ECOTOXr/reference/get_ecotox_info.md),
[`list_ecotox_fields()`](https://pepijn-devries.github.io/ECOTOXr/reference/list_ecotox_fields.md),
[`migrate_ecotox_path()`](https://pepijn-devries.github.io/ECOTOXr/reference/migrate_ecotox_path.md),
[`search_ecotox()`](https://pepijn-devries.github.io/ECOTOXr/reference/search_ecotox.md)

## Author

Pepijn de Vries

## Examples

``` r
get_ecotox_path()
#> [1] "/home/runner/.local/share/R/ECOTOXr"

if (check_ecotox_availability()) {
  ## This will only work if a local database exists:
  get_ecotox_sqlite_file()
}
```
