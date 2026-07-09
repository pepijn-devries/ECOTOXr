# Open or close a connection to the local ECOTOX database

**\[stable\]** Wrappers for
[`dbConnect()`](https://rsqlite.r-dbi.org/reference/SQLite.html) and
[`dbDisconnect()`](https://rsqlite.r-dbi.org/reference/SQLite.html)
methods.

## Usage

``` r
dbConnectEcotox(path = get_ecotox_path(), version, ...)

dbDisconnectEcotox(conn, ...)
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

- conn:

  An open connection to the ECOTOX database that needs to be closed.

## Value

A database connection in the form of a
[`DBI::DBIConnection-class()`](https://dbi.r-dbi.org/reference/DBIConnection-class.html)
object. The object is tagged with: a time stamp; the package version
used; and the file path of the SQLite database used in the connection.
These tags are added as attributes to the object.

## Details

Open or close a connection to the local ECOTOX database. These functions
are only required when you want to send custom queries to the database.
For most searches the
[`search_ecotox()`](https://pepijn-devries.github.io/ECOTOXr/reference/search_ecotox.md)
function will be adequate.

## See also

Other database-access-functions:
[`check_ecotox_availability()`](https://pepijn-devries.github.io/ECOTOXr/reference/check_ecotox_availability.md),
[`check_ecotox_build()`](https://pepijn-devries.github.io/ECOTOXr/reference/check_ecotox_build.md),
[`check_ecotox_version()`](https://pepijn-devries.github.io/ECOTOXr/reference/check_ecotox_version.md),
[`cite_ecotox()`](https://pepijn-devries.github.io/ECOTOXr/reference/cite_ecotox.md),
[`get_ecotox_info()`](https://pepijn-devries.github.io/ECOTOXr/reference/get_ecotox_info.md),
[`get_ecotox_sqlite_file()`](https://pepijn-devries.github.io/ECOTOXr/reference/get_path.md),
[`list_ecotox_fields()`](https://pepijn-devries.github.io/ECOTOXr/reference/list_ecotox_fields.md),
[`migrate_ecotox_path()`](https://pepijn-devries.github.io/ECOTOXr/reference/migrate_ecotox_path.md),
[`search_ecotox()`](https://pepijn-devries.github.io/ECOTOXr/reference/search_ecotox.md)

## Author

Pepijn de Vries

## Examples

``` r
## This will only work when a copy of the database exists:
if (check_ecotox_availability()) {
  con <- dbConnectEcotox()

  ## check if the connection works by listing the tables in the database:
  dbListTables(con)

  ## Let's be a good boy/girl and close the connection to the database when we're done:
  dbDisconnectEcotox(con)
}
```
