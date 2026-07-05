# Check whether a ECOTOX database exists locally

**\[stable\]** Tests whether a local copy of the US EPA ECOTOX database
exists in
[`get_ecotox_path()`](https://pepijn-devries.github.io/ECOTOXr/reference/get_path.md).

## Usage

``` r
check_ecotox_availability(target = get_ecotox_path())
```

## Arguments

- target:

  A `character` string specifying the path where to look for the
  database file.

## Value

Returns a `logical` value indicating whether a copy of the database
exists. It also returns a `files` attribute that lists which copies of
the database are found.

## Details

When arguments are omitted, this function will look in the default
directory
([`get_ecotox_path()`](https://pepijn-devries.github.io/ECOTOXr/reference/get_path.md)).
However, it is possible to build a database file elsewhere if necessary.

## See also

Other database-access-functions:
[`check_ecotox_build()`](https://pepijn-devries.github.io/ECOTOXr/reference/check_ecotox_build.md),
[`check_ecotox_version()`](https://pepijn-devries.github.io/ECOTOXr/reference/check_ecotox_version.md),
[`cite_ecotox()`](https://pepijn-devries.github.io/ECOTOXr/reference/cite_ecotox.md),
[`dbConnectEcotox()`](https://pepijn-devries.github.io/ECOTOXr/reference/dbConnectEcotox.md),
[`get_ecotox_info()`](https://pepijn-devries.github.io/ECOTOXr/reference/get_ecotox_info.md),
[`get_ecotox_sqlite_file()`](https://pepijn-devries.github.io/ECOTOXr/reference/get_path.md),
[`list_ecotox_fields()`](https://pepijn-devries.github.io/ECOTOXr/reference/list_ecotox_fields.md)

## Author

Pepijn de Vries

## Examples

``` r
check_ecotox_availability()
#> [1] FALSE
#> attr(,"files")
#> [1] path     database date    
#> <0 rows> (or 0-length row.names)
```
