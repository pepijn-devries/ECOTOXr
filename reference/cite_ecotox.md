# Cite the downloaded copy of the ECOTOX database

**\[stable\]** Cite the downloaded copy of the ECOTOX database and this
package (`citation("ECOTOXr")`) for reproducible results.

## Usage

``` r
cite_ecotox(path = get_ecotox_path(), version)
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

Returns a `vector` of
[`bibentry()`](https://rdrr.io/r/utils/bibentry.html)'s, containing a
reference to the downloaded database and this package.

## Details

When you download a copy of the EPA ECOTOX database using
[`download_ecotox_data()`](https://pepijn-devries.github.io/ECOTOXr/reference/download_ecotox_data.md),
a BibTex file is stored that registers the database release version and
the access (= download) date. Use this function to obtain a citation to
that specific download.

In order for others to reproduce your results, it is key to cite the
data source as accurately as possible.

## See also

Other database-access-functions:
[`check_ecotox_availability()`](https://pepijn-devries.github.io/ECOTOXr/reference/check_ecotox_availability.md),
[`check_ecotox_build()`](https://pepijn-devries.github.io/ECOTOXr/reference/check_ecotox_build.md),
[`check_ecotox_version()`](https://pepijn-devries.github.io/ECOTOXr/reference/check_ecotox_version.md),
[`dbConnectEcotox()`](https://pepijn-devries.github.io/ECOTOXr/reference/dbConnectEcotox.md),
[`get_ecotox_info()`](https://pepijn-devries.github.io/ECOTOXr/reference/get_ecotox_info.md),
[`get_ecotox_sqlite_file()`](https://pepijn-devries.github.io/ECOTOXr/reference/get_path.md),
[`list_ecotox_fields()`](https://pepijn-devries.github.io/ECOTOXr/reference/list_ecotox_fields.md)

## Author

Pepijn de Vries

## Examples

``` r
## In order to cite downloaded database and this package:
cite_ecotox() |> suppressWarnings()
#> [[1]]
#> [[1]]$title
#> [1] "ECOTOXr: An R package for reproducible and transparent retrieval of data from EPA's ECOTOX database"
#> 
#> [[1]]$author
#> [1] "P. de Vries"
#> 
#> [[1]]$journal
#> [1] "Chemosphere"
#> 
#> [[1]]$year
#> [1] "2024"
#> 
#> [[1]]$volume
#> [1] "364"
#> 
#> [[1]]$pages
#> [1] "143078"
#> 
#> [[1]]$doi
#> [1] "10.1016/j.chemosphere.2024.143078"
#> 
#> [[1]]$note
#> [1] "R package version 1.2.4.0001"
#> 
#> attr(,"bibtype")
#> [1] "Article"
#> 
```
