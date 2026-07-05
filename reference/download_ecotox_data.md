# Download and extract ECOTOX database files and compose database

**\[stable\]** In order for this package to fully function, a local copy
of the ECOTOX database needs to be build. This function will download
the required data and build the database.

## Usage

``` r
download_ecotox_data(
  target = get_ecotox_path(),
  write_log = TRUE,
  ask = TRUE,
  verify_ssl = getOption("ECOTOXr_verify_ssl"),
  ...
)
```

## Arguments

- target:

  Target directory where the files will be downloaded and the database
  compiled. Default is
  [`get_ecotox_path()`](https://pepijn-devries.github.io/ECOTOXr/reference/get_path.md).

- write_log:

  A `logical` value indicating whether a log file should be written to
  the target path `TRUE`.

- ask:

  There are several steps in which files are (potentially) overwritten
  or deleted. In those cases the user is asked on the command line what
  to do in those cases. Set this parameter to `FALSE` in order to
  continue without warning and asking.

- verify_ssl:

  When set to `FALSE` the SSL certificate of the host (EPA) is not
  verified. Can also be set as option:
  `options(ECOTOXr_verify_ssl = TRUE)`. Default is `TRUE`.

- ...:

  Arguments passed on to
  [`httr2::req_options()`](https://httr2.r-lib.org/reference/req_options.html).

## Value

Returns `NULL` invisibly.

## Details

This function will attempt to find the latest download url for the
ECOTOX database from the [EPA
website](https://cfpub.epa.gov/ecotox/index.cfm) (see
[`get_ecotox_url()`](https://pepijn-devries.github.io/ECOTOXr/reference/get_ecotox_url.md)).
When found it will attempt to download the zipped archive containing all
required data. This data is then extracted and a local copy of the
database is build.

Use '[`suppressMessages()`](https://rdrr.io/r/base/message.html)' to
suppress the progress report.

## Known issues

On some machines this function fails to connect to the database download
URL from the [EPA website](https://cfpub.epa.gov/ecotox/index.cfm) due
to missing SSL certificates. Unfortunately, there is no easy fix for
this in this package. A work around is to download and unzip the file
manually using a different machine or browser that is less strict with
SSL certificates. You can then call
[`build_ecotox_sqlite()`](https://pepijn-devries.github.io/ECOTOXr/reference/build_ecotox_sqlite.md)
and point the `source` location to the manually extracted zip archive.
For this purpose
[`get_ecotox_url()`](https://pepijn-devries.github.io/ECOTOXr/reference/get_ecotox_url.md)
can be used. Alternatively, one could try to call
`download_ecotox_data()` by setting `verify_ssl = FALSE`; but only do so
when you trust the download URL from
[get_ecotox_URL()](https://pepijn-devries.github.io/ECOTOXr/reference/get_ecotox_url.md).

## See also

Other database-build-functions:
[`build_ecotox_sqlite()`](https://pepijn-devries.github.io/ECOTOXr/reference/build_ecotox_sqlite.md),
[`check_ecotox_build()`](https://pepijn-devries.github.io/ECOTOXr/reference/check_ecotox_build.md),
[`check_ecotox_version()`](https://pepijn-devries.github.io/ECOTOXr/reference/check_ecotox_version.md),
[`get_ecotox_url()`](https://pepijn-devries.github.io/ECOTOXr/reference/get_ecotox_url.md)

Other online-functions:
[`get_ecotox_url()`](https://pepijn-devries.github.io/ECOTOXr/reference/get_ecotox_url.md),
[`websearch_ecotox()`](https://pepijn-devries.github.io/ECOTOXr/reference/websearch.md)

## Author

Pepijn de Vries

## Examples

``` r
if (FALSE) { # \dontrun{
## This will download and build the database in your temp dir:
if (interactive()) {
  download_ecotox_data(tempdir())
}
} # }
```
