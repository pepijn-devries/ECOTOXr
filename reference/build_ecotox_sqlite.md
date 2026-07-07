# Build an SQLite database from zip archived tables downloaded from EPA website

**\[stable\]** This function is called automatically after
[`download_ecotox_data()`](https://pepijn-devries.github.io/ECOTOXr/reference/download_ecotox_data.md).
The database files can also be downloaded manually from the [EPA
website](https://cfpub.epa.gov/ecotox/) from which a local database can
be build using this function.

## Usage

``` r
build_ecotox_sqlite(source, destination = get_ecotox_path(), write_log = TRUE)
```

## Arguments

- source:

  A `character` string pointing to the directory path where the text
  files with the raw tables are located. These can be obtained by
  extracting the zip archive from <https://cfpub.epa.gov/ecotox/> and
  look for 'Download ASCII Data'.

- destination:

  A `character` string representing the destination path for the SQLite
  file. By default this is
  [`get_ecotox_path()`](https://pepijn-devries.github.io/ECOTOXr/reference/get_path.md).

- write_log:

  A `logical` value indicating whether a log file should be written in
  the destination path `TRUE`. The log contains information on the
  source and destination path, the version of this package, the creation
  date, and the operating system on which the database was created.

## Value

Returns `NULL` invisibly.

## Details

Raw data downloaded from the EPA website is in itself not very efficient
to work with in R. The files are large and would put a large strain on R
when loading completely into the system's memory. Instead use this
function to build an SQLite database from the tables. That way, the data
can be queried without having to load it all into memory.

EPA provides the raw table from the [ECOTOX
database](https://cfpub.epa.gov/ecotox/) as text files with
pipe-characters ('\|') as table column separators. Although not
documented, the tables appear not to contain comment or quotation
characters. There are records containing the reserved pipe-character
that will confuse the table parser. For these records, the
pipe-character is replaced with a dash character ('-').

In addition, while reading the tables as text files, this package
attempts to decode the text as UTF8. Unfortunately, this process appears
to be platform-dependent, and may therefore result in different
end-results on different platforms. This problem only seems to occur for
characters that are listed as 'control characters' under UTF8. This will
have consequences for reproducibility, but only if you build search
queries that look for such special characters. It is therefore advised
to stick to common (non-accented) alpha-numerical characters in your
searches, for the sake of reproducibility.

Use '[`suppressMessages()`](https://rdrr.io/r/base/message.html)' to
suppress the progress report.

## See also

Other database-build-functions:
[`check_ecotox_build()`](https://pepijn-devries.github.io/ECOTOXr/reference/check_ecotox_build.md),
[`check_ecotox_version()`](https://pepijn-devries.github.io/ECOTOXr/reference/check_ecotox_version.md),
[`download_ecotox_data()`](https://pepijn-devries.github.io/ECOTOXr/reference/download_ecotox_data.md),
[`get_ecotox_url()`](https://pepijn-devries.github.io/ECOTOXr/reference/get_ecotox_url.md)

## Author

Pepijn de Vries

## Examples

``` r
source_path <- tempfile()
dir.create(source_path)

## This is a small mockup file resembling the larger zip
## files that can be downloaded with `download_ecotox_data()`:

source_file <- system.file("ecotox-test.zip", package = "ECOTOXr")

unzip(source_file, exdir = source_path)

build_ecotox_sqlite(source_path, tempdir())
#> ⠙ Table (0/50), added 0 lines to 'chemical_carriers'
#> ⠙ Table (50/50), added 0 lines to 'chemical_carriers'
#> ✔ Completed building the database
```
