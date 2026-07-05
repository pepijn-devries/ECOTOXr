# Get ECOTOX download URL from EPA website

**\[stable\]** This function downloads the webpage at
<https://cfpub.epa.gov/ecotox/index.cfm>. It then searches for the
download link for the complete ECOTOX database and extract its URL.

## Usage

``` r
get_ecotox_url(verify_ssl = getOption("ECOTOXr_verify_ssl"), ...)
```

## Arguments

- verify_ssl:

  When set to `FALSE` the SSL certificate of the host (EPA) is not
  verified. Can also be set as option:
  `options(ECOTOXr_verify_ssl = TRUE)`. Default is `TRUE`.

- ...:

  arguments passed on to
  [`httr2::req_options()`](https://httr2.r-lib.org/reference/req_options.html)

## Value

Returns a `character` string containing the download URL of the latest
version of the EPA ECOTOX database.

## Details

This function is called by
[`download_ecotox_data()`](https://pepijn-devries.github.io/ECOTOXr/reference/download_ecotox_data.md)
which tries to download the file from the resulting URL. On some
machines this fails due to issues with the SSL certificate. The user can
try to download the file by using this URL in a different browser (or on
a different machine). Alternatively, the user could try to use
`[download_ecotox_data](verify_ssl = FALE)` when the download URL is
trusted.

## See also

Other database-build-functions:
[`build_ecotox_sqlite()`](https://pepijn-devries.github.io/ECOTOXr/reference/build_ecotox_sqlite.md),
[`check_ecotox_build()`](https://pepijn-devries.github.io/ECOTOXr/reference/check_ecotox_build.md),
[`check_ecotox_version()`](https://pepijn-devries.github.io/ECOTOXr/reference/check_ecotox_version.md),
[`download_ecotox_data()`](https://pepijn-devries.github.io/ECOTOXr/reference/download_ecotox_data.md)

Other online-functions:
[`download_ecotox_data()`](https://pepijn-devries.github.io/ECOTOXr/reference/download_ecotox_data.md),
[`websearch_ecotox()`](https://pepijn-devries.github.io/ECOTOXr/reference/websearch.md)

## Author

Pepijn de Vries

## Examples

``` r
if (interactive()) {
  get_ecotox_url()
}
```
