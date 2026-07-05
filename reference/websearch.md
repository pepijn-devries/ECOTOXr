# Search and retrieve toxicity records from the online database

**\[experimental\]** Functions to search and retrieve records from the
online database at <https://cfpub.epa.gov/ecotox/search.cfm>.

## Usage

``` r
websearch_ecotox(
  fields = list_ecotox_web_fields(),
  habitat = c("aquire", "terrestrial"),
  verify_ssl = getOption("ECOTOXr_verify_ssl"),
  ...
)

list_ecotox_web_fields(...)
```

## Arguments

- fields:

  A named `list` of `character`s, used to build a search for for the
  online search query of <https://cfpub.epa.gov/ecotox/search.cfm>. Use
  `list_ecotox_web_fields()` to construct a valid list.

- habitat:

  Use `aquire` (default) to retrieve aquatic data, `terrestrial` for,
  you've guessed it, terrestrial data.

- verify_ssl:

  When set to `FALSE` the SSL certificate of the host (EPA) is not
  verified. Can also be set as option:
  `options(ECOTOXr_verify_ssl = TRUE)`. Default is `TRUE`.

- ...:

  In case of `list_ecotox_web_fields()` the dots can be used as search
  field values used to update the returned list of fields.

  In case of `websearch_ecotox()` the dots can be used to pass custom
  options to the underlying
  [`httr2::req_options()`](https://httr2.r-lib.org/reference/req_options.html)
  call. For available field names, use `names(list_ecotox_web_fields())`

## Value

Returns named `list` of
[dplyr::tibble](https://tibble.tidyverse.org/reference/tibble.html)s
with search results. Results are unpolished and \`as is' returned by
EPA's web service.

`list_ecotox_web_fields()` returns a named list with fields that can be
used in a web search of EPA's ECOTOX database, using
`websearch_ecotox()`.

## Details

The functions described here to search and retrieve records from the
online database are experimental. This is because this feature is not
formally supported by the EPA, and it may break in future iterations of
the online database. The functions form an interface between R and the
ECOTOX website and is therefore limited by its restrictions as described
in the package documentation:
[ECOTOXr](https://pepijn-devries.github.io/ECOTOXr/reference/ECOTOXr-package.md).
The functions should therefore be used with caution.

## Note

**IMPORTANT:** when you plan to perform multiple adjacent searches (for
instance in a loop), please insert a call to
[`Sys.sleep()`](https://rdrr.io/r/base/Sys.sleep.html). This to avoid
overloading the server and getting your IP address banned from the
server.

## See also

Other online-functions:
[`download_ecotox_data()`](https://pepijn-devries.github.io/ECOTOXr/reference/download_ecotox_data.md),
[`get_ecotox_url()`](https://pepijn-devries.github.io/ECOTOXr/reference/get_ecotox_url.md)

Other search-functions:
[`search_ecotox()`](https://pepijn-devries.github.io/ECOTOXr/reference/search_ecotox.md)

## Author

Pepijn de Vries

## Examples

``` r
if (interactive()) {
  search_fields <-
    list_ecotox_web_fields(
      txAdvancedSpecEntries     = "daphnia magna",
      RBSPECSEARCHTYPE          = "EXACT",
      txAdvancedChemicalEntries = "benzene",
      RBCHEMSEARCHTYPE          = "EXACT")
  search_results <- websearch_ecotox(search_fields)
}
```
