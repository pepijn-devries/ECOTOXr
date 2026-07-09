# Search and retrieve toxicity records from the database

**\[stable\]** Create (and execute) an SQL search query based on basic
search terms and options. This allows you to search the database,
without having to understand SQL.

## Usage

``` r
search_ecotox(
  search,
  output_fields = list_ecotox_fields("default"),
  group_by_results = TRUE,
  compute = FALSE,
  as_data_frame = TRUE,
  ...
)

search_ecotox_lazy(
  search,
  output_fields = list_ecotox_fields("default"),
  compute = FALSE,
  ...
)

search_query_ecotox(search, output_fields = list_ecotox_fields("default"), ...)
```

## Arguments

- search:

  A named `list` containing the search terms. The names of the elements
  should refer to the field (i.e. table header) in which the terms are
  searched. Use
  [`list_ecotox_fields()`](https://pepijn-devries.github.io/ECOTOXr/reference/list_ecotox_fields.md)
  to obtain a list of available field names.

  Each element in that list should contain another list with at least
  one element named 'terms'. This should contain a `vector` of
  `character` strings with search terms. Optionally, a second element
  named 'method' can be provided which should be set to either
  '`contains`' (default, when missing) or '`exact`'. In the first case
  the query will match any record in the indicated field that contains
  the search term. In case of '`exact`' it will only return exact
  matches. Note that searches are not case sensitive, but are picky with
  special (accented) characters. While building the local database (see
  [build_ecotox_sqlite](https://pepijn-devries.github.io/ECOTOXr/reference/build_ecotox_sqlite.md))
  such special characters may be treated differently on different
  operating systems. For the sake of reproducibility, the user is
  advised to stick with non-accented alpha-numeric characters.

  Search terms for a specific field (table header) will be combined with
  'or'. Meaning that any record that matches any of the terms are
  returned. For instance when 'latin_name' 'Daphnia magna' and
  'Skeletonema costatum' are searched, results for both species are
  returned. Search terms across fields (table headers) are combined with
  'and', which will narrow the search. For instance if 'chemical_name'
  'benzene' is searched in combination with 'latin_name' 'Daphnia
  magna', only tests where Daphnia magna are exposed to benzene are
  returned.

  When this search behaviour described above is not desirable, the user
  can either adjust the query manually, or use this function to perform
  several separate searches and combine the results afterwards.

  Beware that some field names are ambiguous and occur in multiple
  tables (like `cas_number' and `code'). When searching such fields, the
  search result may not be as expected.

- output_fields:

  A `vector` of `character` strings indicating which field names (table
  headers) should be included in the output. By default
  `[list_ecotox_fields]("default")` is used. Use
  `[list_ecotox_fields]("all")` to list all available fields.

- group_by_results:

  Ecological test results are generally the most informative element in
  the ECOTOX database. Therefore, this search function returns a table
  with unique results in each row.

  However, some tables in the database (such as 'chemical_carriers' and
  'dose_responses') have a one to many relationship with test results.
  This means that multiple chemical carriers can be linked to a single
  test result, similarly, multiple doses can also be linked to a single
  test result.

  By default the search results are grouped by test results. As a result
  not all doses or chemical carriers may be displayed in the output. Set
  the `group_by_results` parameter to `FALSE` in order to force SQLite
  to output all data (e.g., all carriers). But beware that test results
  may be duplicated in those cases.

- compute:

  The ECOTOXr package tries to construct database queries as lazy as
  possible. Meaning that R moves as much of the heavy lifting as
  possible to the database. When your search becomes complicated (e.g.,
  when including many output fields), you may run into trouble and hit
  the SQL parser limits. In those cases you can set this parameter to
  `TRUE`. Database queries are then computed in the process of joining
  tables. This is generally slower. Alternatively, you could try to
  include less output fields in order to simplify the query.

- as_data_frame:

  **\[experimental\]** `logical` value indicating whether the result
  should be converted into a `data.frame` (default is `TRUE`). When set
  to `FALSE` the data will be returned as a
  [`tbl_df()`](https://tibble.tidyverse.org/reference/tibble.html).

- ...:

  Arguments passed to
  [`dbConnectEcotox()`](https://pepijn-devries.github.io/ECOTOXr/reference/dbConnectEcotox.md)
  and other functions. You can use this when the database is not located
  at the default path
  ([`get_ecotox_path()`](https://pepijn-devries.github.io/ECOTOXr/reference/get_path.md)).

## Value

In case of `search_query_ecotox`, a `character` string containing an SQL
query is returned. This query is built based on the provided search
terms and options.

In case of `search_ecotox` a `data.frame` is returned based on the
search query built with `search_query_ecotox`. The `data.frame` is
unmodified as returned by SQLite, meaning that all fields are returned
as `character`s (even where the field types are 'date' or 'numeric').
Therefore, retrieved search results may need some post-processing with
[`process_ecotox_numerics()`](https://pepijn-devries.github.io/ECOTOXr/reference/process_ecotox_numerics.md)
[`as_numeric_ecotox()`](https://pepijn-devries.github.io/ECOTOXr/reference/as_numeric_ecotox.md)

The results are tagged with: a time stamp; the package version used; and
the file path of the SQLite database used in the search (when
applicable). These tags are added as attributes to the output table or
query.

## Details

The ECOTOX database is stored locally as an SQLite file, which can be
queried with SQL. These functions allow you to automatically generate an
SQL query and send it to the database, without having to understand SQL.
The function `search_query_ecotox` generates and returns the SQL query
(which can be edited by hand if desired). You can also directly call
`search_ecotox`, this will first generate the query, send it to the
database and retrieve the result.

Although the generated query is not optimized for speed, it should be
able to process most common searches within an acceptable time. The time
required for retrieving data from a search query depends on the
complexity of the query, the size of the query and the speed of your
machine. Most queries should be completed within seconds (or several
minutes at most) on modern machines. If your search require optimisation
for speed, you could try reordering the search fields. You can also edit
the query generated with `search_query_ecotox` by hand and retrieve it
with
[`DBI::dbGetQuery()`](https://dbi.r-dbi.org/reference/dbGetQuery.html).

Note that this package is actively maintained and this function may be
revised in future versions. In order to create reproducible results the
user must: always work with an official release from CRAN and document
the package and database version that are used to generate specific
results (see also
[`cite_ecotox()`](https://pepijn-devries.github.io/ECOTOXr/reference/cite_ecotox.md)).

## See also

Other search-functions:
[`websearch_ecotox()`](https://pepijn-devries.github.io/ECOTOXr/reference/websearch.md)

Other database-access-functions:
[`check_ecotox_availability()`](https://pepijn-devries.github.io/ECOTOXr/reference/check_ecotox_availability.md),
[`check_ecotox_build()`](https://pepijn-devries.github.io/ECOTOXr/reference/check_ecotox_build.md),
[`check_ecotox_version()`](https://pepijn-devries.github.io/ECOTOXr/reference/check_ecotox_version.md),
[`cite_ecotox()`](https://pepijn-devries.github.io/ECOTOXr/reference/cite_ecotox.md),
[`dbConnectEcotox()`](https://pepijn-devries.github.io/ECOTOXr/reference/dbConnectEcotox.md),
[`get_ecotox_info()`](https://pepijn-devries.github.io/ECOTOXr/reference/get_ecotox_info.md),
[`get_ecotox_sqlite_file()`](https://pepijn-devries.github.io/ECOTOXr/reference/get_path.md),
[`list_ecotox_fields()`](https://pepijn-devries.github.io/ECOTOXr/reference/list_ecotox_fields.md),
[`migrate_ecotox_path()`](https://pepijn-devries.github.io/ECOTOXr/reference/migrate_ecotox_path.md)

## Author

Pepijn de Vries

## Examples

``` r
## let's find the ids of all ecotox tests on species
## where Latin names contain either of 2 specific genus names and
## where they were exposed to the chemical benzene
if (check_ecotox_availability()) {
  search <-
    list(
      latin_name    = list(
        terms          = c("Skeletonema", "Daphnia"),
        method         = "contains"
      ),
      chemical_name = list(
        terms          = "benzene",
        method         = "exact"
      )
    )
  ## rows in result each represent a unique test id from the database
  result <- search_ecotox(search)
  query  <- search_query_ecotox(search)
  cat(query)
} else {
  print("Sorry, you need to use 'download_ecotox_data()' first in order for this to work.")
}
#> [1] "Sorry, you need to use 'download_ecotox_data()' first in order for this to work."
```
