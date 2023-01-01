
> `{ECOTOXr}` Harness information from the [US EPA ECOTOXicology
> Knowledgebase](https://www.epa.gov/ecotox/) [![R build
> status](https://github.com/pepijn-devries/ECOTOXr/workflows/R-CMD-check/badge.svg)](https://github.com/pepijn-devries/ECOTOXr/actions)
> [![version](https://www.r-pkg.org/badges/version/ECOTOXr)](https://CRAN.R-project.org/package=ECOTOXr)
> ![cranlogs](https://cranlogs.r-pkg.org/badges/ECOTOXr)

## Overview

<a href="https://github.com/pepijn-devries/ECOTOXr/"><img src="man/figures/logo.png" alt="ECOTOXr logo" align="right" /></a>
`{ECOTOXr}` can be used to explore and analyse data from the [US EPA
ECOTOX database](https://www.epa.gov/ecotox). More specifically you can:

-   Build a local SQLite copy of the [US EPA ECOTOX
    database](https://www.epa.gov/ecotox)
-   Search and extract data from the local database
-   Use experimental features to search the on-line dashboards:
    [ECOTOX](https://www.epa.gov/ecotox/search.cfm) and
    [CompTox](https://comptox.epa.gov/dashboard/batch-search)

## Why use `{ECOTOXr}`?

The `{ECOTOXr}` package allows you to search and extract data from the
[ECOTOXicological Knowledgebase](https://www.epa.gov/ecotox/) and import
it directly into `R`. This will allow you to formalize and document the
search- and extract-procedures in `R` code. This makes it easier to
share and reproduce such procedures and its results. Moreover, you can
directly apply any statistical analysis offered in `R`.

## Installation

> Get CRAN version

``` r
install.packages("ECOTOXr")
```

> Get development version on github

``` r
devtools::install_github('pepijn-devries/ECOTOXr')
```

## Usage

### Preparing the database

Although `{ECOTOXr}` has experimental features to search the on-line
database. The package will reach its full potential when you build a
copy of the database on your local machine.

> Download and build a local copy of the latest ASCII export of the US
> EPA ECOTOX database

``` r
download_ecotox_data()
```

### Searching the local database for species and substances

Obviously, searching the local database is only possible after the
download and build is ready (see previous section).

> Search the local database for tests of water flea Daphnia magna
> exposed to benzene

``` r
search_ecotox(
  list(
    latin_name    = list(terms = "Daphnia magna", method = "exact"),
    chemical_name = list(terms = "benzene",       method = "exact")
  )
)
```

### Three ways of querying the local database

Let’s have a look at 3 different approaches for retrieving a specific
record from the local database, using the unique identifier `result_id`.
The first option is to use the build in `search_ecotox` function. It
uses simple `R` syntax and allows you to search and collect any field
from any table in the database. Furthermore, all requested output fields
are automatically joined to the result without the end-user needing to
know anything about the database structure.

> Using the prefab function `search_ecotox` packaged by `{ECOTOXr}`

``` r
search_ecotox(
  list(
    result_id = list(terms = "401386", method = "exact")
  ),
  as_data_frame = F
)
#> 'dose_responses.response_site' was renamed 'dose_link_response_site'
#> 'chemicals.cas_number' was renamed 'test_cas'
#> 'chemicals.chemical_name' was renamed 'test_chemical'
#> 'dose_responses.dose_resp_id' was renamed 'dose_link_dose_resp_id'
#> # A tibble: 1 x 98
#>   test_cas test_grade test_gra~1 test_~2 test_~3 test_~4 test_~5 test_~6 test_~7
#> *    <int> <chr>      <chr>      <chr>   <chr>   <chr>   <chr>   <chr>   <chr>  
#> 1    71432 NR         None       None    NR      None    NR      None    NR     
#> # ... with 89 more variables: test_purity_comments <chr>,
#> #   organism_lifestage <chr>, organism_age_mean_op <chr>,
#> #   organism_age_mean <chr>, organism_age_min_op <chr>, organism_age_min <chr>,
#> #   organism_age_max_op <chr>, organism_age_max <chr>,
#> #   exposure_duration_mean_op <chr>, exposure_duration_mean <chr>,
#> #   exposure_duration_min_op <chr>, exposure_duration_min <chr>,
#> #   exposure_duration_max_op <chr>, exposure_duration_max <chr>, ...
```

If you like to use [`{dplyr}`](https://dplyr.tidyverse.org/) verbs, you
are in luck. SQLite database can be approached using `{dplyr}` verbs.
This approach will only return information from the `results` table. The
end-user will have to join other information (like test species and test
substance) manually. This does require knowledge of the database
structure.

> Using `{dplyr}` verbs

``` r
con <- dbConnectEcotox()
dplyr::tbl(con, "results") %>%
  dplyr::filter(result_id == "401386") %>%
  dplyr::collect()
#> # A tibble: 1 x 137
#>   result_id test_id sample_siz~1 sampl~2 sampl~3 sampl~4 sampl~5 sampl~6 sampl~7
#>       <int>   <int> <chr>        <chr>   <chr>   <chr>   <chr>   <chr>   <chr>  
#> 1    401386 1020021 None         NC      None    NC      None    NC      NC     
#> # ... with 128 more variables: sample_size_comments <chr>,
#> #   obs_duration_mean_op <chr>, obs_duration_mean <chr>,
#> #   obs_duration_min_op <chr>, obs_duration_min <chr>,
#> #   obs_duration_max_op <chr>, obs_duration_max <chr>, obs_duration_unit <chr>,
#> #   obs_duration_comments <chr>, endpoint <chr>, endpoint_comments <chr>,
#> #   trend <chr>, effect <chr>, effect_comments <chr>, measurement <chr>,
#> #   measurement_comments <chr>, response_site <chr>, ...
```

If you prefer working using `SQL` directly, that is fine too. The
[`{RSQLite}`](https://cran.r-project.org/package=RSQLite) package allows
you to get queries using `SQL` statements. The result is identical to
that of the previous approach. Here too the end-user needs knowledge of
the database structure in order to join additional data.

> Using `SQL` syntax

``` r
dbGetQuery(con, "SELECT * FROM results WHERE result_id='401386'") %>%
  dplyr::as_tibble()
#> # A tibble: 1 x 137
#>   result_id test_id sample_siz~1 sampl~2 sampl~3 sampl~4 sampl~5 sampl~6 sampl~7
#>       <int>   <int> <chr>        <chr>   <chr>   <chr>   <chr>   <chr>   <chr>  
#> 1    401386 1020021 None         NC      None    NC      None    NC      NC     
#> # ... with 128 more variables: sample_size_comments <chr>,
#> #   obs_duration_mean_op <chr>, obs_duration_mean <chr>,
#> #   obs_duration_min_op <chr>, obs_duration_min <chr>,
#> #   obs_duration_max_op <chr>, obs_duration_max <chr>, obs_duration_unit <chr>,
#> #   obs_duration_comments <chr>, endpoint <chr>, endpoint_comments <chr>,
#> #   trend <chr>, effect <chr>, effect_comments <chr>, measurement <chr>,
#> #   measurement_comments <chr>, response_site <chr>, ...
```

## Disclaimers

It is the end-users own responsibility to check the quality of collected
data, using the original referenced source in order to evaluate its
fitness for use, see also:
<https://cfpub.epa.gov/ecotox/help.cfm#info-limitations>.

Note that the package maintainer is not affiliated with the US EPA, this
package is therefore **not** official US EPA software.

## Resources

-   [Manual of the CRAN
    release](https://cran.r-project.org/web/packages/ECOTOXr/ECOTOXr.pdf)
-   EPA ECOTOX help <https://www.epa.gov/ecotox/help.cfm>
-   Olker, J. H.; Elonen, C. M.; Pilli, A.; Anderson, A.;
    Kinziger, B.; Erickson, S.; Skopinski, M.; Pomplun,
    A.; LaLone, C. A.; Russom, C. L.; Hoff, D. (2022):
    The ECOTOXicology Knowledgebase: A Curated Database of Ecologically
    Relevant Toxicity Tests to Support Environmental Research and Risk
    Assessment. *Environmental Toxicology and Chemistry* 41(6) 1520-1539
    <https://doi.org/10.1002/etc.5324>
