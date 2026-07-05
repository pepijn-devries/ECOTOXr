# Process ECOTOX search results by converting `character` to dates where relevant

**\[experimental\]** The function
[`search_ecotox()`](https://pepijn-devries.github.io/ECOTOXr/reference/search_ecotox.md)
returns fields from the ECOTOX database as is. Fields that represent
dates are usually formatted as `"%m\%d\%Y"`. Unfortunately, this format
is not consistently used throughout the database.
`process_ecotox_dates()` takes a `data.frame` returned by
[`search_ecotox()`](https://pepijn-devries.github.io/ECOTOXr/reference/search_ecotox.md),
locates date columns, represented by text, sanitises the text and
converts them to `Date` objects. It will sanitise the date fields as
much as possible. It will correct most dates. Dates without a specified
calender year, a date range, illegal date format (even after sanitation)
are returned as `NA`.

## Usage

``` r
process_ecotox_dates(x, .fns = as_date_ecotox, ..., .names = NULL)
```

## Arguments

- x:

  A `data.frame` obtained with
  [`search_ecotox()`](https://pepijn-devries.github.io/ECOTOXr/reference/search_ecotox.md),
  for which the dates need to be processed.

- .fns:

  Function to convert `character` to `Date`. By default
  [`as_date_ecotox()`](https://pepijn-devries.github.io/ECOTOXr/reference/as_date_ecotox.md)
  is used which also sanitises the input. You can also use
  [`as.Date()`](https://rdrr.io/r/base/as.Date.html) if you don't want
  the sanitation step. You can also write a custom function.

- ...:

  Arguments passed to `.fns`.

- .names:

  A 'glue' specification used to rename the date columns. By default it
  is `"{.col}"`, which will overwrite existing text columns with date
  columns. You can for instance add a suffix with `"{.col}_date"` if you
  want to rename the resulting date columns.

## Value

Returns a `data.frame` in which the columns containing date information
is converted from the character format from the database to actual date
objects ( `"POSIXlt"` and `"POSIXct"`).

## See also

Other ecotox-sanitisers:
[`as_date_ecotox()`](https://pepijn-devries.github.io/ECOTOXr/reference/as_date_ecotox.md),
[`as_numeric_ecotox()`](https://pepijn-devries.github.io/ECOTOXr/reference/as_numeric_ecotox.md),
[`as_unit_ecotox()`](https://pepijn-devries.github.io/ECOTOXr/reference/as_unit_ecotox.md),
[`mixed_to_single_unit()`](https://pepijn-devries.github.io/ECOTOXr/reference/mixed_to_single_unit.md),
[`process_ecotox_numerics()`](https://pepijn-devries.github.io/ECOTOXr/reference/process_ecotox_numerics.md),
[`process_ecotox_units()`](https://pepijn-devries.github.io/ECOTOXr/reference/process_ecotox_units.md)

## Author

Pepijn de Vries

## Examples

``` r
if (check_ecotox_availability()) {
  df <- search_ecotox(
    list(
      latin_name    = list(
        terms          = c("Skeletonema", "Daphnia"),
        method         = "contains"
      ),
      chemical_name = list(
        terms          = "benzene",
        method         = "exact"
      )
    ), list_ecotox_fields("full"))

  df_dat <-
    process_ecotox_dates(df, warn = FALSE)
}
```
