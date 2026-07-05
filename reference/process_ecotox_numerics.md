# Process ECOTOX search results by converting `character` to `numeric` where relevant

**\[experimental\]** The function
[`search_ecotox()`](https://pepijn-devries.github.io/ECOTOXr/reference/search_ecotox.md)
returns fields from the ECOTOX database as is. Many numeric values are
stored in the database as text. It is not uncommon that these text
fields cannot be converted directly and need some sanitising first.
`process_ecotox_numerics()` takes a `data.frame` returned by
[`search_ecotox()`](https://pepijn-devries.github.io/ECOTOXr/reference/search_ecotox.md),
locates numeric columns, represented by text, sanitises the text and
converts them to numerics.

## Usage

``` r
process_ecotox_numerics(
  x,
  .fns = as_numeric_ecotox,
  ...,
  add_units = FALSE,
  .names = NULL
)
```

## Arguments

- x:

  A `data.frame` obtained with
  [`search_ecotox()`](https://pepijn-devries.github.io/ECOTOXr/reference/search_ecotox.md),
  for which the numerics need to be processed.

- .fns:

  Function to convert `character` to `numeric`. By default
  [`as_numeric_ecotox()`](https://pepijn-devries.github.io/ECOTOXr/reference/as_numeric_ecotox.md)
  is used which also sanitises the input. You can also use
  [`as.numeric()`](https://rdrr.io/r/base/numeric.html) if you don't
  want the sanitation step. You can also write a custom function.

- ...:

  Arguments passed to `.fns`.

- add_units:

  A `logical` value. When set to `TRUE` corresponding units are parsed
  with
  [`as_unit_ecotox()`](https://pepijn-devries.github.io/ECOTOXr/reference/as_unit_ecotox.md)
  (if available) and added to the numeric value.

- .names:

  A 'glue' specification used to rename the numeric columns. By default
  it is `"{.col}"`, which will overwrite existing text columns with
  numeric columns. You can for instance add a suffix with `"{.col}_num"`
  if you want to rename the resulting numeric columns.

## Value

Returns a `data.frame` in which the columns containing numeric
information is converted from the character format from the database to
actual numerics.

## See also

Other ecotox-sanitisers:
[`as_date_ecotox()`](https://pepijn-devries.github.io/ECOTOXr/reference/as_date_ecotox.md),
[`as_numeric_ecotox()`](https://pepijn-devries.github.io/ECOTOXr/reference/as_numeric_ecotox.md),
[`as_unit_ecotox()`](https://pepijn-devries.github.io/ECOTOXr/reference/as_unit_ecotox.md),
[`mixed_to_single_unit()`](https://pepijn-devries.github.io/ECOTOXr/reference/mixed_to_single_unit.md),
[`process_ecotox_dates()`](https://pepijn-devries.github.io/ECOTOXr/reference/process_ecotox_dates.md),
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

  df_num <-
    process_ecotox_numerics(df, add_units = TRUE, warn = FALSE)
}
```
