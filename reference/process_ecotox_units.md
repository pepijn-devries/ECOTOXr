# Process ECOTOX search results by converting `character` to units where relevant

**\[experimental\]** The function
[`search_ecotox()`](https://pepijn-devries.github.io/ECOTOXr/reference/search_ecotox.md)
returns fields from the ECOTOX database as is. Fields that represent
units are not standardised in the database. Therefore, this format is
not consistently used throughout the database. `process_ecotox_units()`
takes a `data.frame` returned by
[`search_ecotox()`](https://pepijn-devries.github.io/ECOTOXr/reference/search_ecotox.md),
locates unit columns, represented by text, sanitises the text and
converts them to
[`units::mixed_units()`](https://r-quantities.github.io/units/reference/mixed_units.html)
objects. It will sanitise the unit fields as much as possible. Units
that could not be interpreted are returned as arbitrary `unit`.

## Usage

``` r
process_ecotox_units(x, .fns = as_unit_ecotox, ..., .names = NULL)
```

## Arguments

- x:

  A `data.frame` obtained with
  [`search_ecotox()`](https://pepijn-devries.github.io/ECOTOXr/reference/search_ecotox.md),
  for which the units need to be processed.

- .fns:

  Function to convert `character` to unit. By default
  [`as_unit_ecotox()`](https://pepijn-devries.github.io/ECOTOXr/reference/as_unit_ecotox.md)
  is used which also sanitises the input. You can also write a custom
  function.

- ...:

  Arguments passed to `.fns`.

- .names:

  A 'glue' specification used to rename the unit columns. By default it
  is `"{.col}"`, which will overwrite existing text columns with unit
  columns. You can for instance add a suffix with `"{.col}_unit"` if you
  want to rename the resulting unit columns.

## Value

Returns a `data.frame` in which the columns containing unit information
is converted from the character format from the database to actual unit
objects (
[`?units::units`](https://r-quantities.github.io/units/reference/units.html)).

## See also

Other ecotox-sanitisers:
[`as_date_ecotox()`](https://pepijn-devries.github.io/ECOTOXr/reference/as_date_ecotox.md),
[`as_numeric_ecotox()`](https://pepijn-devries.github.io/ECOTOXr/reference/as_numeric_ecotox.md),
[`as_unit_ecotox()`](https://pepijn-devries.github.io/ECOTOXr/reference/as_unit_ecotox.md),
[`mixed_to_single_unit()`](https://pepijn-devries.github.io/ECOTOXr/reference/mixed_to_single_unit.md),
[`process_ecotox_dates()`](https://pepijn-devries.github.io/ECOTOXr/reference/process_ecotox_dates.md),
[`process_ecotox_numerics()`](https://pepijn-devries.github.io/ECOTOXr/reference/process_ecotox_numerics.md)

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

  df_unit <-
    process_ecotox_units(df, warn = FALSE)
}
```
