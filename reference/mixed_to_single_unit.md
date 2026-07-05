# Convert mixed units to a specific unit

Converts a list of mixed units to a specific unit, using the `units`
package.

## Usage

``` r
mixed_to_single_unit(x, target_unit)
```

## Arguments

- x:

  A mixed units object
  ([`units::mixed_units()`](https://r-quantities.github.io/units/reference/mixed_units.html))
  to be converted to the `target_unit`

- target_unit:

  A `character` string representing the target unit

## Value

Returns a units object
([`?units::units`](https://r-quantities.github.io/units/reference/units.html)).
Values with units that cannot be converted to the `target_unit` is
returned as `NA`.

## See also

Other ecotox-sanitisers:
[`as_date_ecotox()`](https://pepijn-devries.github.io/ECOTOXr/reference/as_date_ecotox.md),
[`as_numeric_ecotox()`](https://pepijn-devries.github.io/ECOTOXr/reference/as_numeric_ecotox.md),
[`as_unit_ecotox()`](https://pepijn-devries.github.io/ECOTOXr/reference/as_unit_ecotox.md),
[`process_ecotox_dates()`](https://pepijn-devries.github.io/ECOTOXr/reference/process_ecotox_dates.md),
[`process_ecotox_numerics()`](https://pepijn-devries.github.io/ECOTOXr/reference/process_ecotox_numerics.md),
[`process_ecotox_units()`](https://pepijn-devries.github.io/ECOTOXr/reference/process_ecotox_units.md)

## Author

Pepijn de Vries

## Examples

``` r
mishmash <- as_unit_ecotox(c("mg/L", "ppt w/v", "% w/v", "mmol/L"))

## Note that 'mmol/L' cannot be converted to 'ug/L'
## without a molar mass. It is returned as `NA`
mixed_to_single_unit(mishmash, "ug/L")
#> Units: [ug/L]
#> [1] 1e+03 1e+06 1e+07    NA

mishmash <- as_unit_ecotox(c("h", "sec", "mi", "dph"))

mixed_to_single_unit(mishmash, "h")
#> Units: [h]
#> [1] 1.000000e+00 2.777778e-04 1.666667e-02 2.400000e+01
```
