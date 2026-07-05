# Values represented by ECOTOX `character` to `numeric`

**\[experimental\]** Similar to
[`as.numeric()`](https://rdrr.io/r/base/numeric.html), but it also
performs some text sanitising before coercing text to numerics.

## Usage

``` r
as_numeric_ecotox(x, range_fun = NULL, ..., warn = TRUE)
```

## Arguments

- x:

  A vector of `character` strings. It expects fields as commonly
  returned from the ECOTOX database.

- range_fun:

  Function to summarise range values. If `NULL` range values are
  returned as `NA`

- ...:

  Arguments passed to `range_fun`.

- warn:

  If set to `FALSE` warnings while converting text to numerics are
  suppressed.

## Value

A vector of `numeric` values with the same length as `x`.

## Details

The following steps are performed to sanitise text before coercing it to
numerics:

- Notes labelled with `"x"` or `"\*"` are removed.

- Operators (`">"`, `">="`, `"<"`, `"<="`, `"~"`, `"="`, `"ca"`, `"er"`)
  are removed.

- Text between brackets (`"()"`) is removed (including the brackets)

- Comma's are considered to be a thousand separator when they are
  located at any fourth character (from the right) and removed. Comma's
  at any other location is assumed to be a decimal separator and is
  replaced by a period.

- If there is a hyphen present (not preceded by an "`"e"` or `"E"`) it
  is probably representing a range of values. When `range_fun` is `NULL`
  it will result in a `NA`. Otherwise, the numbers are split at the
  hyphen and aggregated with `range_fun`

It is your own responsibility to check if the sanitising steps are
appropriate for your analyses.

## See also

Other ecotox-sanitisers:
[`as_date_ecotox()`](https://pepijn-devries.github.io/ECOTOXr/reference/as_date_ecotox.md),
[`as_unit_ecotox()`](https://pepijn-devries.github.io/ECOTOXr/reference/as_unit_ecotox.md),
[`mixed_to_single_unit()`](https://pepijn-devries.github.io/ECOTOXr/reference/mixed_to_single_unit.md),
[`process_ecotox_dates()`](https://pepijn-devries.github.io/ECOTOXr/reference/process_ecotox_dates.md),
[`process_ecotox_numerics()`](https://pepijn-devries.github.io/ECOTOXr/reference/process_ecotox_numerics.md),
[`process_ecotox_units()`](https://pepijn-devries.github.io/ECOTOXr/reference/process_ecotox_units.md)

## Author

Pepijn de Vries

## Examples

``` r
## a vector of commonly used notations in the database to represent
## numeric values 
char_num <- c("10", " 2", "3 ", "~5", "9.2*", "2,33",
              "2,333", "2.1(1.0 - 3.2)", "1-5", "1e-3")

## Text fields reported as ranges are returned as `NA`:
as_numeric_ecotox(char_num, warn = FALSE)
#>  [1]   10.000    2.000    3.000    5.000    9.200    2.330 2333.000    2.100
#>  [9]       NA    0.001
#> attr(,"has_notation")
#>  [1] FALSE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE
#> attr(,"has_brackets")
#>  [1] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE

## Text fields reported as ranges are processed with `range_fun`
as_numeric_ecotox(char_num, range_fun = median)
#>  [1]   10.000    2.000    3.000    5.000    9.200    2.330 2333.000    2.100
#>  [9]    3.000    0.001
#> attr(,"has_notation")
#>  [1] FALSE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE
#> attr(,"has_brackets")
#>  [1] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE
```
