# Values represented by ECOTOX `character` to dates

**\[experimental\]** Similar to
[`as.Date()`](https://rdrr.io/r/base/as.Date.html), but it also performs
some text sanitising before coercing text to dates.

## Usage

``` r
as_date_ecotox(x, dd = 1L, mm = 1L, nr = 1L, ..., warn = TRUE)
```

## Arguments

- x:

  A vector of `character` strings. It expects fields as commonly
  returned from the ECOTOX database.

- dd:

  Replacement values for unspecified days in a date. Defaults to `1L`.
  If you want dates with unspecified days to result in `NA`, use
  `dd = -1L`.

- mm:

  Replacement values for unspecified months in a date. Defaults to `1L`.
  If you want dates with unspecified months to result in `NA`, use
  `mm = -1L`.

- nr:

  Replacement values for generically unspecified values in a date.
  Defaults to `1L`. If you want dates with unspecified values to result
  in `NA`, use `nr = -1L`.

- ...:

  Passed to [`as.Date()`](https://rdrr.io/r/base/as.Date.html).

- warn:

  If set to `FALSE` warnings while converting text to dates are
  suppressed.

## Value

A vector of `Date` class objects with the same length as `x`.

## Details

The following steps are performed (in the order as listed) to sanitise
text before coercing it to numerics:

- Trim whitespaces

- Replace hyphens with forward slashes

- Replace double forward slashes, forward slashes followed by a zero and
  spaces, with a single forward slash

- Replace `"mm"` or `"dd"` (case insensitive) with the value specified
  as argument. Add a forward slash to it when missing.

- Treat `"na"`, `"nr"`, `"xx"` and `"00"` (case insensitive) as
  unreported values when followed by a forward slash. Replace it with
  the `nr` argument

- Remove alphabetical characters when directly followed by a numerical
  character.

- Replace literal month names with its numerical calendar value (1-12).

- When the date consists of one value, assume it is a calender year and
  add `dd` and `mm` as day and month value.

- If a date consists of two numbers, assume it is month, followed by
  year. In that case insert the `dd` value for the day.

It is your own responsibility to check if the sanitising steps are
appropriate for your analyses.

## See also

Other ecotox-sanitisers:
[`as_numeric_ecotox()`](https://pepijn-devries.github.io/ECOTOXr/reference/as_numeric_ecotox.md),
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
## dates. Most frequent format is %m/%d/%Y
char_date <- c("5-19-1987   ", "5/dd/2021", "3/19/yyyy", "1985", "mm/19/1999",
               "October 2004", "nr/nr/2015")

as_date_ecotox(char_date)
#> [1] "1987-05-19" "2021-05-01" NA           "1985-01-01" "1999-01-19"
#> [6] "2004-10-01" "2015-01-01"

## Set unspecified days to 15:
as_date_ecotox(char_date, dd = 15L)
#> [1] "1987-05-19" "2021-05-15" NA           "1985-01-15" "1999-01-19"
#> [6] "2004-10-15" "2015-01-01"

## Unspecified days should result in NA:
as_date_ecotox(char_date, dd = -1L)
#> [1] "1987-05-19" NA           NA           NA           "1999-01-19"
#> [6] NA           "2015-01-01"

## Set unspecified months to 6:
as_date_ecotox(char_date, mm = 6L)
#> [1] "1987-05-19" "2021-05-01" NA           "1985-06-01" "1999-06-19"
#> [6] "2004-10-01" "2015-01-01"

## Set generically unspecified value to 6:
as_date_ecotox(char_date, nr = 6L)
#> [1] "1987-05-19" "2021-05-01" NA           "1985-01-01" "1999-01-19"
#> [6] "2004-10-01" "2015-06-06"
```
