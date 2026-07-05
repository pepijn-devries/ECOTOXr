# Sanitising ECOTOX

The ASCII files provided by the EPA contain all data required for
building the local database
([`download_ecotox_data()`](https://pepijn-devries.github.io/ECOTOXr/reference/download_ecotox_data.md)).
As documented by the EPA most table fields are stored as text (with a
few exceptions). During the build process, all fields are kept as is,
without any cleanup or standardisation. This is done to avoid any data
loss or corruption and keep it as close to the source as possible.
Therefore, it is likely that you need to post-process data after
querying the locally built database.

![](data:image/svg+xml;base64,PHN2ZyB3aWR0aD0iNjM2cHQiIGhlaWdodD0iNDRwdCIgdmlld2JveD0iMC4wMCAwLjAwIDYzNi4wMCAyNi4wMCIgeG1sbnM9Imh0dHA6Ly93d3cudzMub3JnLzIwMDAvc3ZnIiB4bWxuczp4bGluaz0iaHR0cDovL3d3dy53My5vcmcvMTk5OS94bGluayI+PGcgaWQ9ImdyYXBoMCIgY2xhc3M9ImdyYXBoIiB0cmFuc2Zvcm09InNjYWxlICgwLjYgMC42KSByb3RhdGUoMCkgdHJhbnNsYXRlKDQgNDApIj48dGl0bGU+UHJvVHJhY2tlciBtb2R1bGVzPC90aXRsZT4KPHBvbHlnb24gZmlsbD0iI2ZmZmZmZiIgc3Ryb2tlPSJ0cmFuc3BhcmVudCIgcG9pbnRzPSItNCw0IC00LC00MCA2MzIsLTQwIDYzMiw0IC00LDQiPjwvcG9seWdvbj48IS0tIGJ1aWxkIC0tPjxnIGlkPSJub2RlMSIgY2xhc3M9Im5vZGUiPjx0aXRsZT5idWlsZDwvdGl0bGU+CjxwYXRoIGZpbGw9IiNmZmZmZmYiIHN0cm9rZT0iIzAwMDAwMCIgZD0iTTExOCwtMzZDMTE4LC0zNiAxMiwtMzYgMTIsLTM2IDYsLTM2IDAsLTMwIDAsLTI0IDAsLTI0IDAsLTEyIDAsLTEyIDAsLTYgNiwwIDEyLDAgMTIsMCAxMTgsMCAxMTgsMCAxMjQsMCAxMzAsLTYgMTMwLC0xMiAxMzAsLTEyIDEzMCwtMjQgMTMwLC0yNCAxMzAsLTMwIDEyNCwtMzYgMTE4LC0zNiIgLz48dGV4dCB0ZXh0LWFuY2hvcj0ibWlkZGxlIiB4PSI2NSIgeT0iLTIyLjIiIGZvbnQtZmFtaWx5PSJIZWx2ZXRpY2Esc2Fucy1TZXJpZiIgZm9udC1zaXplPSIxNC4wMCIgZmlsbD0iIzAwMDAwMCI+YnVpbGQgZGF0YWJhc2U8L3RleHQ+PHRleHQgdGV4dC1hbmNob3I9Im1pZGRsZSIgeD0iNjUiIHk9Ii01LjQiIGZvbnQtZmFtaWx5PSJIZWx2ZXRpY2Esc2Fucy1TZXJpZiIgZm9udC1zaXplPSIxNC4wMCIgZmlsbD0iIzAwMDAwMCI+ZnJvbSBFUEEgZmlsZXM8L3RleHQ+PC9nPjwhLS0gcXVlcnkgLS0+PGcgaWQ9Im5vZGUyIiBjbGFzcz0ibm9kZSI+PHRpdGxlPnF1ZXJ5PC90aXRsZT4KPHBhdGggZmlsbD0iI2ZmZmZmZiIgc3Ryb2tlPSIjMDAwMDAwIiBkPSJNMjg0LC0zNkMyODQsLTM2IDE3OCwtMzYgMTc4LC0zNiAxNzIsLTM2IDE2NiwtMzAgMTY2LC0yNCAxNjYsLTI0IDE2NiwtMTIgMTY2LC0xMiAxNjYsLTYgMTcyLDAgMTc4LDAgMTc4LDAgMjg0LDAgMjg0LDAgMjkwLDAgMjk2LC02IDI5NiwtMTIgMjk2LC0xMiAyOTYsLTI0IDI5NiwtMjQgMjk2LC0zMCAyOTAsLTM2IDI4NCwtMzYiIC8+PHRleHQgdGV4dC1hbmNob3I9Im1pZGRsZSIgeD0iMjMxIiB5PSItMTMuOCIgZm9udC1mYW1pbHk9IkhlbHZldGljYSxzYW5zLVNlcmlmIiBmb250LXNpemU9IjE0LjAwIiBmaWxsPSIjMDAwMDAwIj5xdWVyeSBkYXRhYmFzZTwvdGV4dD48L2c+PCEtLSBidWlsZCYjNDU7Jmd0O3F1ZXJ5IC0tPjxnIGlkPSJlZGdlMSIgY2xhc3M9ImVkZ2UiPjx0aXRsZT5idWlsZC0mZ3Q7cXVlcnk8L3RpdGxlPgo8cGF0aCBmaWxsPSJub25lIiBzdHJva2U9IiMwMDAwMDAiIGQ9Ik0xMzAuMTMxMiwtMThDMTMwLjEzMTIsLTE4IDE1NS43ODQyLC0xOCAxNTUuNzg0MiwtMTgiIC8+PHBvbHlnb24gZmlsbD0iIzAwMDAwMCIgc3Ryb2tlPSIjMDAwMDAwIiBwb2ludHM9IjE1NS43ODQyLC0yMS41MDAxIDE2NS43ODQyLC0xOCAxNTUuNzg0MiwtMTQuNTAwMSAxNTUuNzg0MiwtMjEuNTAwMSI+PC9wb2x5Z29uPjwvZz48IS0tIHBvc3QgLS0+PGcgaWQ9Im5vZGUzIiBjbGFzcz0ibm9kZSI+PHRpdGxlPnBvc3Q8L3RpdGxlPgo8cGF0aCBmaWxsPSIjZmZmZmZmIiBzdHJva2U9IiMwMDAwMDAiIGQ9Ik00NTAsLTM2QzQ1MCwtMzYgMzQ0LC0zNiAzNDQsLTM2IDMzOCwtMzYgMzMyLC0zMCAzMzIsLTI0IDMzMiwtMjQgMzMyLC0xMiAzMzIsLTEyIDMzMiwtNiAzMzgsMCAzNDQsMCAzNDQsMCA0NTAsMCA0NTAsMCA0NTYsMCA0NjIsLTYgNDYyLC0xMiA0NjIsLTEyIDQ2MiwtMjQgNDYyLC0yNCA0NjIsLTMwIDQ1NiwtMzYgNDUwLC0zNiIgLz48dGV4dCB0ZXh0LWFuY2hvcj0ibWlkZGxlIiB4PSIzOTciIHk9Ii0yMi4yIiBmb250LWZhbWlseT0iSGVsdmV0aWNhLHNhbnMtU2VyaWYiIGZvbnQtc2l6ZT0iMTQuMDAiIGZpbGw9IiMwMDAwMDAiPnBvc3QgcHJvY2Vzcy88L3RleHQ+PHRleHQgdGV4dC1hbmNob3I9Im1pZGRsZSIgeD0iMzk3IiB5PSItNS40IiBmb250LWZhbWlseT0iSGVsdmV0aWNhLHNhbnMtU2VyaWYiIGZvbnQtc2l6ZT0iMTQuMDAiIGZpbGw9IiMwMDAwMDAiPmNsZWFudXA8L3RleHQ+PC9nPjwhLS0gcXVlcnkmIzQ1OyZndDtwb3N0IC0tPjxnIGlkPSJlZGdlMiIgY2xhc3M9ImVkZ2UiPjx0aXRsZT5xdWVyeS0mZ3Q7cG9zdDwvdGl0bGU+CjxwYXRoIGZpbGw9Im5vbmUiIHN0cm9rZT0iIzAwMDAwMCIgZD0iTTI5Ni4xMzEyLC0xOEMyOTYuMTMxMiwtMTggMzIxLjc4NDIsLTE4IDMyMS43ODQyLC0xOCIgLz48cG9seWdvbiBmaWxsPSIjMDAwMDAwIiBzdHJva2U9IiMwMDAwMDAiIHBvaW50cz0iMzIxLjc4NDIsLTIxLjUwMDEgMzMxLjc4NDIsLTE4IDMyMS43ODQyLC0xNC41MDAxIDMyMS43ODQyLC0yMS41MDAxIj48L3BvbHlnb24+PC9nPjwhLS0gYW5hIC0tPjxnIGlkPSJub2RlNCIgY2xhc3M9Im5vZGUiPjx0aXRsZT5hbmE8L3RpdGxlPgo8cGF0aCBmaWxsPSIjZmZmZmZmIiBzdHJva2U9IiMwMDAwMDAiIGQ9Ik02MTYsLTM2QzYxNiwtMzYgNTEwLC0zNiA1MTAsLTM2IDUwNCwtMzYgNDk4LC0zMCA0OTgsLTI0IDQ5OCwtMjQgNDk4LC0xMiA0OTgsLTEyIDQ5OCwtNiA1MDQsMCA1MTAsMCA1MTAsMCA2MTYsMCA2MTYsMCA2MjIsMCA2MjgsLTYgNjI4LC0xMiA2MjgsLTEyIDYyOCwtMjQgNjI4LC0yNCA2MjgsLTMwIDYyMiwtMzYgNjE2LC0zNiIgLz48dGV4dCB0ZXh0LWFuY2hvcj0ibWlkZGxlIiB4PSI1NjMiIHk9Ii0xMy44IiBmb250LWZhbWlseT0iSGVsdmV0aWNhLHNhbnMtU2VyaWYiIGZvbnQtc2l6ZT0iMTQuMDAiIGZpbGw9IiMwMDAwMDAiPmZ1cnRoZXIgYW5hbHlzZXM8L3RleHQ+PC9nPjwhLS0gcG9zdCYjNDU7Jmd0O2FuYSAtLT48ZyBpZD0iZWRnZTMiIGNsYXNzPSJlZGdlIj48dGl0bGU+cG9zdC0mZ3Q7YW5hPC90aXRsZT4KPHBhdGggZmlsbD0ibm9uZSIgc3Ryb2tlPSIjMDAwMDAwIiBkPSJNNDYyLjEzMTIsLTE4QzQ2Mi4xMzEyLC0xOCA0ODcuNzg0MiwtMTggNDg3Ljc4NDIsLTE4IiAvPjxwb2x5Z29uIGZpbGw9IiMwMDAwMDAiIHN0cm9rZT0iIzAwMDAwMCIgcG9pbnRzPSI0ODcuNzg0MiwtMjEuNTAwMSA0OTcuNzg0MiwtMTggNDg3Ljc4NDIsLTE0LjUwMDEgNDg3Ljc4NDIsLTIxLjUwMDEiPjwvcG9seWdvbj48L2c+PC9nPjwvc3ZnPg==)

Although it is the user’s responsibility to evaluate the correctness and
validity of the data, the `ECOTOXr` package provides some tools to make
the cleanup process easier. This vignette presents important aspects for
cleaning:

- [units](#sanitising-units)
- [numeric data](#sanitising-numerics)
- [dates](#sanitising-dates)

In general there are two types of sanitising functions; those named
`as_..._ecotox()` and those starting with `process_ecotox_...s()`. Where
‘`...`’ is the data type being sanitised (e.g. `unit`, `numeric`, or
`date`). The first function type (‘as’) handles vectors of `character`s.
The second function type (‘process’), handles `data.frame`s, where
relevant columns are automatically detected and processed with the ‘as’
type functions.

Note that the sanitation routines are subject to development, so they
may change. For reproducible results you should therefore always report
the version of `ECOTOXr` you are using
([`cite_ecotox()`](https://pepijn-devries.github.io/ECOTOXr/reference/cite_ecotox.md)).

## Sanitising units

Quantity units are vital for the interpretation of measurements. The
ECOTOX database contains units as reported by its source publication. As
a result, the units are often not stored consistently and are not
standardised. The `ECOTOXr` package implements a function that sanitises
the unit text fields and then parses them with the [units
package](https://r-quantities.github.io/units/). This package provides
instruments to convert units using the [UNIDATA udunits
library](https://www.unidata.ucar.edu/software/udunits).

The advantage of using the `units` package is that it provides a
mechanism to apply arithmetic manipulations of data and conversion
between compatible units. Or as the documentation of the package puts
it: “*When used in expression, it automatically converts units, and
simplifies units of results when possible; in case of incompatible
units, errors are raised*”

So the goal of the sanitation steps here is to create a format that can
be parsed by the `units` package. In order to achieve this the following
steps are performed:

- Annotations are stripped (i.e., prefixes and suffixes to units,
  indicating for instance ‘active ingredient’, the medium it refers to
  (e.g. soil), etc.)
- Ambiguous units like percentages and ‘parts per …’ are converted to
  more explicit units where possible. This is only possible when it is
  recorded if the units indicate if they are w/w, w/v, v/v or v/w. If
  this annotation is missing from ‘parts per …’ units, it is assumed to
  be weight over volume. The same is true for percentages, but only if
  the unit type is `concentration`.
- Units that are not known or interpreted incorrectly by the `units`
  package are converted such that they are handled correctly. For
  instance ‘C’ in the ECOTOX database frequently stands for ‘degrees
  Celsius’ (although it is also used to indicate Carbon). So if it’s not
  an annotation, ‘C’ is replaced with ‘Celsius’. Another example is
  ‘sqft’ (square feet) which can not be interpreted by the units package
  is replaced with ‘ft2’.
- Units that are not reported or interpretable are set as generic ‘unit’
  (`units::mixed_units(1, "unit")`).

The documentation of the
[`as_unit_ecotox()`](https://pepijn-devries.github.io/ECOTOXr/reference/as_unit_ecotox.md)
function has a more detailed description of the cleanup procedure. If
you need even more details you can check the [source
code](https://github.com/pepijn-devries/ECOTOXr/blob/main/R/process_unit.r).

In order to demonstrate how unit sanitation works in this packages,
let’s first initialise a vector of mishmash units. These are actually a
random sample from the ECOTOX database, not necessarily the most common
ones:

``` r

library(ECOTOXr) |> suppressMessages()
library(dplyr)   |> suppressMessages()

mishmash <-
  c("ppm-d", "ml/2.5 cm eu", "fl oz/10 gal/1k sqft", "kg/100 L",
    "mopm", "ng/kg", "ug", "AI ng/g", "PH", "pm", "uM/cm3", "1e-4 mM",
    "degree", "fs", "mg/TI", "RR", "ug/g org/d", "1e+4 IU/TI", "pg/mg TE",
    "pmol/mg", "1e-9/l", "no >15 cm", "umol/mg pro", "cc/org/wk", "PIg/L",
    "ug/100 ul/org", "ae mg/kg diet/d", "umol/mg/h", "cmol/kg d soil",
    "ug/L diet", "kg/100 kg sd", "1e+6 cells", "ul diet", "S", "mmol/h/g TI",
    "g/70 d", "vg", "ng/200 mg diet", "uS/cm2", "AI ml/ha", "AI pt/acre",
    "mg P/h/g TI", "no/m", "kg/ton sd", "ug/g wet wt", "AI mg/2 L diet",
    "nmol/TI", "umol/g wet wt", "PSU", "Wijs number")
```

With
[`as_unit_ecotox()`](https://pepijn-devries.github.io/ECOTOXr/reference/as_unit_ecotox.md),
the mishmash of units, represented by `character` strings are cleaned
and coerced to
[`units::mixed_units()`](https://r-quantities.github.io/units/reference/mixed_units.html).
As `units` objects have a numeric component, but the `character` strings
from the database do not, each unit is given a value of `1`. As you can
see not all units in the `mishmash` vector can be interpreted and are
just returned as arbitrary `1 unit`.

``` r

as_unit_ecotox(mishmash, warn = FALSE)
#> Mixed units: ppm*d (1), ml/(2.5cm) (1), oz/(10gal*1000ft^2) (1), kg/(100L) (1), month (1), ng/kg (1), ug (1), ng/g (1), unit (12), umol/(L*cm^3) (1), 0.0001mmol/L (1), ° (1), ug/(g*d) (1), pg/mg (1), pmol/mg (1), 1e-09/L (1), umol/mg (1), counts/(counts*week) (1), ug/(100ul*counts) (1), mg/(kg*d) (1), umol/(mg*h) (1), cmol/kg (1), ug/L (1), kg/(100kg) (1), 1000000counts (1), ul (1), S (1), mmol/(h*g) (1), g/(70d) (1), ng/(200mg) (1), uS/cm^2 (1), ml/ha (1), pt/acre (1), mg/(h*g) (1), counts/m (1), kg/ton (1), ug/g (1), mg/(2L) (1), umol/g (1) 
#> 1 [ppm*d], 1 [ml/(2.5cm)], 1 [oz/(10gal*1000ft^2)], 1 [kg/(100L)], 1 [month], 1 [ng/kg], 1 [ug], 1 [ng/g], 1 [unit], 1 [unit], 1 [umol/(L*cm^3)], 1 [0.0001mmol/L], 1 [°], 1 [unit], 1 [unit], 1 [unit], 1 [ug/(g*d)], 1 [unit], 1 [pg/mg], 1 [pmol/mg], 1 [1e-09/L], 1 [unit], 1 [umol/mg], 1 [counts/(counts*week)], 1 [unit], 1 [ug/(100ul*counts)], 1 [mg/(kg*d)], 1 [umol/(mg*h)], 1 [cmol/kg], 1 [ug/L], 1 [kg/(100kg)], 1 [1000000counts], 1 [ul], 1 [S], 1 [mmol/(h*g)], 1 [g/(70d)], 1 [unit], 1 [ng/(200mg)], 1 [uS/cm^2], 1 [ml/ha], 1 [pt/acre], 1 [mg/(h*g)], 1 [counts/m], 1 [kg/ton], 1 [ug/g], 1 [mg/(2L)], 1 [unit], 1 [umol/g], 1 [unit], 1 [unit]
```

With
[`process_ecotox_units()`](https://pepijn-devries.github.io/ECOTOXr/reference/process_ecotox_units.md)
you can process an entire `data.frame`/`tibble`, where each column
ending with `_unit` is processed
(i.e. [`as_unit_ecotox()`](https://pepijn-devries.github.io/ECOTOXr/reference/as_unit_ecotox.md)
is called on them). By setting the `.names` argument, you can preserve
the original unit column:

``` r

tibble(mishmash_unit = mishmash) |>
  process_ecotox_units(.names = "{.col}_parsed", warn = FALSE)
#> # A tibble: 50 × 2
#>    mishmash_unit           mishmash_unit_parsed
#>    <chr>                          <mixed_units>
#>  1 ppm-d                              1 [ppm*d]
#>  2 ml/2.5 cm eu                  1 [ml/(2.5cm)]
#>  3 fl oz/10 gal/1k sqft 1 [oz/(10gal*1000ft^2)]
#>  4 kg/100 L                       1 [kg/(100L)]
#>  5 mopm                               1 [month]
#>  6 ng/kg                              1 [ng/kg]
#>  7 ug                                    1 [ug]
#>  8 AI ng/g                             1 [ng/g]
#>  9 PH                                  1 [unit]
#> 10 pm                                  1 [unit]
#> # ℹ 40 more rows
```

### Consequences of unit sanitation

As the database contains over 6,000 unique unit codes, it is likely that
not all units are processable. Also, because the codes are not always
consistent, some of them may not be interpreted correctly. Most
frequently occurring units should parse correctly. If you think a
specific code is not parsed correctly, and it is not highly outlandish,
you could [file an issue
report](https://github.com/pepijn-devries/ECOTOXr/issues). Furthermore,
you should always inspect automatically parsed units for correctness.

Another point of attention is the removal of annotations from the unit.
Consider the concentration unit with the following annotations:

``` r

as_unit_ecotox(c("mg/L CO3", "mg/L CaCO3", "mg/L HCO3"))
#> Mixed units: mg/L (3) 
#> 1 [mg/L], 1 [mg/L], 1 [mg/L]
```

Note that they are all interpreted as `[mg/L]`. Although technically the
same unit, they are definitely not directly compatible. The `units`
package does not support annotations, so you need to keep track of them
yourself.

## Sanitising numerics

First let me explain what is meant by ‘numerics’ in the ECOTOX database.
These are all records that have a accompanying measurement unit in the
database. This includes, concentrations, durations and many others.
These records are stored as text fields in the database. So in order to
interpret them as actual numerics in R, they need to be coerced to
numerics. You could use a simple call to
[`as.numeric()`](https://rdrr.io/r/base/numeric.html) to do this, but
that will not always work.

The text fields may contain operators such as ‘\<’, ‘\>’, ‘~’, etc. I
think this is a mistake and not by design, because many of the numeric
fields have a corresponding operator field where this operator could be
stored. Text fields can also contain labelling text (such as asterisk
symbol) or inconsistent decimal or thousand separators.

This is why there is
[`as_numeric_ecotox()`](https://pepijn-devries.github.io/ECOTOXr/reference/as_numeric_ecotox.md)
which first cleans the text records before coercing them to numerics:

``` r

## Text fields as possibly encountered in the database
text_records <-
  c("10", " 2", "3 ", "~5", "9.2*", "2,33",
    "2,333", "2.1(1.0 - 3.2)", "1-5", "1e-3")
  
as_numeric_ecotox(text_records)
#> Warning in as_numeric_ecotox(text_records): NAs introduced by coercion
#>  [1]   10.000    2.000    3.000    5.000    9.200    2.330 2333.000    2.100
#>  [9]       NA    0.001
#> attr(,"has_notation")
#>  [1] FALSE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE
#> attr(,"has_brackets")
#>  [1] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE
```

You can use
[`process_ecotox_numerics()`](https://pepijn-devries.github.io/ECOTOXr/reference/process_ecotox_numerics.md)
to process a `data.frame`/`tibble` resulting from a search query. It
automatically applies
[`as_numeric_ecotox()`](https://pepijn-devries.github.io/ECOTOXr/reference/as_numeric_ecotox.md)
to columns containing numeric information:

``` r

text_tbl <- tibble(conc1_mean = text_records)

process_ecotox_numerics(text_tbl, warn = FALSE)
#> # A tibble: 10 × 1
#>    conc1_mean
#>         <dbl>
#>  1     10    
#>  2      2    
#>  3      3    
#>  4      5    
#>  5      9.2  
#>  6      2.33 
#>  7   2333    
#>  8      2.1  
#>  9     NA    
#> 10      0.001
```

### Consequences of numeric sanitation

As indicated above all notations and operators included with numerics
are stripped in the cleaning process. These notations and operators are
potentially important for the interpretation of the values. It may be
wise to keep track of them. One way to do this is by first trying to
coerce texts to numerics with
[`as.numeric()`](https://rdrr.io/r/base/numeric.html) and then with
[`as_numeric_ecotox()`](https://pepijn-devries.github.io/ECOTOXr/reference/as_numeric_ecotox.md).
The cases where the first returns `NA` but the latter returns a value,
is likely to contain notations or operators (or is just inconsistently
formatted). You could also use the `.names` argument in
[`process_ecotox_numerics()`](https://pepijn-devries.github.io/ECOTOXr/reference/process_ecotox_numerics.md)
to rename the numeric columns and keep the original text fields.

``` r

process_ecotox_numerics(text_tbl, warn = FALSE, .names = "{.col}_num") |>
  mutate(
    test = is.na(as.numeric(conc1_mean)) &
      !is.na(as_numeric_ecotox(conc1_mean, warn = FALSE))
    )
#> Warning: There was 1 warning in `mutate()`.
#> ℹ In argument: `test = &...`.
#> Caused by warning:
#> ! NAs introduced by coercion
#> # A tibble: 10 × 3
#>    conc1_mean       conc1_mean_num test 
#>    <chr>                     <dbl> <lgl>
#>  1 "10"                     10     FALSE
#>  2 " 2"                      2     FALSE
#>  3 "3 "                      3     FALSE
#>  4 "~5"                      5     TRUE 
#>  5 "9.2*"                    9.2   TRUE 
#>  6 "2,33"                    2.33  TRUE 
#>  7 "2,333"                2333     TRUE 
#>  8 "2.1(1.0 - 3.2)"          2.1   TRUE 
#>  9 "1-5"                    NA     FALSE
#> 10 "1e-3"                    0.001 FALSE
```

## Combining numerics with units

The steps above show how to sanitise numerics and units separately. In
order to standardise numeric values to a specific unit, these steps need
to be combined. This can be achieved by calling
[`process_ecotox_numerics()`](https://pepijn-devries.github.io/ECOTOXr/reference/process_ecotox_numerics.md)
with `add_units` set to `TRUE`. This will add the corresponding units to
the numeric value. But they are still mixed units. By calling
[`mixed_to_single_unit()`](https://pepijn-devries.github.io/ECOTOXr/reference/mixed_to_single_unit.md)
you can convert the values to a specific unit.

``` r

tibble(
  conc1_mean = c("1", "2", "5e-4", "0.2"),
  conc1_unit = c("mg/L", "M", "% w/v", "ppt w/v")
) |>
  process_ecotox_numerics(add_units = TRUE) |>
  mutate(
    conc1_mean_standard = mixed_to_single_unit(conc1_mean, "ug/L")
  )
#> # A tibble: 4 × 3
#>      conc1_mean conc1_unit conc1_mean_standard
#>   <mixed_units> <chr>                   [ug/L]
#> 1      1 [mg/L] mg/L                      1000
#> 2     2 [mol/L] M                           NA
#> 3  5e-04 [g/dL] % w/v                     5000
#> 4     0.2 [g/L] ppt w/v                 200000
```

Note that in the example above not all units can be converted to the
target unit `"ug/L"`. This is because concentrations in ‘mol/L’ requires
the molar mass of the solute in order to convert. It is returned as
`NA`.

## Sanitising dates

The ECOTOX contains several date fields. They can represent
meta-information about the record (date created and modified),
administrative information (publication date), or experimental
information (application date). These dates are stored as text in the
database. As not all records are consistent or complete, some cleaning
is required before coercing the text to a Date object
([`?Date`](https://rdrr.io/r/base/Dates.html)).

The example below shows some typical date formats as encountered in the
database and how to coerce them to `Date` objects using
[`as_date_ecotox()`](https://pepijn-devries.github.io/ECOTOXr/reference/as_date_ecotox.md):

``` r

char_date <- c("5-19-1987   ", "5/dd/2021", "3/19/yyyy", "1985", "mm/19/1999",
               "October 2004", "nr/nr/2015")

as_date_ecotox(char_date)
#> [1] "1987-05-19" "2021-05-01" NA           "1985-01-01" "1999-01-19"
#> [6] "2004-10-01" "2015-01-01"
```

The only date that cannot be coerced is the one with an unspecified
year. It is returned as `NA`.

You can use
[`process_ecotox_dates()`](https://pepijn-devries.github.io/ECOTOXr/reference/process_ecotox_dates.md)
to process a `data.frame`/`tibble` as returned by a search query. Date
columns are automatically coerced with
[`as_date_ecotox()`](https://pepijn-devries.github.io/ECOTOXr/reference/as_date_ecotox.md).
Column names ending with `_date` are recognised as date records.

``` r

tibble(
  my_date = char_date
) |>
  process_ecotox_dates()
#> # A tibble: 7 × 1
#>   my_date   
#>   <date>    
#> 1 1987-05-19
#> 2 2021-05-01
#> 3 NA        
#> 4 1985-01-01
#> 5 1999-01-19
#> 6 2004-10-01
#> 7 2015-01-01
```
