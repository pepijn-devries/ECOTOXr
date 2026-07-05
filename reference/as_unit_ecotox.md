# Text from the ECOTOX database to `mixed_units`

**\[experimental\]** Convert text to units after sanitising.

## Usage

``` r
as_unit_ecotox(
  x,
  type = c("concentration", "duration", "length", "media", "application", "size",
    "weight", "unknown"),
  ...,
  warn = TRUE
)
```

## Arguments

- x:

  A vector of `character` strings. It expects fields as commonly
  returned from the ECOTOX database.

- type:

  The type of unit that can help the sanitation process. See the 'usage'
  section for available options. These options are linked to the
  different unit tables in the database (see
  [`vignette("ecotox-schema")`](https://pepijn-devries.github.io/ECOTOXr/articles/ecotox-schema.md)).
  It can help to interpret ambiguous units correctly. For instance,
  'dpm' can both mean 'disintegrations per minute'
  (`type = "concentration"`) and 'days post-moult'
  (`type = "duration"`).

- ...:

  Ignored.

- warn:

  If set to `FALSE` warnings while converting text to units are
  suppressed.

## Value

A vector of `?units::unit` class objects with the same length as `x`.

## Details

The following steps are performed (in the order as listed) to sanitise
text before coercing it to units:

- The following is removed:

  - Leading/trailing white spaces

  - Square brackets and commas

  - A list of common prefixes

  - Double spaces are replaced by single spaces

  - Brackets around multiply symbol

- The following is corrected/adjusted:

  - 'for' is interpreted as multiplication

  - Scientific notation of numbers is standardised where possible.

  - A list of ambiguous patterns is replaced with more explicit strings.
    For instance, 'deg' is replaced with 'degree'.

- The following miscellaneous corrections are made:

  - A list of 'known' annotations are removed from the units

  - A list of elements known to represent counts are renamed 'counts'.

  - Percentages are renamed as explicit concentration in mass per volume
    or volume per volume units where possible

  - 'CI' is renamed 'Curies'.

  - 'M' is renamed 'mol/L'.

  - Units expressed as 'parts per ...' are explicitly renamed to mass
    over volume, or volume over volume where possible

- Type specific sanitation steps

  - Concentration units:

    - 'K' is renamed 'Karmen'

    - 'dpm' is renamed 'counts/min' (i.e., disintegrations per minute)

  - Media units:

    - 'K' is renamed 'Kelvin'

    - 'C' is renamed 'Celsius'

- Some final miscellaneous adjustments:

  - Scientific notation in numbers is not supported by the units
    package. Numbers are formatted in decimal notation where possible.

  - Spaces are removed if preceded by numeric and followed by
    alphabetical character

  - All equivalents of ambiguous synonyms for time units are explicitly
    renamed to their respective unit (e.g., 'dph' (days post hatching)
    -\> 'day')

  - unreported/missing units are renamed 'unit'

It is your own responsibility to check if the sanitising steps are
appropriate for your analyses.

## See also

Other ecotox-sanitisers:
[`as_date_ecotox()`](https://pepijn-devries.github.io/ECOTOXr/reference/as_date_ecotox.md),
[`as_numeric_ecotox()`](https://pepijn-devries.github.io/ECOTOXr/reference/as_numeric_ecotox.md),
[`mixed_to_single_unit()`](https://pepijn-devries.github.io/ECOTOXr/reference/mixed_to_single_unit.md),
[`process_ecotox_dates()`](https://pepijn-devries.github.io/ECOTOXr/reference/process_ecotox_dates.md),
[`process_ecotox_numerics()`](https://pepijn-devries.github.io/ECOTOXr/reference/process_ecotox_numerics.md),
[`process_ecotox_units()`](https://pepijn-devries.github.io/ECOTOXr/reference/process_ecotox_units.md)

## Author

Pepijn de Vries

## Examples

``` r
## Try parsing a random set of units from the database:
c("ppm-d", "ml/2.5 cm eu", "fl oz/10 gal/1k sqft", "kg/100 L",
  "mopm", "ng/kg", "ug", "AI ng/g", "PH", "pm", "uM/cm3", "1e-4 mM",
  "degree", "fs", "mg/TI", "RR", "ug/g org/d", "1e+4 IU/TI", "pg/mg TE",
  "pmol/mg", "1e-9/l", "no >15 cm", "umol/mg pro", "cc/org/wk", "PIg/L",
  "ug/100 ul/org", "ae mg/kg diet/d", "umol/mg/h", "cmol/kg d soil",
  "ug/L diet", "kg/100 kg sd", "1e+6 cells", "ul diet", "S", "mmol/h/g TI",
  "g/70 d", "vg", "ng/200 mg diet", "uS/cm2", "AI ml/ha", "AI pt/acre",
  "mg P/h/g TI", "no/m", "kg/ton sd", "ug/g wet wt", "AI mg/2 L diet",
  "nmol/TI", "umol/g wet wt", "PSU", "Wijs number") |>
  as_unit_ecotox(warn = FALSE)
#> Mixed units: ppm*d (1), ml/(2.5cm) (1), oz/(10gal*1000ft^2) (1), kg/(100L) (1), month (1), ng/kg (1), ug (1), ng/g (1), unit (12), umol/(L*cm^3) (1), 0.0001mmol/L (1), ° (1), ug/(g*d) (1), pg/mg (1), pmol/mg (1), 1e-09/L (1), umol/mg (1), counts/(counts*week) (1), ug/(100ul*counts) (1), mg/(kg*d) (1), umol/(mg*h) (1), cmol/kg (1), ug/L (1), kg/(100kg) (1), 1000000counts (1), ul (1), S (1), mmol/(h*g) (1), g/(70d) (1), ng/(200mg) (1), uS/cm^2 (1), ml/ha (1), pt/acre (1), mg/(h*g) (1), counts/m (1), kg/ton (1), ug/g (1), mg/(2L) (1), umol/g (1) 
#> 1 [ppm*d], 1 [ml/(2.5cm)], 1 [oz/(10gal*1000ft^2)], 1 [kg/(100L)], 1 [month], 1 [ng/kg], 1 [ug], 1 [ng/g], 1 [unit], 1 [unit], 1 [umol/(L*cm^3)], 1 [0.0001mmol/L], 1 [°], 1 [unit], 1 [unit], 1 [unit], 1 [ug/(g*d)], 1 [unit], 1 [pg/mg], 1 [pmol/mg], 1 [1e-09/L], 1 [unit], 1 [umol/mg], 1 [counts/(counts*week)], 1 [unit], 1 [ug/(100ul*counts)], 1 [mg/(kg*d)], 1 [umol/(mg*h)], 1 [cmol/kg], 1 [ug/L], 1 [kg/(100kg)], 1 [1000000counts], 1 [ul], 1 [S], 1 [mmol/(h*g)], 1 [g/(70d)], 1 [unit], 1 [ng/(200mg)], 1 [uS/cm^2], 1 [ml/ha], 1 [pt/acre], 1 [mg/(h*g)], 1 [counts/m], 1 [kg/ton], 1 [ug/g], 1 [mg/(2L)], 1 [unit], 1 [umol/g], 1 [unit], 1 [unit] 

## Adding the type of measurement can affect interpretation:
as_unit_ecotox(c("C", "K"), type = "concentration")
#> Mixed units: C (1), unit (1) 
#> 1 [C], 1 [unit] 
as_unit_ecotox(c("C", "K"), type = "media")
#> Mixed units: °C (1), K (1) 
#> 1 [°C], 1 [K] 
```
