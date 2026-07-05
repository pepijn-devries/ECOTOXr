# Build an SQLite database from zip archived tables downloaded from EPA website

**\[stable\]** This function is called automatically after
[`download_ecotox_data()`](https://pepijn-devries.github.io/ECOTOXr/reference/download_ecotox_data.md).
The database files can also be downloaded manually from the [EPA
website](https://cfpub.epa.gov/ecotox/) from which a local database can
be build using this function.

## Usage

``` r
build_ecotox_sqlite(source, destination = get_ecotox_path(), write_log = TRUE)
```

## Arguments

- source:

  A `character` string pointing to the directory path where the text
  files with the raw tables are located. These can be obtained by
  extracting the zip archive from <https://cfpub.epa.gov/ecotox/> and
  look for 'Download ASCII Data'.

- destination:

  A `character` string representing the destination path for the SQLite
  file. By default this is
  [`get_ecotox_path()`](https://pepijn-devries.github.io/ECOTOXr/reference/get_path.md).

- write_log:

  A `logical` value indicating whether a log file should be written in
  the destination path `TRUE`. The log contains information on the
  source and destination path, the version of this package, the creation
  date, and the operating system on which the database was created.

## Value

Returns `NULL` invisibly.

## Details

Raw data downloaded from the EPA website is in itself not very efficient
to work with in R. The files are large and would put a large strain on R
when loading completely into the system's memory. Instead use this
function to build an SQLite database from the tables. That way, the data
can be queried without having to load it all into memory.

EPA provides the raw table from the [ECOTOX
database](https://cfpub.epa.gov/ecotox/) as text files with
pipe-characters ('\|') as table column separators. Although not
documented, the tables appear not to contain comment or quotation
characters. There are records containing the reserved pipe-character
that will confuse the table parser. For these records, the
pipe-character is replaced with a dash character ('-').

In addition, while reading the tables as text files, this package
attempts to decode the text as UTF8. Unfortunately, this process appears
to be platform-dependent, and may therefore result in different
end-results on different platforms. This problem only seems to occur for
characters that are listed as 'control characters' under UTF8. This will
have consequences for reproducibility, but only if you build search
queries that look for such special characters. It is therefore advised
to stick to common (non-accented) alpha-numerical characters in your
searches, for the sake of reproducibility.

Use '[`suppressMessages()`](https://rdrr.io/r/base/message.html)' to
suppress the progress report.

## See also

Other database-build-functions:
[`check_ecotox_build()`](https://pepijn-devries.github.io/ECOTOXr/reference/check_ecotox_build.md),
[`check_ecotox_version()`](https://pepijn-devries.github.io/ECOTOXr/reference/check_ecotox_version.md),
[`download_ecotox_data()`](https://pepijn-devries.github.io/ECOTOXr/reference/download_ecotox_data.md),
[`get_ecotox_url()`](https://pepijn-devries.github.io/ECOTOXr/reference/get_ecotox_url.md)

## Author

Pepijn de Vries

## Examples

``` r
source_path <- tempfile()
dir.create(source_path)

## This is a small mockup file resembling the larger zip
## files that can be downloaded with `download_ecotox_data()`:

source_file <- system.file("ecotox-test.zip", package = "ECOTOXr")

unzip(source_file, exdir = source_path)

build_ecotox_sqlite(source_path, tempdir())
#> Adding 'application_frequency_codes' table (1/50) to database:
#> 
#>   0 lines (incl. header) of 'application_frequency_codes' added to database
#>  230 lines (incl. header) of 'application_frequency_codes' added to database
#>  Done
#> 
#> Adding 'application_type_codes' table (2/50) to database:
#> 
#>   0 lines (incl. header) of 'application_type_codes' added to database
#>  22 lines (incl. header) of 'application_type_codes' added to database
#>  Done
#> 
#> Adding 'chemical_analysis_codes' table (3/50) to database:
#> 
#>   0 lines (incl. header) of 'chemical_analysis_codes' added to database
#>  10 lines (incl. header) of 'chemical_analysis_codes' added to database
#>  Done
#> 
#> Adding 'chemical_carriers' table (4/50) to database:
#> 
#>   0 lines (incl. header) of 'chemical_carriers' added to database
#>  5 lines (incl. header) of 'chemical_carriers' added to database
#>  Done
#> 
#> Adding 'chemical_formulation_codes' table (5/50) to database:
#> 
#>   0 lines (incl. header) of 'chemical_formulation_codes' added to database
#>  89 lines (incl. header) of 'chemical_formulation_codes' added to database
#>  Done
#> 
#> Adding 'chemical_grade_codes' table (6/50) to database:
#> 
#>   0 lines (incl. header) of 'chemical_grade_codes' added to database
#>  113 lines (incl. header) of 'chemical_grade_codes' added to database
#>  Done
#> 
#> Adding 'chemicals' table (7/50) to database:
#> 
#>   0 lines (incl. header) of 'chemicals' added to database
#>  23 lines (incl. header) of 'chemicals' added to database
#>  Done
#> 
#> Adding 'concentration_type_codes' table (8/50) to database:
#> 
#>   0 lines (incl. header) of 'concentration_type_codes' added to database
#>  11 lines (incl. header) of 'concentration_type_codes' added to database
#>  Done
#> 
#> Adding 'concentration_unit_codes' table (9/50) to database:
#> 
#>   0 lines (incl. header) of 'concentration_unit_codes' added to database
#>  20 lines (incl. header) of 'concentration_unit_codes' added to database
#>  Done
#> 
#> Adding 'control_type_codes' table (10/50) to database:
#> 
#>   0 lines (incl. header) of 'control_type_codes' added to database
#>  16 lines (incl. header) of 'control_type_codes' added to database
#>  Done
#> 
#> Adding 'dose_response_details' table (11/50) to database:
#> 
#>   0 lines (incl. header) of 'dose_response_details' added to database
#>  1 lines (incl. header) of 'dose_response_details' added to database
#>  Done
#> 
#> Adding 'dose_response_links' table (12/50) to database:
#> 
#>   0 lines (incl. header) of 'dose_response_links' added to database
#>  1 lines (incl. header) of 'dose_response_links' added to database
#>  Done
#> 
#> Adding 'dose_responses' table (13/50) to database:
#> 
#>   0 lines (incl. header) of 'dose_responses' added to database
#>  1 lines (incl. header) of 'dose_responses' added to database
#>  Done
#> 
#> Adding 'dose_stat_method_codes' table (14/50) to database:
#> 
#>   0 lines (incl. header) of 'dose_stat_method_codes' added to database
#>  10 lines (incl. header) of 'dose_stat_method_codes' added to database
#>  Done
#> 
#> Adding 'doses' table (15/50) to database:
#> 
#>   0 lines (incl. header) of 'doses' added to database
#>  1 lines (incl. header) of 'doses' added to database
#>  Done
#> 
#> Adding 'duration_unit_codes' table (16/50) to database:
#> 
#>   0 lines (incl. header) of 'duration_unit_codes' added to database
#>  175 lines (incl. header) of 'duration_unit_codes' added to database
#>  Done
#> 
#> Adding 'effect_codes' table (17/50) to database:
#> 
#>   0 lines (incl. header) of 'effect_codes' added to database
#>  49 lines (incl. header) of 'effect_codes' added to database
#>  Done
#> 
#> Adding 'endpoint_assigned_codes' table (18/50) to database:
#> 
#>   0 lines (incl. header) of 'endpoint_assigned_codes' added to database
#>  7 lines (incl. header) of 'endpoint_assigned_codes' added to database
#>  Done
#> 
#> Adding 'endpoint_codes' table (19/50) to database:
#> 
#>   0 lines (incl. header) of 'endpoint_codes' added to database
#>  8 lines (incl. header) of 'endpoint_codes' added to database
#>  Done
#> 
#> Adding 'exposure_type_codes' table (20/50) to database:
#> 
#>   0 lines (incl. header) of 'exposure_type_codes' added to database
#>  117 lines (incl. header) of 'exposure_type_codes' added to database
#>  Done
#> 
#> Adding 'field_study_type_codes' table (21/50) to database:
#> 
#>   0 lines (incl. header) of 'field_study_type_codes' added to database
#>  11 lines (incl. header) of 'field_study_type_codes' added to database
#>  Done
#> 
#> Adding 'gender_codes' table (22/50) to database:
#> 
#>   0 lines (incl. header) of 'gender_codes' added to database
#>  7 lines (incl. header) of 'gender_codes' added to database
#>  Done
#> 
#> Adding 'geographic_codes' table (23/50) to database:
#> 
#>   0 lines (incl. header) of 'geographic_codes' added to database
#>  249 lines (incl. header) of 'geographic_codes' added to database
#>  Done
#> 
#> Adding 'habitat_codes' table (24/50) to database:
#> 
#>   0 lines (incl. header) of 'habitat_codes' added to database
#>  13 lines (incl. header) of 'habitat_codes' added to database
#>  Done
#> 
#> Adding 'ion_codes' table (25/50) to database:
#> 
#>   0 lines (incl. header) of 'ion_codes' added to database
#>  187 lines (incl. header) of 'ion_codes' added to database
#>  Done
#> 
#> Adding 'length_type_codes' table (26/50) to database:
#> 
#>   0 lines (incl. header) of 'length_type_codes' added to database
#>  13 lines (incl. header) of 'length_type_codes' added to database
#>  Done
#> 
#> Adding 'length_unit_codes' table (27/50) to database:
#> 
#>   0 lines (incl. header) of 'length_unit_codes' added to database
#>  7 lines (incl. header) of 'length_unit_codes' added to database
#>  Done
#> 
#> Adding 'lifestage_codes' table (28/50) to database:
#> 
#>   0 lines (incl. header) of 'lifestage_codes' added to database
#>  139 lines (incl. header) of 'lifestage_codes' added to database
#>  Done
#> 
#> Adding 'measurement_codes' table (29/50) to database:
#> 
#>   0 lines (incl. header) of 'measurement_codes' added to database
#>  10 lines (incl. header) of 'measurement_codes' added to database
#>  Done
#> 
#> Adding 'media_char_unit_codes' table (30/50) to database:
#> 
#>   0 lines (incl. header) of 'media_char_unit_codes' added to database
#>  172 lines (incl. header) of 'media_char_unit_codes' added to database
#>  Done
#> 
#> Adding 'media_characteristics' table (31/50) to database:
#> 
#>   0 lines (incl. header) of 'media_characteristics' added to database
#>  21 lines (incl. header) of 'media_characteristics' added to database
#>  Done
#> 
#> Adding 'media_type_codes' table (32/50) to database:
#> 
#>   0 lines (incl. header) of 'media_type_codes' added to database
#>  25 lines (incl. header) of 'media_type_codes' added to database
#>  Done
#> 
#> Adding 'organic_matter_type_codes' table (33/50) to database:
#> 
#>   0 lines (incl. header) of 'organic_matter_type_codes' added to database
#>  18 lines (incl. header) of 'organic_matter_type_codes' added to database
#>  Done
#> 
#> Adding 'organism_source_codes' table (34/50) to database:
#> 
#>   0 lines (incl. header) of 'organism_source_codes' added to database
#>  13 lines (incl. header) of 'organism_source_codes' added to database
#>  Done
#> 
#> Adding 'radio_label_codes' table (35/50) to database:
#> 
#>   0 lines (incl. header) of 'radio_label_codes' added to database
#>  91 lines (incl. header) of 'radio_label_codes' added to database
#>  Done
#> 
#> Adding 'references' table (36/50) to database:
#> 
#>   0 lines (incl. header) of 'references' added to database
#>  21 lines (incl. header) of 'references' added to database
#>  Done
#> 
#> Adding 'response_site_codes' table (37/50) to database:
#> 
#>   0 lines (incl. header) of 'response_site_codes' added to database
#>  609 lines (incl. header) of 'response_site_codes' added to database
#>  Done
#> 
#> Adding 'results' table (38/50) to database:
#> 
#>   0 lines (incl. header) of 'results' added to database
#>  21 lines (incl. header) of 'results' added to database
#>  Done
#> 
#> Adding 'sample_size_unit_codes' table (39/50) to database:
#> 
#>   0 lines (incl. header) of 'sample_size_unit_codes' added to database
#>  138 lines (incl. header) of 'sample_size_unit_codes' added to database
#>  Done
#> 
#> Adding 'season_codes' table (40/50) to database:
#> 
#>   0 lines (incl. header) of 'season_codes' added to database
#>  8 lines (incl. header) of 'season_codes' added to database
#>  Done
#> 
#> Adding 'species' table (41/50) to database:
#> 
#>   0 lines (incl. header) of 'species' added to database
#>  19 lines (incl. header) of 'species' added to database
#>  Done
#> 
#> Adding 'species_synonyms' table (42/50) to database:
#> 
#>   0 lines (incl. header) of 'species_synonyms' added to database
#>  18 lines (incl. header) of 'species_synonyms' added to database
#>  Done
#> 
#> Adding 'statistical_significance_codes' table (43/50) to database:
#> 
#>   0 lines (incl. header) of 'statistical_significance_codes' added to database
#>  9 lines (incl. header) of 'statistical_significance_codes' added to database
#>  Done
#> 
#> Adding 'substrate_codes' table (44/50) to database:
#> 
#>   0 lines (incl. header) of 'substrate_codes' added to database
#>  12 lines (incl. header) of 'substrate_codes' added to database
#>  Done
#> 
#> Adding 'test_location_codes' table (45/50) to database:
#> 
#>   0 lines (incl. header) of 'test_location_codes' added to database
#>  7 lines (incl. header) of 'test_location_codes' added to database
#>  Done
#> 
#> Adding 'test_method_codes' table (46/50) to database:
#> 
#>   0 lines (incl. header) of 'test_method_codes' added to database
#>  56 lines (incl. header) of 'test_method_codes' added to database
#>  Done
#> 
#> Adding 'test_type_codes' table (47/50) to database:
#> 
#>   0 lines (incl. header) of 'test_type_codes' added to database
#>  14 lines (incl. header) of 'test_type_codes' added to database
#>  Done
#> 
#> Adding 'tests' table (48/50) to database:
#> 
#>   0 lines (incl. header) of 'tests' added to database
#>  21 lines (incl. header) of 'tests' added to database
#>  Done
#> 
#> Adding 'trend_codes' table (49/50) to database:
#> 
#>   0 lines (incl. header) of 'trend_codes' added to database
#>  8 lines (incl. header) of 'trend_codes' added to database
#>  Done
#> 
#> Adding 'weight_unit_codes' table (50/50) to database:
#> 
#>   0 lines (incl. header) of 'weight_unit_codes' added to database
#>  9 lines (incl. header) of 'weight_unit_codes' added to database
#>  Done
#> 
```
