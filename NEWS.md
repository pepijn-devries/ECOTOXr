ECOTOXr v1.1.0.0001
-------------

  * Added `pkgdown` generated website
  * Updated documentation

ECOTOXr v1.0.9
-------------

  * switched from the 'magrittr' pipe operator ('%>%')
    to R's native pipe operator ('|>')
  * switched from 'httr' to 'httr2' dependency
  * Added functions to check the local build
    and its version
  * Fix in database build routine
  * Fix in manual to pass CRAN checks
  * Explicit mentioning of required versions
    of dependencies

ECOTOXr v1.0.5
-------------

  * Added verify_ssl argument to on-line
    functions in order to make it easier to
    try and avert failing certificate checks.
    
  * Minor fix in CAS handlers

ECOTOXr v1.0.3
-------------

  * Adjustments to pass latest CRAN checks

ECOTOXr v1.0.2
-------------

  * Fixes for 2023 June release of the database:
  
    * Handling of new fields and tables introduced
      in the new release.
  
    * Better handler unintended line-feed characters
      in source material, when building SQLite file.
    
    * When table primary key is not unique, update
      table with latest occurrence of the key.
      This is also reported to EPA.
  
  * Keeping track of missing tables and fields, and
    occurrence of unexpected fields while building.
    These are added to the build log file.

ECOTOXr v1.0.1
-------------

  * Experimental features for on-line searching of
    ECOTOX and CompTox

  * Switched to roxygen2md for documentation and
    markdown format for NEWS

  * Added life-cycle badges

  * Some minor fixes in the SQLite building routine
    after the December 2022 release of ECOTOX

ECOTOXr v0.2.0
-------------

  * Major changes:

    * Modified searching routines to make advantage of
      the sql parser and optimisers implemented in
      the package 'dplyr'. Performance (i.e. speed) of the
      search routines have improved considerably.
    * Added support for handling Chemical Abstracts
      Service (CAS) numbers.

  * Several minor adjustments and corrections to code
    and manual. These include fixes to address notes
    from CRAN checks.

ECOTOXr v0.1.1
-------------

  * Updates to DESCRIPTION file and manual in order to
    comply with CRAN policies.
  
  * Function progress reports are now shown with 'message'
    instead of 'cat', such that they can be suppressed.

ECOTOXr v0.1.0
-------------

  * Initial release which can:

    * Download raw ECOTOX database tables from the EPA website
    * Build an SQLite database from those files
    * Search and extract data from the created local database
