ECOTOXr v1.0.0 (Release date: 2023-01-01)
-------------

  * Experimental features for on-line searching of
    ECOTOX and CompTox

  * Switched to roxygen2md for documentation and
    markdown format for NEWS

  * Added life-cycle badges

  * Some minor fixes in the SQLite building routine
    after the December 2022 release of ECOTOX

ECOTOXr v0.2.0 (Release date: 2022-11-17)
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

ECOTOXr v0.1.1 (Release date: 2021-10-04)
-------------

  * Updates to DESCRIPTION file and manual in order to
    comply with CRAN policies.
  
  * Function progress reports are now shown with 'message'
    instead of 'cat', such that they can be suppressed.

ECOTOXr v0.1.0 (Release date: 2021-10-03)
-------------

  * Initial release which can:

    * Download raw ECOTOX database tables from the EPA website
    * Build an SQLite database from those files
    * Search and extract data from the created local database