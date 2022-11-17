#' Package description
#'
#' Everything you need to know when you start using the ECOTOXr package.
#'
#' The ECOTOXr provides the means to efficiently search, extract and analyse \href{https://www.epa.gov/}{US EPA}
#' \href{https://cfpub.epa.gov/ecotox/}{ECOTOX} data, with a focus on reproducible results. Although the package
#' creator/maintainer is confident in the quality of this software, it is the end users sole responsibility to
#' assure the quality of his or her work while using this software. As per the provided license terms the package
#' maintainer is not liable for any damage resulting from its usage. That being said, below we present some tips
#' for generating reproducible results with this package.
#'
#' @section How do I get started?:
#' Installing this package is only the first step to get things started. You need to perform the following steps
#' in order to use the package to its full capacity.
#'
#' \itemize{
#' \item{
#' First download a copy of the complete EPA database. This can be done by calling \code{\link{download_ecotox_data}}.
#' This may not always work on all machines as R does not always accept the website SSL certificate from the EPA.
#' In those cases the zipped archive with the database files can be downloaded manually with a different (more
#' forgiving) browser. The files from the zip archive can be extracted to a location of choice. Alternatively,
#' the user could try to use \code{\link{download_ecotox_data}(ssl_verifypeer = 0L)} when the download URL is trusted.
#' }
#' \item{
#' Next, an SQLite database needs to be build from the downloaded files. This will be done automatically when
#' you used \code{\link{download_ecotox_data}} in the previous step. When you have manually downloaded the files
#' you can call \code{\link{build_ecotox_sqlite}} to build the database locally.
#' }
#' \item{
#' When the previous steps have been performed successfully, you can now search the database by calling
#' \code{\link{search_ecotox}}. You can also use \code{\link{dbConnectEcotox}} to open a connection to the
#' database. You can query the database using this connection and any of the methods provided from the
#' \link[DBI:DBI]{DBI} or \link[RSQLite:RSQLite]{RSQLite} packages.
#' }
#' }
#'
#' @section How do I obtain reproducible results?:
#' Each individual user is responsible for evaluating the reproducibility of his or her work. Although
#' this package offers instruments to achieve reproducibility, it is not guaranteed. In order to increase the
#' chances of generating reproducible results, one should adhere at least to the following rules:
#' \itemize{
#' \item{
#' Always use an official release from CRAN, and cite the version used in your analyses (\code{citation("ECOTOXr")}).
#' Different versions, may produce different end results (although we will strive for backward compatibility).
#' }
#' \item{
#' Make sure you are working with a clean (unaltered) version of the database. When in doubt, download and build
#' a fresh copy of the database (\code{\link{download_ecotox_data}}). Also cite the (release) version of the downloaded
#' database (\code{\link{cite_ecotox}}), and the system operating system in which the local database was build
#' \code{\link{get_ecotox_info}}). Or, just make sure that you never modify the database (e.g., write data to it, delete
#' data from it, etc.)
#' }
#' \item{
#' In order to avoid platform dependencies it is advised to only include non-accented alpha-numerical characters in
#' search terms. See also \link{search_ecotox} and \link{build_ecotox_sqlite}.
#' }
#' \item{
#' When trying to reproduce database extractions from earlier database releases, filter out additions after
#' that specific release. This can be done by adding output fields 'tests.modified_date', 'tests.created_date' and
#' 'tests.published_date' to your search and compare those with the release date of the database you are trying to
#' reproduce results from.
#' }
#' }
#'
#' @section Why isn't the database included in the package?:
#' This package doesn't come bundled with a copy of the database which needs to be downloaded the first time the
#' package is used. Why is this? There are several reasons:
#' \itemize{
#' \item{
#' The database is maintained and updated by the \href{https://www.epa.gov/}{US EPA}. This process is and should be
#' outside the sphere of influence of the package maintainer.
#' }
#' \item{
#' Packages on CRAN are not allowed to contain large amounts of data. Publication on CRAN is key to control
#' the quality of this package and therefore outweighs the convenience of having the data bundled with the package.
#' }
#' \item{
#' The user has full control over the release version of the database that is being used.
#' }
#' }
#'
#' @section Why doesn't this package search the online ECOTOX database?:
#' Although this is possible, there are several reasons why we opted for creating a local copy:
#' \itemize{
#' \item{
#' The user would be restricted to the search options provided on the website (\href{https://cfpub.epa.gov/ecotox/}{ECOTOX}).
#' }
#' \item{
#' The online database doesn't come with an API that would allow for convenient interface.
#' }
#' \item{
#' The user is not limited by an internet connection and its bandwidth.
#' }
#' \item{
#' Not all database fields can be retrieved from the online interface.
#' }
#' }
#' @docType package
#' @name ECOTOXr
#' @author Pepijn de Vries
#' @references
#' Official US EPA ECOTOX website:
#' \url{https://cfpub.epa.gov/ecotox/}
NULL
