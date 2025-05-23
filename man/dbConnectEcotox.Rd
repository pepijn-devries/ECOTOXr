% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/database_access.r
\name{dbConnectEcotox}
\alias{dbConnectEcotox}
\alias{dbDisconnectEcotox}
\title{Open or close a connection to the local ECOTOX database}
\usage{
dbConnectEcotox(path = get_ecotox_path(), version, ...)

dbDisconnectEcotox(conn, ...)
}
\arguments{
\item{path}{A \code{character} string with the path to the location of the local database (default is
\code{\link[=get_ecotox_path]{get_ecotox_path()}}).}

\item{version}{A \code{character} string referring to the release version of the database you wish to locate.
It should have the same format as the date in the EPA download link, which is month, day, year, separated by
underscores ("\%m_\%d_\%Y"). When missing, the most recent available copy is selected automatically.}

\item{...}{Arguments that are passed to \code{\link[RSQLite:SQLite]{dbConnect()}} method
or \code{\link[RSQLite:SQLite]{dbDisconnect()}} method.}

\item{conn}{An open connection to the ECOTOX database that needs to be closed.}
}
\value{
A database connection in the form of a \code{\link[DBI:DBIConnection-class]{DBI::DBIConnection-class()}} object.
The object is tagged with: a time stamp; the package version used; and the
file path of the SQLite database used in the connection. These tags are added as attributes
to the object.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}} Wrappers for \code{\link[RSQLite:SQLite]{dbConnect()}} and
\code{\link[RSQLite:SQLite]{dbDisconnect()}} methods.
}
\details{
Open or close a connection to the local ECOTOX database. These functions are only required when you want
to send custom queries to the database. For most searches the \code{\link[=search_ecotox]{search_ecotox()}} function
will be adequate.
}
\examples{
## This will only work when a copy of the database exists:
if (check_ecotox_availability()) {
  con <- dbConnectEcotox()

  ## check if the connection works by listing the tables in the database:
  dbListTables(con)

  ## Let's be a good boy/girl and close the connection to the database when we're done:
  dbDisconnectEcotox(con)
}
}
\seealso{
Other database-access-functions: 
\code{\link{check_ecotox_availability}()},
\code{\link{check_ecotox_build}()},
\code{\link{check_ecotox_version}()},
\code{\link{cite_ecotox}()},
\code{\link{get_ecotox_info}()},
\code{\link{get_ecotox_sqlite_file}()},
\code{\link{list_ecotox_fields}()}
}
\author{
Pepijn de Vries
}
\concept{database-access-functions}
