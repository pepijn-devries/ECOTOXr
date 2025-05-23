% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/database_access.r
\name{list_ecotox_fields}
\alias{list_ecotox_fields}
\title{List the field names that are available from the ECOTOX database}
\usage{
list_ecotox_fields(
  which = c("default", "extended", "full", "all"),
  include_table = TRUE
)
}
\arguments{
\item{which}{A \code{character} string that specifies which fields to return. Can be any of:
'\code{default}': returns default output field names; '\code{all}': returns all fields;
'\code{extended}': returns all fields of the default tables; or
'\code{full}': returns all fields except those from tables 'chemical_carriers',
'media_characteristics', 'doses', 'dose_responses',
'dose_response_details', 'dose_response_links' and 'dose_stat_method_codes'.}

\item{include_table}{A \code{logical} value indicating whether the table name should be included
as prefix. Default is \code{TRUE}.}
}
\value{
Returns a \code{vector} of type \code{character} containing the field names from the ECOTOX database.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}} List the field names (table headers) that are available from the ECOTOX database
}
\details{
This can be useful when specifying a \code{\link[=search_ecotox]{search_ecotox()}}, to identify which fields
are available from the database, for searching and output.

Not that when requesting '\code{all}' fields, you will get all fields available from the
latest EPA release of the ECOTOX database. This means that not necessarily all
fields are available in your local build of the database.
}
\examples{
## Fields that are included in search results by default:
list_ecotox_fields("default")

## All fields that are available from the ECOTOX database:
list_ecotox_fields("all")

## All except fields from the tables 'chemical_carriers', 'media_characteristics',
## 'doses', 'dose_responses', 'dose_response_details', 'dose_response_links' and
## 'dose_stat_method_codes' that are available from the ECOTOX database:
list_ecotox_fields("full")
}
\seealso{
Other database-access-functions: 
\code{\link{check_ecotox_availability}()},
\code{\link{check_ecotox_build}()},
\code{\link{check_ecotox_version}()},
\code{\link{cite_ecotox}()},
\code{\link{dbConnectEcotox}()},
\code{\link{get_ecotox_info}()},
\code{\link{get_ecotox_sqlite_file}()}
}
\author{
Pepijn de Vries
}
\concept{database-access-functions}
