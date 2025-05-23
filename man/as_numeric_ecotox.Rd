% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/process_numeric.r
\name{as_numeric_ecotox}
\alias{as_numeric_ecotox}
\title{Values represented by ECOTOX \code{character} to \code{numeric}}
\usage{
as_numeric_ecotox(x, range_fun = NULL, ..., warn = TRUE)
}
\arguments{
\item{x}{A vector of \code{character} strings. It expects fields as commonly returned
from the ECOTOX database.}

\item{range_fun}{Function to summarise range values. If \code{NULL} range values are
returned as \code{NA}}

\item{...}{Arguments passed to \code{range_fun}.}

\item{warn}{If set to \code{FALSE} warnings while converting text to numerics are suppressed.}
}
\value{
A vector of \code{numeric} values with the same length as \code{x}.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}} Similar to \code{as.numeric()}, but it also
performs some text sanitising before coercing text to numerics.
}
\details{
The following steps are performed to sanitise text before coercing it to numerics:
\itemize{
\item Notes labelled with \code{"x"} or \code{"\\*"} are removed.
\item Operators (\code{">"}, \code{">="}, \code{"<"}, \code{"<="}, \code{"~"}, \code{"="}, \code{"ca"}, \code{"er"})
are removed.
\item Text between brackets (\code{"()"}) is removed (including the brackets)
\item Comma's are considered to be a thousand separator when they are located
at any fourth character (from the right) and removed. Comma's at any other location
is assumed to be a decimal separator and is replaced by a period.
\item If there is a hyphen present (not preceded by an "\code{"e"} or \code{"E"}) it is probably
representing a range of values. When \code{range_fun} is \code{NULL} it will result in a \code{NA}.
Otherwise, the numbers are split at the hyphen and aggregated with \code{range_fun}
}

It is your own responsibility to check if the sanitising steps are appropriate for
your analyses.
}
\examples{
## a vector of commonly used notations in the database to represent
## numeric values 
char_num <- c("10", " 2", "3 ", "~5", "9.2*", "2,33",
              "2,333", "2.1(1.0 - 3.2)", "1-5", "1e-3")

## Text fields reported as ranges are returned as `NA`:
as_numeric_ecotox(char_num, warn = FALSE)

## Text fields reported as ranges are processed with `range_fun`
as_numeric_ecotox(char_num, range_fun = median)
}
\seealso{
Other ecotox-sanitisers: 
\code{\link{as_date_ecotox}()},
\code{\link{as_unit_ecotox}()},
\code{\link{mixed_to_single_unit}()},
\code{\link{process_ecotox_dates}()},
\code{\link{process_ecotox_numerics}()},
\code{\link{process_ecotox_units}()}
}
\author{
Pepijn de Vries
}
\concept{ecotox-sanitisers}
