#' Functions for handling chemical abstract service (CAS) registry numbers
#'
#' `r lifecycle::badge('stable')` Functions for handling chemical abstract service (CAS) registry numbers
#' 
#' In the database [CAS registry](https://en.wikipedia.org/wiki/Chemical_Abstracts_Service) numbers are stored
#' as text (type `character`). As CAS numbers can consist of a maximum of 10 digits (plus two hyphens) this means
#' that each CAS number can consume up to 12 bytes of memory or disk space. By storing the data numerically, only
#' 5 bytes are required. These functions provide the means to handle CAS registry numbers and coerce from and to
#' different formats and types.
#' @param x Object from which data needs to be extracted or replaced, or needs to be coerced into a specific
#' format. For nearly all of the functions documented here, this needs to be an object of the S3 class 'cas',
#' which can be created with `as.cas`. For `as.cas`, `x` can be a `character` (CAS registry number
#' with or without hyphenation) or a `numeric` value. Note that `as.cas` will only accept correctly
#' formatted and valid CAS registry numbers.
#' @param i Index specifying element(s) to extract or replace. See also [base::Extract()].
#' @param value A replacement value, can be anything that can be converted into an S3 cas-class object with `as.cas`.
#' @param length A non-negative `integer` specifying the desired length. Double values will be coerced to
#' `integer`: supplying an argument of length other than one is an error.
#' @param hyphenate A `logical` value indicating whether the formatted CAS number needs to be hyphenated.
#' Default is `TRUE`.
#' @param ... Arguments passed to other functions
#' @returns Functions `cas`, `c` and `as.cas` return S3 class 'cas' objects. Coercion functions
#' (starting with 'as') return the object as specified by their respective function names (i.e., `integer`,
#' `double`, `character`, `list` and `data.frame`). The `show.cas` and `print` functions
#' also return formatted `charater`s. The function `is.cas` will return a single `logical` value,
#' indicating whether `x` is a valid S3 cas-class object. The square brackets return the selected index/indices,
#' or the `vector` of cas objects where the selected elements are replaced by `value`.
#' @rdname cas
#' @name cas 
#' @examples
#' ## This will generate a vector of cas objects containing 10
#' ## fictive (0-00-0), but valid registry numbers:
#' cas(10)
#' 
#' ## This is a cas-object:
#' is.cas(cas(0L))
#' 
#' ## This is not a cas-object:
#' is.cas(0L)
#' 
#' ## Three different ways of creating a cas object from
#' ## Benzene's CAS registry number (the result is the same)
#' as.cas("71-43-2")
#' as.cas("71432")
#' as.cas(71432L)
#' 
#' ## This is one way of creating a vector with multiple CAS registry numbers:
#' cas_data <- as.cas(c("64175", "71432", "58082"))
#' 
#' ## This is how you select a specific element(s) from the vector:
#' cas_data[2:3]
#' cas_data[[2]]
#' 
#' ## You can also replace specific elements in the vector:
#' cas_data[1] <- "7440-23-5"
#' cas_data[[2]] <- "129-00-0"
#' 
#' ## You can format CAS numbers with or without hyphens:
#' format(cas_data, TRUE)
#' format(cas_data, FALSE)
#' 
#' ## The same can be achieved using as.character
#' as.character(cas_data, TRUE)
#' as.character(cas_data, FALSE)
#' 
#' ## There are also show and print methods available:
#' show(cas_data)
#' print(cas_data)
#' 
#' ## Numeric values can be obtained from CAS using as.numeric, as.double or as.integer
#' as.numeric(cas_data)
#' 
#' ## Be careful, however. Some CAS numbers cannot be represented by R's 32 bit integers
#' ## and will produce NA's. This will work OK:
#' huge_cas <- as.cas("9999999-99-5")
#' 
#' \dontrun{
#' ## This will not:
#' as.integer(huge_cas)
#' }
#' 
#' ## The trick applied by this package is that the final
#' ## validation digit is stored separately as attribute:
#' unclass(huge_cas)
#' 
#' ## This is how cas objects can be concatenated:
#' cas_data <- c(huge_cas, cas_data)
#' 
#' ## This will create a data.frame
#' as.data.frame(cas_data)
#' 
#' ## This will create a list:
#' as.list(cas_data)
#' @author Pepijn de Vries
#' @export
cas <- function(length = 0L) {
  structure (
    integer(length),        ## The registry number is stored as integer without the final digit (=checksum)
    checksum = raw(length), ## last digit of CAS number, which serves as a checksum, stored as raw value
    class    = "cas"
  )
}

#' @rdname cas
#' @name is.cas
#' @export
is.cas <- function(x) {
  if (!(class(x) %in% "cas")) return(F)
  checksums <- attributes(x)$checksum
  if (length(checksums) != length(x)) stop("Each CAS registry in the vector needs a checksum")
  validate <- outer(unclass(x), 0:9, function(x, y) {
    floor(x/(10^y)) %% 10
  })
  validate <- apply(validate, 1, function(x) {
    x <- sum(seq_along(x)*x) %% 10
  })
  return(all(validate == as.numeric(checksums)))
}

#' @rdname cas
#' @name as.cas
#' @export
as.cas <- function(x) {
  if (is.cas(x)) return(x)
  x <- as.character(x)
  is_hyphenated    <- stringr::str_sub(x, -2, -2) == "-" & stringr::str_sub(x, -5, -5) == "-"
  x[is_hyphenated] <- paste0(
    stringr::str_sub(x[is_hyphenated], 1, -6),
    stringr::str_sub(x[is_hyphenated], -4, -3),
    stringr::str_sub(x[is_hyphenated], -1, -1)
  )
  if (any(!grepl("^[0-9]+$", x))) stop("CAS numbers can only contain hyphens at correct positions and numeric characters otherwise...")
  registry  <- as.integer(stringr::str_sub(x, 1, -2))
  registry[is.na(registry)] <- 0L
  attributes(registry)$checksum <- as.raw(as.integer(stringr::str_sub(x, -1, -1)))
  class(registry) <- "cas"
  if (!is.cas(registry)) stop("Input contains invalid CAS numbers")
  registry
}

#' @rdname cas
#' @name [[.cas
#' @export
`[[.cas` <- function(x, i) {
  attribs          <- attributes(x)
  attribs$checksum <- attribs$checksum[[i]]
  attribs$names    <- attribs$names[[i]]
  x                <- unclass(x)
  x                <- x[[i]]
  attributes(x)    <- attribs
  x
}

#' @rdname cas
#' @name [.cas
#' @export
`[.cas` <- function(x, i) {
  attribs          <- attributes(x)
  attribs$checksum <- attribs$checksum[i]
  attribs$names    <- attribs$names[i]
  x                <- unclass(x)
  x                <- x[i]
  attributes(x)    <- attribs
  x
}

#' @rdname cas
#' @name [[<-.cas
#' @export
`[[<-.cas` <- function(x, i, value) {
  value            <- as.cas(value)
  attribs          <- attributes(x)
  attribs$checksum[[i]] <- attributes(value)$checksum
  attribs$names[[i]]    <- attributes(value)$names
  x                <- unclass(x)
  x[[i]]           <- unclass(value)
  attributes(x)    <- attribs
  x
}

#' @rdname cas
#' @name [<-.cas
#' @export
`[<-.cas` <- function(x, i, value) {
  value            <- as.cas(value)
  attribs          <- attributes(x)
  attribs$checksum[i] <- attributes(value)$checksum
  attribs$names[i]    <- attributes(value)$names
  x                <- unclass(x)
  x[i]             <- unclass(value)
  attributes(x)    <- attribs
  x
}

#' @rdname cas
#' @name format.cas
#' @export
format.cas <- function(x, hyphenate = TRUE, ...) {
  checksums <- attributes(x)$checksum
  x <- unclass(x)
  repp <- x
  repp[repp == 0] <- 1
  repp <- ceiling(2 - log10(repp))
  repp[repp < 0] <- 0
  x <- paste0(strrep("0", repp), x)
  sprintf("%s%s%s%s%01i",
          stringr::str_sub(x, 1, -3),
          ifelse(hyphenate, "-", ""),
          stringr::str_sub(x, -2, -1),
          ifelse(hyphenate, "-", ""),
          as.numeric(checksums)
  )
}

#' @rdname cas
#' @name as.character.cas
#' @export
as.character.cas <- function(x, ...) {
  format(x, ...)
}

#' @rdname cas
#' @name show.cas
#' @export
show.cas <- function(x, ...) {
  format(x, ...)
}

#' @rdname cas
#' @name print.cas
#' @export
print.cas <- function(x, ...) {
  if (length(x) == 0)
    cat("cas(0)\n") else print(format.cas(x), ...)
}

#' @rdname cas
#' @name as.list.cas
#' @export
as.list.cas <- function(x, ...) {
  lapply(seq_along(x), function(i) x[i])
}

#' @rdname cas
#' @name as.double.cas
#' @export
as.double.cas <- function(x, ...) {
  as.double(as.integer.cas(x, ...), ...)
}

#' @rdname cas
#' @name as.integer.cas
#' @export
as.integer.cas <- function(x, ...) {
  checksums <- as.integer(attributes(x)$checksum, ...)
  x <- 10L*unclass(x)
  attributes(x) <- NULL
  x + checksums
}

#' @rdname cas
#' @name c.cas
#' @export
c.cas <- function(...) {
  result        <- list(...)
  result        <- lapply(result, as.cas)
  checksums     <- do.call(c, lapply(result, function(x) attributes(x)$checksum))
  result        <- do.call(c, lapply(result, function(x) unclass(x)))
  class(result) <- "cas"
  attributes(result)$checksum <- checksums
  result
}

#' @rdname cas
#' @name as.data.frame.cas
#' @export
as.data.frame.cas <- function(...) {
  as.data.frame(tibble::tibble(...))
}
