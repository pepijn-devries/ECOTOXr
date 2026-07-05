# Functions for handling chemical abstract service (CAS) registry numbers

**\[stable\]** Functions for handling chemical abstract service (CAS)
registry numbers

## Usage

``` r
cas(length = 0L)

is.cas(x)

as.cas(x)

# S3 method for class 'cas'
x[[i]]

# S3 method for class 'cas'
x[i]

# S3 method for class 'cas'
x[[i]] <- value

# S3 method for class 'cas'
x[i] <- value

# S3 method for class 'cas'
format(x, hyphenate = TRUE, ...)

# S3 method for class 'cas'
as.character(x, ...)

show.cas(x, ...)

# S3 method for class 'cas'
print(x, ...)

# S3 method for class 'cas'
as.list(x, ...)

# S3 method for class 'cas'
as.double(x, ...)

# S3 method for class 'cas'
as.integer(x, ...)

# S3 method for class 'cas'
c(...)

# S3 method for class 'cas'
as.data.frame(...)
```

## Arguments

- length:

  A non-negative `integer` specifying the desired length. Double values
  will be coerced to `integer`: supplying an argument of length other
  than one is an error.

- x:

  Object from which data needs to be extracted or replaced, or needs to
  be coerced into a specific format. For nearly all of the functions
  documented here, this needs to be an object of the S3 class 'cas',
  which can be created with `as.cas`. For `as.cas`, `x` can be a
  `character` (CAS registry number with or without hyphenation) or a
  `numeric` value. Note that `as.cas` will only accept correctly
  formatted and valid CAS registry numbers.

- i:

  Index specifying element(s) to extract or replace. See also
  [`base::Extract()`](https://rdrr.io/r/base/Extract.html).

- value:

  A replacement value, can be anything that can be converted into an S3
  cas-class object with `as.cas`.

- hyphenate:

  A `logical` value indicating whether the formatted CAS number needs to
  be hyphenated. Default is `TRUE`.

- ...:

  Arguments passed to other functions

## Value

Functions `cas`, `c` and `as.cas` return S3 class 'cas' objects.
Coercion functions (starting with 'as') return the object as specified
by their respective function names (i.e., `integer`, `double`,
`character`, `list` and `data.frame`). The `show.cas` and `print`
functions also return formatted `charater`s. The function `is.cas` will
return a single `logical` value, indicating whether `x` is a valid S3
cas-class object. The square brackets return the selected index/indices,
or the `vector` of cas objects where the selected elements are replaced
by `value`.

## Details

In the database [CAS
registry](https://en.wikipedia.org/wiki/Chemical_Abstracts_Service)
numbers are stored as text (type `character`). As CAS numbers can
consist of a maximum of 10 digits (plus two hyphens) this means that
each CAS number can consume up to 12 bytes of memory or disk space. By
storing the data numerically, only 5 bytes are required. These functions
provide the means to handle CAS registry numbers and coerce from and to
different formats and types.

## Author

Pepijn de Vries

## Examples

``` r
## This will generate a vector of cas objects containing 10
## fictive (0-00-0), but valid registry numbers:
cas(10)
#>  [1] "0-00-0" "0-00-0" "0-00-0" "0-00-0" "0-00-0" "0-00-0" "0-00-0" "0-00-0"
#>  [9] "0-00-0" "0-00-0"

## This is a cas-object:
is.cas(cas(0L))
#> [1] TRUE

## This is not a cas-object:
is.cas(0L)
#> [1] FALSE

## Three different ways of creating a cas object from
## Benzene's CAS registry number (the result is the same)
as.cas("71-43-2")
#> [1] "71-43-2"
as.cas("71432")
#> [1] "71-43-2"
as.cas(71432L)
#> [1] "71-43-2"

## This is one way of creating a vector with multiple CAS registry numbers:
cas_data <- as.cas(c("64175", "71432", "58082"))

## This is how you select a specific element(s) from the vector:
cas_data[2:3]
#> [1] "71-43-2" "58-08-2"
cas_data[[2]]
#> [1] "71-43-2"

## You can also replace specific elements in the vector:
cas_data[1] <- "7440-23-5"
cas_data[[2]] <- "129-00-0"

## You can format CAS numbers with or without hyphens:
format(cas_data, TRUE)
#> [1] "7440-23-5" "129-00-0"  "58-08-2"  
format(cas_data, FALSE)
#> [1] "7440235" "129000"  "58082"  

## The same can be achieved using as.character
as.character(cas_data, TRUE)
#> [1] "7440-23-5" "129-00-0"  "58-08-2"  
as.character(cas_data, FALSE)
#> [1] "7440235" "129000"  "58082"  

## There are also show and print methods available:
show(cas_data)
#> [1] "7440-23-5" "129-00-0"  "58-08-2"  
print(cas_data)
#> [1] "7440-23-5" "129-00-0"  "58-08-2"  

## Numeric values can be obtained from CAS using as.numeric, as.double or as.integer
as.numeric(cas_data)
#> [1] 7440235  129000   58082

## Be careful, however. Some CAS numbers cannot be represented by R's 32 bit integers
## and will produce NA's. This will work OK:
huge_cas <- as.cas("9999999-99-5")

if (FALSE) { # \dontrun{
## This will not:
as.integer(huge_cas)
} # }

## The trick applied by this package is that the final
## validation digit is stored separately as attribute:
unclass(huge_cas)
#> [1] 999999999
#> attr(,"checksum")
#> [1] 05

## This is how cas objects can be concatenated:
cas_data <- c(huge_cas, cas_data)

## This will create a data.frame
as.data.frame(cas_data)
#>          <cas>
#> 1 9999999-99-5
#> 2    7440-23-5
#> 3     129-00-0
#> 4      58-08-2

## This will create a list:
as.list(cas_data)
#> [[1]]
#> [1] "9999999-99-5"
#> 
#> [[2]]
#> [1] "7440-23-5"
#> 
#> [[3]]
#> [1] "129-00-0"
#> 
#> [[4]]
#> [1] "58-08-2"
#> 
```
