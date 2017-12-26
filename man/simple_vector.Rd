\name{simple_vector}
\alias{simple_vector}
\alias{as_simple_vector}
\alias{as_simple_vector.default}
\alias{is_simple_vector}
\title{Simple Vector Objects}
\description{
Create or test for simple vector objects.
}
\usage{
as_simple_vector(x, like = NULL, ...)

is_simple_vector(x)
}
\arguments{
\item{x}{object to be converted or tested.}

\item{like}{object to cast like, or \code{NULL}.}

\item{\dots}{further arguments passed to or from other methods.}
}
\details{
A simple vector is a vector (\code{dim(x) == NULL}) of one of the
following types:

\itemize{
  \item \code{logical};
  \item \code{integer};
  \item \code{numeric} (\code{double});
  \item \code{complex};
  \item \code{character} in \code{"UTF-8"} encoding;
  \item \code{Date};
  \item \code{POSIXct} in any time zone.
}

These are the only types allowed for key sets. Other types get converted
to simple via \code{as.logical()}, \code{as.integer()}, etc. For character
conversion, we call \code{utf8::as_utf8()} on the result to convert to UTF-8
encoding, throwing an error for invalid encoded values.

Note that \code{factor} types are not considered simple;
\code{as_simple_vector} will convert factor values to character.

\code{as_simple_vector} function converts \code{x} to a simple vector of the
appropriate type.  If \code{like} is \code{NULL}, then the result defaults
to whatever simple type is most natural; otherwise, the type will be the
same as the \code{like} argument, including the same \code{tzone} attribute
if one is set.

When \code{like} is non-\code{NULL}, it may not be possible to convert all
elements of \code{x} to the requested type. When this is the case, those
elements get set to \code{NA} and the result contains a \code{\"failed\"}
attribute containing the indices of the failed conversions.

\code{is_simple_vector} tests whether or not \code{x} is simple.
}
\section{Date and time conversions}{
When converting to \code{Date} or \code{POSIXct}, we call \code{as.Date}
or \code{as.POSIXct}, respectively. Both of these conversions require a
\code{tz} or an \code{origin} argument, or both.

For the origin we use \code{"1970-01-01"}; this get interpreted as
\code{"1970-01-01 00:00:00 UTC"}.

For conversion to \code{Date} or conversion to \code{POSIXct} with
\code{like = NULL}, we use \code{attr(x, "tzone")[[1]]} for the
\code{tz} argument if non-\code{NULL} and \code{"UTC"} otherwise. For
conversion to \code{POSIXct} with non-\code{NULL} like, we default to
\code{attr(like, "tzone")[[1]]} instead of \code{"UTC"}. Then, we set
the \code{"tzone"} attribute of the result to \code{attr(like, "tzone")}.
}
\value{
\code{as_simple_vector} returns \code{x} converted to a simple vector. When
\code{like} is non-\code{NULL}, if any of the conversions fail, then the
result \code{attr(, "failed")} will be set to the indices of the failed
conversions.

\code{is_simple_vector} returns \code{TRUE} or \code{FALSE} depending on
whether its argument is a simple vector.
}
\seealso{
\code{\link{as_dataset}}.
}
\examples{
## Numeric and integer
x <- c(1, 1.3, .Machine$integer.max + 1, 23, NaN, Inf)
is_simple_vector(x)
identical(x, as_simple_vector(x))

as_simple_vector(x) # fails for entries 3 and 6; succeeds for rest

## Factors
f <- factor(c("5", "4", "3", "2", "1"), c(1, 2, 5, 3, 4))

# factor is not simple
is_simple_vector(f)

# by default, convert factor to character, not integer
identical(as_simple_vector(f), as.character(f))

# with 'like', convert factor via labels, not levels
as_simple_vector(f, integer())
as.integer(f) # uses levels


## Dates and times
ct <- as.POSIXct("2017-12-25 17:23:45", tz = "America/Los_Angeles")
lt <- as.POSIXlt(ct)

# POSIXct is simple, POSIXlt is not
is_simple_vector(ct)
is_simple_vector(lt)

# by default convert to POSIXct, preserving time zone
identical(ct, as_simple_vector(ct)) # from POSIXct (unchanged)
identical(ct, as_simple_vector(lt)) # from POSIXlt

# use own time zone when converting to date
as_simple_vector(ct, as.Date(NA))
as_simple_vector(lt, as.Date(NA))
as.Date(ct) # uses UTC
as.Date(lt) # uses own time zone

# use 'like' time zone when converting to date or time
as_simple_vector("2017-07-04 15:00:12", ct)
as.POSIXct("2017-07-04 15:00:12") # uses local time zone

# use '1970-01-01 00:00:00 UTC' as origin for numeric
as_simple_vector(as.numeric(ct), ct)
}
