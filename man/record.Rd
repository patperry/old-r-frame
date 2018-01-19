\name{record}
\alias{record}
\alias{as_record}
\alias{as_record.default}
\alias{as_record.list}
\alias{as_record.record}
\alias{is_record}
\title{Record Objects}
\description{
Create or test for record objects.
}
\usage{
record(...)

as_record(x, names = NULL, ...)

is_record(x)
}
\arguments{
\item{\dots}{columns for \code{record}; further arguments passed to or
    from other methods for \code{as_record}.}

\item{x}{object to be converted or tested.}

\item{names}{field names for the converted object, or \code{NULL} to
    preserve existing names.}
}
\details{
These functions are used to create or test for a \code{record} object,
essentially a list. There are some crucial differences between
\code{record} and \code{list} objects:

\enumerate{
    \item when accessing fields by name, record objects do not
        allow partial matching;

    \item record objects always have valid UTF-8 encoded names
        (code or \code{NULL});

    \item record objects have more reliable indexing with empty
       and \code{NA} field names.
}

Other than that, records and lists have identical behavior.

The \code{record} function takes a set of fields as arguments and creates a
record object with these fields. When argument names are missing, the
result creates names from the call. If the \code{names} argument is
non-\code{NULL}, the \code{as_record} sets the names of the result to this
value.

\code{as_record} converts \code{x} to a record object. This is a generic
function with a default implementation that converts the result of
\code{as.list(x)} to a record.
    
\code{is_record} tests whether \code{x} is a record object.
}
\value{
\code{record} and \code{as_record} return a record objects.

\code{is_record} returns \code{TRUE} or \code{FALSE} depending on whether
    its argument is a record object.
}
\seealso{
\code{\link{dataset}}, \code{\link{record}}, \code{link{[[.dataset}}.
}
\examples{
# create a record
record(a = 10, b = "hello", c = 3.14)

# take argument names from the call
x <- 19
y <- 98.6
record(x, y)

# convert an object
as_record(list(foo = 10, bar = "party"))

# convert an object, setting new names
as_record(list(foo = 10, bar = "party"), names = c("zzz", "yyy"))

# test for records
is_record(record(x, y))
is_record(list(x, y))
}
\keyword{classes}
