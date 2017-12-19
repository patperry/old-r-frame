\name{keyset}
\alias{keyset}
\alias{as_keyset}
\alias{as_keyset.dataset}
\alias{as_keyset.default}
\alias{as_keyset.keyset}
\alias{is_keyset}
\title{Key Set Objects}
\description{
Create or test for key set objects.
}
\usage{
keyset(...)

as_keyset(x, ...)

is_keyset(x)
}
\arguments{
\item{\dots}{columns for \code{keyset}; further arguments passed to or
    from other methods for \code{as_keyset}.}

\item{x}{object to be converted or tested.}
}
\details{
These functions are used to create or test for a \code{keyset} object,
a \code{dataset} with vector columns of atomic type (logical, integer, double,
complex, or character) and unique rows. \code{keyset} objects can be used
as keys on \code{dataset} objects.

Key set objects use themselves as keys: if \code{x} is a \code{keyset}, then
\code{keys(x)} returns \code{x}. Consequently, you cannot set \code{keys(x)}
on a \code{keyset}.

The \code{keyset} function takes a set of columns as arguments and creates a
key set object with these columns as variables. The function has similar
semantics to \code{dataset}, except that matrix-like columns are not allowed.
Columns with types other than the atomic types listed above get converted to
character via \code{as.character} and then converted to UTF-8 encoding.
Character values that are not valid UTF-8 are not allowed.

\code{as_keyset} converts \code{x} to a keyset object. This is a generic
function with a default implementation that converts the result of
\code{as_dataset(x)} to a keyset.
    
\code{is_keyset} tests whether \code{x} is a valid keyset object.
}
\value{
\code{keyset} and \code{as_keyset} return a keyset objects.

\code{is_keyset} returns \code{TRUE} or \code{FALSE} depending on
    whether its argument is a valid keyset object or not.
}
\seealso{
\code{\link{dataset}}, \code{\link{keys}}.
}
\examples{
# 2-component keyset
(ks <- keyset(k1 = rep(c("a", "b", "c"), 2),
              k2 = rep(1:2, 3)))

# convert a dataset
ds <- dataset(name = LETTERS[6:12])
as_keyset(ds)

# error if rows are not unique
ds2 <- dataset(name = c("a", "b", "c", "a", "e"))
\dontrun{as_keyset(ds2)}

# test for keyset
is_keyset(ks)
is_keyset(ds)
}
\keyword{classes}
