\name{lookup}
\alias{lookup}
\alias{lookup.default}
\alias{keys<-}
\alias{keys<-.dataset}
\title{Data Keys}
\description{
Get or set the keys associated with a set of values.
}
\usage{
lookup(x, keys, default = NA_integer_, ...)
}
\arguments{
\item{x}{an object.}

\item{keys}{a key or set of keys to look up in \code{x}.}

\item{default}{a value to return when a key is not in \code{x}.}

\item{...}{further arguments passed to or from other methods.}
}
\details{
Look up the row indices in a data set \code{x} corresponding to a set
of keys. For keys that do not exist in \code{x}, return \code{default}
(or \code{trunc(default)} if \code{default} is not an integer).

When \code{keys(x)} has a single column, the \code{keys} argument can
be a vector and is coerced to the appropriate type to perform the lookup.
When \code{keys(x)} has multiple columns, the \code{keys} argument
gets coerced as \code{framed(keys)} and the number of columns must match.
Any row or column names in the \code{keys} argument get ignored, and columns
in \code{framed(keys)} get coerced to the appropriate types.
}
\value{
A vector of row indices in \code{x} for the corresponding rows in the
\code{keys} argument. The result will be integer-valued, with type
\code{integer} if \code{nrow(x)} and \code{default} have have magnitudes below
\code{.Machine$integer.max} and type \code{double} otherwise.
}
\seealso{
\code{\link{keys}}, \code{\link{[.dataset}}.
}
\examples{
# single-component key
x <- framed(mtcars)
lookup(x, c("Valiant", "Fiat 128", "Delorean"))

# multi-component keys
keys(x) <- dataset(k1 = rep(LETTERS[1:4], each = 8), k2 = rep(1:8, 4))

# look up a single item
lookup(x, list("A", 5))

# look up multiple items
lookup(x, list(c("A", "A", "B", "Z"),
               c(  5, 100,   1,   3)))
}
