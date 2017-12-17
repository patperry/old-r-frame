\name{lookup}
\alias{lookup}
\alias{lookup.default}
\title{Data Key Lookup}
\description{
Find rows with specified keys.
}
\usage{
lookup(x, keys, default = NA_integer_, ...)
}
\arguments{
\item{x}{a key or set of keys to look up in the collection.}

\item{keys}{a collection of keys.}

\item{default}{a value to return when a key is not in \code{keys}.}

\item{...}{further arguments passed to or from other methods.}
}
\details{
Look up the row indices in a data set \code{keys} corresponding to a set of
keys in \code{x}. For keys that do not exist in \code{keys}, return
\code{default} (or \code{trunc(default)} if \code{default} is not an integer).

When \code{keys} has a single column, the \code{x} argument can
be a vector and is coerced to the appropriate type to perform the lookup.
When \code{keys} has multiple columns, the \code{x} argument
gets coerced as \code{framed(x)} and the number of columns must match.
Any row or column names in the \code{x} argument get ignored, and columns
in \code{x} get coerced to the appropriate types.
}
\value{
A vector of row indices in \code{keys} for the corresponding rows in the
\code{x} argument. The result will be integer-valued, with type
\code{integer} if \code{nrow(keys)} and \code{default} have have magnitudes below
\code{.Machine$integer.max} and type \code{double} otherwise.
}
\seealso{
\code{\link{keys}}, \code{\link{[.dataset}}.
}
\examples{
# single-component key
keys <- rownames(mtcars)
lookup(c("Valiant", "Fiat 128", "Delorean"), keys)

# multi-component keys
keys <- dataset(k1 = rep(LETTERS[1:4], each = 8), k2 = rep(1:8, 4))

# look up a single item
lookup(list("A", 5), keys)

# look up multiple items
lookup(list(c("A", "A", "B", "Z"),
            c(  5, 100,   1,   3)), keys)
}
