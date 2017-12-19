\name{lookup}
\alias{lookup}
\alias{rowid}
\alias{rowid.dataset}
\alias{rowid.default}
\alias{rowid.keyset}
\title{Data Key Lookup}
\description{
Find rows with specified keys.
}
\usage{
lookup(keys, x, default = NA_integer_, ...)

rowid(x, keys, default = NA_integer_, ...)
}
\arguments{
\item{keys}{a key or set of keys to look up in the object.}

\item{x}{an object with a key for each row.}

\item{default}{a value to return when no row in \code{x} has the specified
    key.}

\item{...}{further arguments passed to or from other methods.}
}
\details{
Look up the row indices in a data set \code{keys} corresponding to a set of
keys in \code{x}. For keys that do not exist in \code{keys}, return
\code{default} (or \code{trunc(default)} if \code{default} is not an integer).

When the rows in \code{x} have single-component keys, the \code{keys} argument
can be a vector and is coerced to the appropriate type to perform the lookup.
With multi-component keys, the \code{keys} argument gets coerced with
\code{as_dataset} and the number of columns must match.  Any row or column
names in the \code{keys} argument get ignored, and columns in \code{keys} get
coerced to the appropriate types.

\code{lookup} calls \code{rowid} with its arguments swapped.

\code{rowid} function is a generic function with implementations for
\code{dataset} and \code{keyset}.
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
x <- as_dataset(mtcars)
lookup(c("Valiant", "Fiat 128", "Delorean"), x)

# rowid is similar, but has arguments swapped
rowid(x, c("Valiant", "Fiat 128", "Delorean"))

# multi-component keys
keys <- keyset(k1 = rep(LETTERS[1:4], each = 8), k2 = rep(1:8, 4))

# look up a single item
lookup(list("A", 5), keys)

# look up multiple items
lookup(list(c("A", "A", "B", "Z"),
            c(  5, 100,   1,   3)), keys)
}
