\name{[.dataset}
\alias{[.dataset}
\title{Data Set Indexing}
\description{
Extract a subset of a data set.
}
\usage{
\S3method{[}{dataset}(x, ..., drop = FALSE)
}
\arguments{
\item{x}{a data set object.}
\item{...}{row or column selection (see below).}
\item{drop}{logical indicating whether to return a column rather than a
    dataset when selecting a single column.}
}
\details{
For a \code{\link{dataset}} object \code{x}, the index operations
\code{x[i,j]} and \code{x[j]}$ behave like their counterparts for
\code{data.frame}, except that \code{drop = FALSE} is the default. Where the
index method differs is that it allows indexing by a matrix-like object
\code{i} as in \code{x[i,]}, selecting rows in \code{x} with keys matching
the rows in \code{i}. Datasets with keys can also be indexed or sliced by
specified key components.
}
\section{Slicing by Key}{
Datasets with keys can be indexed with named arguments as in \code{x[k1 = ,]}
\code{x[k1 = , k2 = , ]}, and so on. In these instances, we select rows with
key components belonging to the sets specified by the arguments. If you
specify a single value for the key component, then that component gets dropped
from the key in the result. To prevent this behavior, put the specified
component in an \code{I()} expression.

As an alternative to using named arguments, you can specify the slice with a
list the same length as \code{keys(x)}. Here \code{NULL} components of the
list get ignored, and other components slice the key components in the
corresponding positions.
}
\value{
A dataset with the specified rows and columns. Or, when selecting a single
column with \code{drop = TRUE}, just that column or its selected rows. The
result will have the same key components as \code{x} unless slicing by key,
in which case the corresponding key component gets removed from the result.
}
\seealso{
\code{\link{dataset}}, \code{\link{lookup}}, \code{\link{keys}}.
}
\examples{
# data in wide format
value <- matrix(1:12, 4, 3, dimnames = list(c("a", "b", "c", "d"),
                                            c("x", "y", "z")))
# same data, in narrow format
x <- dataset(value = as.vector(value))
keys(x) <- list(row = rownames(value)[row(value)],
                col = colnames(value)[col(value)])
print(x)

# indexing with logical or numeric works like a data.frame:
x[c(2, 3, 4), ]
x[rep(c(TRUE, FALSE), 6), ]

# you can also index rows with a matrix of keys:
x[dataset(row = c("b", "d"), col = c("z", "x")), ]

# you can use a named argument to slice, dropping singleton keys;
# with named arguments, the trailing comma is optional
x[row = c("b", "d")]
x[row = c("b", "d"), col = "y"]
x[col = "x"]

# use I() to prevent dropping
x[col = I("x")]

# alternatively, slice by indexing with a list (trailing comma needed here)
x[list(c("b", "d"), "y"),]
x[list(c("b", "d"), I("y")),]
}