\name{Extract.dataset}
\alias{[.dataset}
\alias{[[.dataset}
\alias{$.dataset}
\alias{[<-.dataset}
\alias{[[<-.dataset}
\alias{$<-.dataset}
\title{Data Set Indexing}
\description{
Extract a subset of a data set.
}
\usage{
\S3method{[}{dataset}(x, i, j, drop = FALSE)
\S3method{[[}{dataset}(x, i, exact = TRUE)
\S3method{$}{dataset}(x, name)

\S3method{[}{dataset}(x, i, j) <-  value
\S3method{[[}{dataset}(x, i) <-  value
\S3method{$}{dataset}(x, name) <- value
}
\arguments{
\item{x}{a data set object.}

\item{i, j}{indices specifying elements to extract or replace.}

\item{name}{column name.}

\item{drop}{logical indicating whether to return a column rather than a
    dataset when selecting a single column.}

\item{exact}{ignored.}

\item{value}{replacement column or set of values.}
}
\details{
For a \code{\link{dataset}} object \code{x}, the index operations
\code{x[i,j]} and \code{x[j]}$ behave like their counterparts for
\code{data.frame}, except that \code{drop = FALSE} is the default. Where the
index method differs is that it allows indexing by a matrix-like object
\code{i} as in \code{x[i,]}, selecting rows in \code{x} with keys matching
the rows in \code{i}.
}
\value{
A dataset with the specified rows and columns. Or, when selecting a single
row or column with \code{drop = TRUE}, just that row or column (or its
selected subset).  The result will have the same key components as \code{x}.
}
\seealso{
\code{\link{dataset}}, \code{\link{lookup}}, \code{\link{restrict}}.
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
}
