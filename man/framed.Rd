\name{framed}
\alias{framed}
\alias{framed.data.frame}
\alias{framed.dataset}
\alias{framed.default}
\alias{framed.list}
\alias{framed.NULL}
\title{Framed Variables}
\description{
Convert a set of values to a dataset that has keys identifying the rows.
}
\usage{
framed(x, keys = NULL, ...)
}
\arguments{
\item{x}{object to be converted.}

\item{keys}{a dataset of keys for the rows, identifiers for the columns
    in \code{x} to use as keys, or \code{NULL}.}

\item{\dots}{further arguments passed to or from other methods.}
}
\details{
\code{framed} is a generic method that converts a data object like a vector or
a \code{data.frame} to a \code{dataset} with keys identifying the rows. One
can specify the keys using the \code{keys} argument:
\itemize{
    \item if \code{NULL}, then the keys are taken from the original object
    (\code{keys(x)} if non-\code{NULL}, or \code{rownames(x)} otherwise);

    \item if a character vector of column names in \code{x} or an integer
    vector of column indices in \code{x}, then these columns get removed
    from \code{x} in the converted object and used as key columns for
    the result;

    \item if a matrix-like object, then this object is converted to a
    \code{dataset} and used as the keys for the result.
  }
}
\value{
A dataset object with keys as specified by the \code{keys} argument.
}
\seealso{
\code{\link{dataset}}, \code{\link{keys}}, \code{\link{grouped}}.
}
\examples{
# use row names as keys
framed(mtcars)

# use an existing column for keys
x <- data.frame(col1 = letters[5:10], col2 = rnorm(6), col3 = runif(6))
framed(x, key = "col1")

# use a different set of keys
framed(mtcars, keys = dataset(key = 2 * (1:nrow(mtcars))))
}
\keyword{classes}
