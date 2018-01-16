\name{cbind.dataset}
\alias{cbind.dataset}
\alias{rbind.dataset}
\title{Combining Data Sets by Rows or Columns}
\description{
Take a sequence of datasets and combine them into a single object
by concatenating rows or columns.
}
\usage{
\S3method{cbind}{dataset}(..., deparse.level = 1)

\S3method{rbind}{dataset}(..., deparse.level = 1)
}
\arguments{
\item{...}{a sequence of dataset or atomic objects.}

\item{deparse.level}{ignored.}
}
\details{
\code{rbind.dataset} coerces each of its non-\code{NULL} arguments to a
dataset via \code{as_dataset} and then concatenates their rows together. Each
converted argument must have the same number of columns, and the column names
must match or be \code{NULL}. Further, each must have the same number of key
columns, and the key column names must match or be \code{NULL}.

\code{cbind.dataset} coerces each of its non-\code{NULL} arguments via
\code{as_dataset}, drops \code{NULL} values, and then concatenates
their columns together. Each converted argument must have the same number of
rows. Their keys must also agree, or be \code{NULL}.
}
\section{Dispatch}{
\code{cbind} and \code{rbind} use non-standard argument dispatch. A call to
\code{cbind} dispatches to \code{cbind.dataset} only when all arguments are
either atomic or inherit from \code{"dataset"}. If any argument is a plain
\code{data.frame} or has another class, then \code{base::cbind} gets called
instead. To avoid this, either convert each argument via \code{as_dataset}
before calling \code{cbind}, or else call \code{cbind.dataset} directly.
The same applies to \code{rbind}; use \code{rbind.dataset} for controlled
dispatch.
}
\value{
A dataset object gotten by concatenating the arguments. For \code{rbind},
we also concatenate key rows, and if this introduces duplicates, to
disambiguate the keys we append a key column called \code{"#"} with the
key copy number.  For \code{cbind}, the result will have the same keys
as its arguments, and names gotten by concatenating its arguments' names
together.
}
\seealso{
\code{\link{as_dataset}}, \code{\link{cbind}}.
}
\examples{
# concatenate columns
cbind.dataset(mtcars[1:4,1:3], y = rnorm(4))

# concatenate rows; disambiguate the result keys by adding a column
rbind.dataset(mtcars[1:4,], mtcars[1:4,])
}
