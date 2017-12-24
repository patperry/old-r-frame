\name{unique.dataset}
\alias{anyDuplicated.dataset}
\alias{anyDuplicated.keyset}
\alias{duplicated.dataset}
\alias{duplicated.keyset}
\alias{unique.dataset}
\alias{unique.keyset}
\title{Distinct Data Rows}
\description{
Extract the set of distinct rows in a dataset.
}
\usage{
\S3method{unique}{dataset}(x, incomparables = FALSE, ..., sorted = FALSE)
\S3method{duplicated}{dataset}(x, incomparables = FALSE, fromLast = FALSE, ...)
\S3method{anyDuplicated}{dataset}(x, incomparables = FALSE, fromLast = FALSE, ...)

\S3method{unique}{keyset}(x, incomparables = FALSE, ..., sorted = FALSE)
\S3method{duplicated}{keyset}(x, incomparables = FALSE, fromLast = FALSE, ...)
\S3method{anyDuplicated}{keyset}(x, incomparables = FALSE, fromLast = FALSE, ...)
}
\arguments{
\item{x}{a simple dataset object.}

\item{incomparables}{ignored.}

\item{fromLast}{logical indicating whether to scan from last to first
    rather than first to last row.}

\item{...}{further arguments passed to or from other methods.}

\item{sorted}{logical indicating whether to put rows in sorted order,
    ordered sequentially by column in descending order.}
}
\details{
\code{unique} takes a simple dataset \code{x} extract the set of unique rows,
as a \code{keyset} object.  If \code{sorted = TRUE}, then the resulting rows
are ordered in descending order sequentially according to column.

\code{duplicated} finds which rows in \code{x} are duplicates of others;
\code{anyDuplicated} reports if any such rows exits.
}
\value{
\code{unique} returns a \code{keyset} with rows corresponding to the unique
rows in \code{x}, in sorted order.

\code{duplicated} returns a logical vector with length equal to the number of
rows in \code{x} and each entry indicating whether or not the corresponding
row in \code{x} is a duplicate of another.

\code{anyDuplicated} returns the index of the first duplicated row, or
\code{0} if all are unique.
}
\seealso{
\code{\link{is_simple_dataset}}, \code{\link{keyset}},
\code{\link{duplicated}}, \code{\link{unique}}.
}
\examples{
# the instances for 'dataset' objects fix bugs in the 'data.frame' versions:
a <- c(.15, .1 + .05)
x <- data.frame(a, b  = "b")
unique(x) # wrong; .15 != .1 + .05 in double-precision arithmetic
unique(as_dataset(x)) # correct
}
