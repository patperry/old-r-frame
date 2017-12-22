\name{unique.dataset}
\alias{unique.dataset}
\alias{unique.keyset}
\title{Distinct Data Rows}
\description{
Extract the set of distinct rows in a dataset.
}
\usage{
\S3method{unique}{dataset}(x, incomparables = FALSE, ..., sorted = FALSE)
\S3method{unique}{keyset}(x, incomparables = FALSE, ..., sorted = FALSE)
}
\arguments{
\item{x}{a dataset object with vector rows.}

\item{incomparables}{ignored.}

\item{...}{further arguments passed to or from other methods.}

\item{sorted}{logical indicating whether to put rows in sorted order,
    ordered sequentially by column in descending order.}
}
\details{
Given a dataset x with vector columns, convert each column to an atomic
type and then extract the set of unique rows, as a \code{keyset} object.
If \code{sorted = TRUE}, then order the rows in descending order sequentially
according to column.
}
\value{
A \code{keyset} with rows corresponding to the unique rows in \code{x},
in sorted order.
}
\seealso{
\code{\link{keyset}}, \code{\link{duplicated}}, \code{\link{unique}}.
}
\examples{
# fill in
}
