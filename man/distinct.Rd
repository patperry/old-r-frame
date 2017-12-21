\name{distinct}
\alias{distinct}
\alias{distinct.dataset}
\alias{distinct.default}
\alias{distinct.keyset}
\title{Distinct Data Rows}
\description{
Extract the set of distinct rows in a dataset.
}
\usage{
distinct(x, ...)
}
\arguments{
\item{x}{a dataset object with vector rows.}

\item{...}{further arguments passed to or from other method.}
}
\details{
Given a dataset x with vector columns, convert each column to an atomic
type and then extract the set of unique rows, as a \code{keyset} object.

(fill in)
}
\value{
A \code{keyset} with rows corresponding to the unique rows in \code{x},
in sorted order.
}
\seealso{
\code{\link{keyset}}, \code{\link{duplicated}}.
}
\examples{
# fill in
}
