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

\S3method{rbind}{dataset}(..., check = TRUE, deparse.level = 1)
}
\arguments{
\item{...}{a sequence of dataset or atomic objects.}

\item{check}{whether to check argument dimensions and names for
    compatibility.}

\item{deparse.level}{ignored.}
}
\details{
\code{rbind} coerces each of its non-\code{NULL} arguments to a dataset via
\code{as_dataset} and then concatenates their rows together. Each converted
argument must have the same names and length as the first (or first
non-\code{NULL} argument if the latter is \code{NULL}, or their names
must be \code{NULL}. Specify \code{check = FALSE} to skip the checking on
the argument names and lengths.

\code{cbind} coerces each of its non-\code{NULL} arguments via
\code{as_dataset}, drops \code{NULL} values, and then concatenates
their columns together. Each converted argument must have the same number of
rows. Their keys must also agree with the first argument, or be \code{NULL}.
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
the result will have \code{names} taken from its first argument, but no
other attributes (in particular, no keys). For \code{cbind}, the result
will have the same keys as its first arguments, and names gotten by
concatenating its arguments' names together.
}
\seealso{
\code{\link{as_dataset}}, \code{\link{cbind}}.
}
\examples{
# fill in
}
