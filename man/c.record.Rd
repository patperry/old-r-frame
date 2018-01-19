\name{c.record}
\alias{c.record}
\title{Combining Records}
\description{
Take a sequence of records and combine them into a single object
by concatenating entries.
}
\usage{
\S3method{c}{record}(...)
}
\arguments{
\item{...}{a sequence of objects to concatenate as records.}
}
\details{
\code{c.record} coerces each of its non-\code{NULL} arguments to a
record via \code{as_record} and then concatenates their entries together
into a single record. If arguments are named, then each field name gets
prefixed by the named argument and a period (\code{"."}).
}
\section{Dispatch}{
The generic method \code{c} dispatches according to the type of its first
argument. We explicitly export \code{c.record} so that you can call this
function if you want to concatenate with a non-record as the first argument.
}
\value{
A record object gotten by concatenating the arguments.
}
\seealso{
\code{\link{c}}, \code{\link{cbind.dataset}}.
}
\examples{

# convert each argument to a record, then combine
c.record(a = 1, b = 10, c = list(x = 2, y = 3))

# NULL is ignored
c.record(list(x = "foo"), NULL, "bar")
}
