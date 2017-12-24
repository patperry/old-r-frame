\name{dataset}
\alias{dataset}
\alias{as_dataset}
\alias{as_dataset.data.frame}
\alias{as_dataset.dataset}
\alias{as_dataset.default}
\alias{as_dataset.list}
\alias{is_dataset}
\alias{is_simple_dataset}
\title{Data Set Objects}
\description{
Create or test for data set objects.
}
\usage{
dataset(...)

as_dataset(x, ..., simple = FALSE)

is_dataset(x)

is_simple_dataset(x)
}
\arguments{
\item{\dots}{columns for \code{dataset}; further arguments passed to or
    from other methods for \code{as_dataset}.}

\item{x}{object to be converted or tested.}

\item{simple}{whether to convert vector columns to simple types.}
}
\details{
These functions are used to create or test for a \code{dataset} object,
a type like a data frame that allows for arbitrary matrix-like columns
(including nested datasets), allows specifying multi-attribute keys
for the rows via the \code{\link{keys<-}} accessor, and has better
printing behavior.

The \code{dataset} function takes a set of columns as arguments and creates a
data set object with these columns as variables. The function has similar
semantics to the \code{\link{data.frame}} function, except that matrix-like
columns do not get split into vectors and string columns do not get
converted to factors.

\code{as_dataset} converts \code{x} to a dataset object. This is a generic
function with a default implementation that converts the result of
\code{as.data.frame(x)} to a dataset. If \code{simple = TRUE}, then all
vector columns get converted to simple types; see \sQuote{Simple Datasets}
for details.
    
\code{is_dataset} tests whether \code{x} is a valid dataset object.

\code{is_simple_dataset} tests whether \code{x} is a valid simple dataset
object.
}
\section{Simple Datasets}{
A simple dataset is one with fewer than 2^31 columns, each of which is of
a simple type.  The following types are considered \dQuote{simple}:
\itemize{
  \item \code{logical};
  \item \code{integer};
  \item \code{numeric} (\code{double});
  \item \code{complex};
  \item \code{character}, in any encoding, provided the values can be
        converted to valid UTF-8 via \code{utf8::as_utf8()};
}
Note that \code{factor} types are not considered simple.

Simple types are the only ones allowed for key sets, and the only types
for which \code{unique()}, \code{duplicate()}, and \code{anyDuplicated()} are
supported.

You can coerce a dataset \code{x} to a simple dataset by calling 
\code{as_dataset(x, simple = TRUE)}. This converts each column to a simple
type by calling \code{as.logical()}, \code{as.numeric()}, \code{as.complex()},
or \code{as.character()}. This converts factor columns to character.

Currently, \code{Date} and \code{POSIXct} columns also get converted to
character. At some point, these may get added as simple types.
}
\value{
\code{dataset} and \code{as_dataset} return a dataset objects.

\code{is_dataset} and \code{is_simple_dataset} return \code{TRUE} or
\code{FALSE} depending on whether their arguments are valid dataset objects
or simple dataset objects, respectively.
}
\seealso{
\code{\link{framed}}, \code{\link{keys}}, \code{\link{schema}}.
}
\examples{
# no need for stringsAsFactors = TRUE
code <- as.hexmode(0x1f600 + 1:10)
(emoji <- dataset(code, text = sapply(code, intToUtf8)))

# matrix-like columns are allowed
(ds <- dataset(vector = rnorm(10),
               emoji,
               matrix = matrix(runif(20), 10, 2)))

# convert a data.frame
as_dataset(mtcars)

# test for datasets
is_dataset(mtcars)
is_dataset(ds)
}
\keyword{classes}
