\name{dataset}
\alias{as_dataset}
\alias{as_dataset.data.frame}
\alias{as_dataset.default}
\alias{as_dataset.dataset}
\alias{dataset}
\alias{is_dataset}
\title{Data Set Objects}
\description{
Create or test for dataset objects.
}
\usage{
dataset(...)

as_dataset(x, ...)

is_dataset(x)
}
\arguments{
\item{\dots}{data frame columns for \code{dataset};
    further arguments passed to or from other methods for \code{as_dataset}.}

\item{x}{object to be coerced or tested.}
}
\details{
These functions create or convert another object to a dataset object.
A dataset object is a data frame with special functions for printing.

\code{dataset} has similar semantics to the \code{\link{data.frame}}
function, except that string columns do not get converted to factors.
    
\code{as_dataset} converts another object to a dataset object. By default,
the method converts \code{x} to a \code{data.frame} and then sets
the class attribute of the result to \code{c("dataset", "data.frame")}.

\code{is_dataset} tests whether \code{x} is a valid dataset object.
    
\code{as_dataset} is generic: you can write methods to handle specific
classes of objects.
}
\value{
    \code{dataset} creates a dataset object with its
        class attribute set to \code{c("dataset", "data.frame")}.

    \code{as_dataset} attempts to coerce its argument to a object.

    \code{is_dataset} returns \code{TRUE} or \code{FALSE} depending on
        whether its argument is a valid dataset object or not.
}
\seealso{
\code{\link{frame-package}}, \code{\link{print.dataset}}.
}
\examples{
# convert a data frame:
emoji <- data.frame(text = sapply(0x1f600 + 1:30, intToUtf8),
                    stringsAsFactors = FALSE)
as_dataset(emoji)

# construct directly (no need for stringsAsFactors = FALSE):
dataset(text = sapply(0x1f600 + 1:30, intToUtf8))
    
# convert a character vector:
as_dataset(c(a = "goodnight", b = "moon")) # keeps names
as_dataset(c(a = "goodnight", b = "moon"), row.names = NULL) # drops names
}
\keyword{classes}
