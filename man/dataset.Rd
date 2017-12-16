\name{dataset}
\alias{dataset}
\alias{is_dataset}
\title{Data Set Objects}
\description{
Create or test for data set objects.
}
\usage{
dataset(...)

is_dataset(x)
}
\arguments{
\item{\dots}{data set columns for \code{dataset}}

\item{x}{object to be tested.}
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
    
\code{is_dataset} tests whether \code{x} is a valid dataset object.
}
\value{
\code{dataset} returns a dataset object.

\code{is_dataset} returns \code{TRUE} or \code{FALSE} depending on
    whether its argument is a valid dataset object or not.
}
\seealso{
\code{\link{framed}}, \code{\link{keys}}, \code{\link{print.dataset}}.
}
\examples{
# no need for stringsAsFactors = TRUE
code <- as.hexmode(0x1f600 + 1:10)
(emoji <- dataset(code, text = sapply(code, intToUtf8)))

# matrix-like columns are allowed
(ds <- dataset(vector = rnorm(10),
               emoji,
               matrix = matrix(runif(20), 10, 2)))

# set multi-attribute keys for the rows
keys(ds) <- dataset(k1 = rep(c("a", "b"), each = 5),
                    k2 = rep(1:5, times = 2))
print(ds)
}
\keyword{classes}
