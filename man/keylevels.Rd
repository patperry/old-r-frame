\name{keylevels}
\alias{keylevels}
\alias{keylevels.default}
\alias{keylevels.dataset}
\title{Data Key Levels}
\description{
Get the set of unique key levels in a data set.
}
\usage{
keylevels(x)
}
\arguments{
  \item{x}{an object.}
}
\details{
Get the unique values for each component in a data set's keys.
}
\value{
If \code{keys(x)} is non-\code{NULL}, then
\code{keylevels(x)} returns a list the same names and length as
\code{keys(x)}; each column is a vector of the unique values in the
corresponding key component.
}
\seealso{
\code{\link{keys}}.
}
\examples{
# for single-component keys, return all keys, in a list
x <- framed(mtcars)
keylevels(x)

# for mul-component keys, return the unique values in each component
keys(x) <- dataset(k1 = rep(LETTERS[1:4], each = 8),
                   k2 = rep(1:8, 4))
keylevels(x)

# if keys are absent, return NULL
keys(x) <- NULL
keylevels(x)
}
